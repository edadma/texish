package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.path.Path

/** `\usfm{path}` — typeset a book of Scripture written in USFM (Unified Standard Format Markers, the format
  * Bible translations are exchanged in). The named file is read, its markers are translated into calls on the
  * `\usfm…` macros of the `usfm` package, and the result is typeset in place. Because every visible decision is
  * a macro — the chapter figure, the verse number, the poetry indents, the section headings, the footnotes, the
  * words of Jesus — a document sets the appearance entirely with ordinary texish commands before the call:
  *
  * {{{
  * \use{usfm}
  * \def usfmwj t {\textcolor{#111111}{\t}}   // words of Jesus in plain black rather than red
  * \set usfmqbase {24}                        // deeper poetry indent
  * \usfm{john.usfm}
  * }}}
  *
  * The path resolves against the directory of the document being processed (as `\include` and `\use` do), so a
  * script reads a Scripture file sitting beside it regardless of where the tool was launched.
  */
object UsfmPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val pathTokens = proc.readArgument(pos)
    val rel = pathTokens.map {
      case Token.Text(s, _)  => s
      case Token.Space(s, _) => s
      case _                 => ""
    }.mkString.trim

    if rel.isEmpty then proc.handler.error("\\usfm requires a file path", pos)

    // Prefer a file beside the current document; fall back to the path as given (absolute paths and
    // CWD-relative invocations keep working), tolerating a host with no working filesystem.
    val resolved =
      try
        val beside = Path(proc.currentDir) / rel
        if beside.exists && beside.isFile then beside.toPlatformString else rel
      catch case _: Throwable => rel

    try
      val text = Path(resolved).readText()
      proc.processContent(Usfm.translate(text))
    catch
      case e: ParserException => throw e
      case e: Exception       => proc.handler.error(s"\\usfm: cannot read '$rel': ${e.getMessage}", pos)

/** Translator from USFM marker source into texish source.
  *
  * USFM is a flat stream of backslash markers over running text. A marker is `\name` (letters, then an optional
  * level digit); a matching `\name*` closes a character span. The translator classifies each marker and rewrites
  * it as a call on a `\usfm…` macro, so that all styling lives in the `usfm` package rather than here:
  *
  *   - Metadata (`\id`, `\usfm`, `\ide`, `\rem`, `\sts`) is dropped along with its line.
  *   - A number marker (`\c`, `\v`) becomes `\usfmc{N}` / `\usfmv{N}`, the number split from the text that follows.
  *   - A title or heading (`\h`, `\toc`, `\mt`, `\ms`, `\mr`, `\s`, `\r`, `\d`, …) captures its content — the rest
  *     of the block, character spans and all — as a braced argument: `\usfms{1}{The Beginning}`.
  *   - A body paragraph or poetry line (`\p`, `\m`, `\q`, `\li`, `\qr`, …) becomes `\usfmp{level}` / `\usfmq{level}`
  *     and the running text flows after it, so embedded `\v`, `\wj` and footnotes stay inline.
  *   - A character span (`\wj…\wj*`, `\ref…\ref*`) becomes `\usfmwj{…}` / `\usfmref{display}{target}`.
  *   - A footnote (`\f …\f*`) becomes `\usfmfootnote{…}`, its inner run-on markers (`\fr`, `\ft`, `\fqa`, …) each a
  *     nested `\usfmfr{…}` / `\usfmft{…}` / `\usfmfqa{…}` segment.
  *
  * The three characters that are special to texish but ordinary in Scripture — the braces and the tilde — plus a
  * literal backslash, dollar and a doubled slash are escaped in every text run, so a verse can contain them safely.
  */
object Usfm:
  private enum Tok:
    case Marker(name: String, closing: Boolean)
    case Txt(s: String)

  import Tok.*

  // Metadata markers whose line is discarded entirely.
  private val ignore = Set("id", "usfm", "ide", "rem", "sts", "toca")

  // Titles and headings: the content up to the next block marker is captured as a braced argument.
  private val headingBases = Set("h", "toc", "mt", "mte", "ms", "mr", "s", "sr", "r", "d", "sp", "qa", "cl", "cp")

  // Body paragraphs and poetry lines: the running text flows after the macro.
  private val bodyBases =
    Set("p", "m", "pc", "pmo", "pi", "pr", "ph", "pm", "pmc", "pmr", "po", "mi", "nb", "cls",
      "q", "qr", "qc", "qm", "qd", "li", "lim", "b")

  // Inline character spans that close with \name*. Each has a matching \usfm… macro in the usfm package; a
  // character marker outside this set drops its styling but keeps the text it wrapped.
  private val charBases =
    Set("wj", "nd", "add", "bk", "tl", "qt", "qs", "sc", "em", "it", "bd", "bdit", "sup", "k", "rq", "w", "fv")

  // Footnote / cross-reference inner markers that run on until the next inner marker or the note's close.
  private val noteInner =
    Set("fr", "ft", "fq", "fqa", "fqb", "fk", "fl", "fp", "fw", "fdc", "fm", "xo", "xt", "xq", "xk")

  // A heading's captured content ends at the next block-level marker (a new heading, a body paragraph, or a
  // chapter). A verse marker does not end it — a psalm's descriptive title carries its own \v.
  private def endsCapture(base: String): Boolean =
    base == "c" || headingBases.contains(base) || bodyBases.contains(base)

  /** Split a marker name into its letters and trailing level digit (`q2` → `("q", 2)`, `s` → `("s", 1)`). */
  private def splitLevel(name: String): (String, Int) =
    val letters = name.takeWhile(_.isLetter)
    val digits  = name.drop(letters.length)
    val level   = if digits.nonEmpty then digits.toIntOption.getOrElse(1) else 1
    (letters, level)

  private def tokenize(src: String): Vector[Tok] =
    val out = Vector.newBuilder[Tok]
    val n   = src.length
    var i   = 0
    val sb  = new StringBuilder

    def flush(): Unit =
      if sb.nonEmpty then { out += Txt(sb.toString); sb.setLength(0) }

    while i < n do
      val c = src.charAt(i)
      if c == '\\' then
        flush()
        i += 1
        val name = new StringBuilder
        while i < n && (src.charAt(i).isLetter || src.charAt(i).isDigit) do
          name += src.charAt(i)
          i += 1
        var closing = false
        if i < n && src.charAt(i) == '*' then
          closing = true
          i += 1
        // A single space after an opening marker is the marker's delimiter, not content, and is dropped. After a
        // closing marker (\wj*) a following space is ordinary content and is kept.
        if !closing && i < n && src.charAt(i) == ' ' then i += 1
        out += Marker(name.toString, closing)
      else
        // Newlines and tabs collapse to spaces so a line break between markers never reads as a paragraph break —
        // paragraph structure comes only from the markers the translator emits.
        sb += (if c == '\n' || c == '\r' || c == '\t' then ' ' else c)
        i += 1

    flush()
    out.result()

  private def escape(s: String): String =
    val b = new StringBuilder
    var k = 0
    while k < s.length do
      val c = s.charAt(k)
      c match
        case '\\' => b.append("\\\\")
        case '{'  => b.append("\\{")
        case '}'  => b.append("\\}")
        case '~'  => b.append("\\~")
        case '$'  => b.append("\\$")
        // A doubled slash would start a texish comment; the empty group between the slashes breaks the pair
        // without changing what is set.
        case '/' if k + 1 < s.length && s.charAt(k + 1) == '/' => b.append("/{}")
        case _ => b.append(c)
      k += 1
    b.toString

  /** Translate a USFM document into texish source. */
  def translate(src: String): String =
    new Walker(tokenize(src)).run()

  private class Walker(toks: Vector[Tok]):
    private val buf = toks.toBuffer
    private val sb  = new StringBuilder
    private var i   = 0

    def run(): String =
      while i < buf.length do step()
      sb.toString

    // The number that opens a \c or \v: the first whitespace-delimited word of the following text run, with the
    // remainder of that run left in place to flow on as ordinary content.
    private def leadingWord(): String =
      if i >= buf.length then return ""
      buf(i) match
        case Txt(s) =>
          val t  = s.dropWhile(_ == ' ')
          val sp = t.indexWhere(_ == ' ')
          if sp < 0 then
            i += 1
            t
          else
            buf(i) = Txt(t.substring(sp + 1))
            t.substring(0, sp)
        case _ => ""

    private def step(): Unit =
      buf(i) match
        case Txt(s) =>
          sb.append(escape(s))
          i += 1
        case Marker(name, true) =>
          i += 1 // a stray closing marker with no open span — drop it
        case Marker(name, false) =>
          val (base, level) = splitLevel(name)
          if base.isEmpty || ignore(base) then
            i += 1
            if i < buf.length && buf(i).isInstanceOf[Txt] then i += 1
          else if base == "c" || base == "v" then
            i += 1
            sb.append(s"\\usfm$base{${escape(leadingWord())}}")
          else if headingBases(base) then
            i += 1
            sb.append(s"\\usfm$base{$level}{")
            captureHeading()
            sb.append("}")
          else if base == "f" || base == "fe" then
            i += 1
            emitNote("usfmfootnote", Set("f", "fe"))
          else if base == "x" then
            i += 1
            emitNote("usfmxref", Set("x"))
          else if base == "ref" then
            i += 1
            emitRef()
          else if base == "fv" || charBases(base) then
            i += 1
            sb.append(s"\\usfm$base{")
            captureSpan(name)
            sb.append("}")
          else if bodyBases(base) then
            i += 1
            if base == "b" then sb.append("\\usfmb")
            else sb.append(s"\\usfm$base{$level}")
          else
            // An unrecognised marker loses its own effect but keeps the text it introduced, degrading rather than
            // swallowing content.
            i += 1

    // Collect a heading's content until the next block-level marker, translating inline spans, footnotes and any
    // embedded verse number, but leaving the block marker for the main loop.
    private def captureHeading(): Unit =
      var done = false
      while !done && i < buf.length do
        buf(i) match
          case Txt(s) =>
            sb.append(escape(s))
            i += 1
          case Marker(nm, true) =>
            i += 1
          case Marker(nm, false) =>
            val (b, _) = splitLevel(nm)
            if endsCapture(b) then done = true
            else if b == "v" then
              i += 1
              sb.append(s"\\usfmv{${escape(leadingWord())}}")
            else if b == "f" || b == "fe" then
              i += 1
              emitNote("usfmfootnote", Set("f", "fe"))
            else if b == "x" then
              i += 1
              emitNote("usfmxref", Set("x"))
            else if b == "ref" then
              i += 1
              emitRef()
            else if b == "fv" || charBases(b) then
              i += 1
              sb.append(s"\\usfm$b{")
              captureSpan(nm)
              sb.append("}")
            else i += 1

    // Collect a character span up to its matching \name*, translating nested spans and footnotes inside it.
    private def captureSpan(name: String): Unit =
      val (spanBase, _) = splitLevel(name)
      var done          = false
      while !done && i < buf.length do
        buf(i) match
          case Marker(nm, true) if nm == name =>
            i += 1
            done = true
          case Txt(s) =>
            sb.append(escape(s))
            i += 1
          case Marker(nm, false) =>
            val (b, _) = splitLevel(nm)
            if b == "f" || b == "fe" then
              i += 1
              emitNote("usfmfootnote", Set("f", "fe"))
            else if b == "ref" then
              i += 1
              emitRef()
            else if b == "v" || b == "c" then
              // A verse or chapter number inside a character span — the BSB writes \wj \v 39 …\wj*, opening the
              // words-of-Jesus span before the verse. The number is editorial, not part of the span's styling, so
              // suspend the span, set the number outside it, and resume: the number stays black even though the
              // words around it are red.
              i += 1
              sb.append("}")
              sb.append(s"\\usfm$b{${escape(leadingWord())}}")
              sb.append(s"\\usfm$spanBase{")
            else if charBases(b) then
              i += 1
              sb.append(s"\\usfm$b{")
              captureSpan(nm)
              sb.append("}")
            else i += 1
          case Marker(_, true) =>
            i += 1

    // A \ref…\ref* cross-reference link: display text before the pipe, canonical target after it.
    private def emitRef(): Unit =
      val content = new StringBuilder
      var done    = false
      while !done && i < buf.length do
        buf(i) match
          case Marker("ref", true) =>
            i += 1
            done = true
          case Txt(s) =>
            content.append(s)
            i += 1
          case _ =>
            i += 1
      val raw     = content.toString
      val bar     = raw.indexOf('|')
      val display = if bar < 0 then raw else raw.substring(0, bar)
      val target  = if bar < 0 then raw else raw.substring(bar + 1)
      sb.append(s"\\usfmref{${escape(display.trim)}}{${escape(target.trim)}}")

    // A footnote or cross-reference note. The caller mark (the `+`, `-` or `?` after the opening marker) is
    // discarded; the body is a run of inner segments, each closed implicitly by the next inner marker or the
    // note's own close.
    private def emitNote(wrapper: String, closeBases: Set[String]): Unit =
      sb.append(s"\\$wrapper{")
      if i < buf.length && buf(i).isInstanceOf[Txt] then i += 1 // drop the caller token
      var done = false
      while !done && i < buf.length do
        buf(i) match
          case Marker(nm, true) if closeBases(splitLevel(nm)._1) =>
            i += 1
            done = true
          case Marker(nm, false) =>
            val (b, _) = splitLevel(nm)
            if closeBases(b) then done = true
            else if b == "ref" then
              i += 1
              emitRef()
            else if b == "fv" || charBases(b) then
              i += 1
              sb.append(s"\\usfm$b{")
              captureSpan(nm)
              sb.append("}")
            else if noteInner(b) then
              i += 1
              sb.append(s"\\usfm$b{")
              captureNoteSegment(closeBases)
              sb.append("}")
            else i += 1
          case Txt(s) =>
            sb.append(escape(s))
            i += 1
          case Marker(_, true) =>
            i += 1
      sb.append("}")

    // One run-on segment of a note (\ft, \fqa, …): text and inline cross-references up to the next inner marker or
    // the note's close, neither of which is consumed here.
    private def captureNoteSegment(closeBases: Set[String]): Unit =
      var done = false
      while !done && i < buf.length do
        buf(i) match
          case Marker(nm, cl) =>
            val (b, _) = splitLevel(nm)
            if cl && closeBases(b) then done = true
            else if !cl && b == "ref" then
              i += 1
              emitRef()
            else if !cl && (b == "fv" || charBases(b)) then
              i += 1
              sb.append(s"\\usfm$b{")
              captureSpan(nm)
              sb.append("}")
            else if noteInner(b) || closeBases(b) then done = true
            else i += 1
          case Txt(s) =>
            sb.append(escape(s))
            i += 1
