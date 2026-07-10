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

    // A document sets `usfmintro` to 0 to drop the book introduction and set only the Scripture text.
    val skipIntro = proc.handler match
      case h: TypesetterHandler if h.typesetter.get("usfmintro").isDefined =>
        h.typesetter.getNumber("usfmintro") == 0.0
      case _ => false

    try
      val text = Path(resolved).readText()
      proc.processContent(Usfm.translate(text, skipIntro))
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

  // Metadata markers whose line is discarded entirely — book identification and the intro-end sentinel.
  private val ignore = Set("id", "usfm", "ide", "rem", "sts", "toca", "ie")

  // Titles and headings: the content up to the next block marker is captured as a braced argument. This covers
  // the book titles (\mt…), major-section headings (\ms, \mr), section headings (\s, \sr, \r, \d, \sp, …) and the
  // book introduction's own titles and outline (\imt, \is, \iot, \io — the outline entries carry \ior refs).
  private val headingBases =
    Set("h", "toc", "mt", "mte", "ms", "mr", "s", "sr", "r", "d", "sp", "qa", "cl", "cp",
      "imt", "imte", "is", "iot", "io")

  // Body paragraphs and poetry lines: the running text flows after the macro. Scripture paragraphs (\p, \m, \q…),
  // lists (\li, \lim, \lh, \lf), and the introduction's prose and poetry (\ip, \im, \iq, \ili, …). \b and \ib are
  // blank-line breaks with no level, handled specially where they are emitted.
  private val bodyBases =
    Set("p", "m", "pc", "pmo", "pi", "pr", "ph", "pm", "pmc", "pmr", "po", "mi", "nb", "cls",
      "q", "qr", "qc", "qm", "qd", "li", "lim", "lh", "lf", "b",
      "ip", "ipi", "ipq", "ipr", "im", "imi", "imq", "iq", "ili", "iex", "ib")

  // Inline character spans that close with \name*. Each has a matching \usfm… macro in the usfm package; a
  // character marker outside this set drops its styling but keeps the text it wrapped.
  private val charBases =
    Set("wj", "nd", "add", "bk", "tl", "qt", "qs", "sc", "em", "it", "bd", "bdit", "sup", "k", "rq", "w", "fv",
      "ior", "iqt", "no", "dc", "pn", "png", "ord", "sig", "sls", "rb", "pro", "wg", "wh", "wa", "qac", "lit")

  // Footnote / cross-reference inner markers that run on until the next inner marker or the note's close.
  private val noteInner =
    Set("fr", "ft", "fq", "fqa", "fqb", "fk", "fl", "fp", "fw", "fdc", "fm", "xo", "xt", "xq", "xk", "xot", "xnt", "xta")

  // Table markers: a row opener (\tr) and the cell openers (\tc / \th, with right-aligned \tcr / \thr variants).
  private def isTableCell(base: String): Boolean = base == "tc" || base == "tcr" || base == "th" || base == "thr"
  private def isTableHeader(base: String): Boolean = base == "th" || base == "thr"

  // The book introduction's markers. A document that only wants Scripture (a pocket portion, say) can ask the
  // reader to drop the whole introduction — from its first marker to the first chapter — rather than edit the file.
  private val introBases =
    Set("imt", "imte", "is", "iot", "io", "ior", "iqt", "ip", "ipi", "ipq", "ipr", "im", "imi", "imq", "iq",
      "ili", "iex", "ib")

  // A heading's captured content ends at the next block-level marker (a new heading, a body paragraph, or a
  // chapter). A verse marker does not end it — a psalm's descriptive title carries its own \v.
  private def endsCapture(base: String): Boolean =
    base == "c" || base == "tr" || headingBases.contains(base) || bodyBases.contains(base)

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
        // USFM nests a character marker inside another by prefixing a plus (`\+w …\+w*` inside `\wj`). The nesting
        // is transparent to translation — `\+w` behaves exactly like `\w` — so drop the plus and read the name.
        if i < n && src.charAt(i) == '+' then i += 1
        val name = new StringBuilder
        while i < n && (src.charAt(i).isLetter || src.charAt(i).isDigit) do
          name += src.charAt(i)
          i += 1
        var closing = false
        if i < n && src.charAt(i) == '*' then
          closing = true
          i += 1
        // A single whitespace after an opening marker is the marker's delimiter, not content, and is dropped —
        // a newline counts, so a marker alone on its line (`\p` then `\v 1 …` on the next, as many translations
        // write it) does not leave a leading space that indents the paragraph's first line. After a closing marker
        // (\wj*) a following space is ordinary content and is kept.
        if !closing && i < n && (src.charAt(i) == ' ' || src.charAt(i) == '\n' || src.charAt(i) == '\r' || src.charAt(i) == '\t') then i += 1
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

  /** Translate a USFM document into texish source. With `skipIntro`, the book introduction (everything from its
    * first marker to the first chapter) is dropped, so only the Scripture text is set. */
  def translate(src: String, skipIntro: Boolean = false): String =
    new Walker(tokenize(src), skipIntro).run()

  private class Walker(toks: Vector[Tok], skipIntro: Boolean):
    private val buf = toks.toBuffer
    private var sb  = new StringBuilder
    private var i   = 0

    // Run `body` with output diverted to a fresh buffer and return what it produced, restoring the previous
    // buffer after. Used to translate a table cell's content into a string before it is placed in the row.
    private def capture(body: => Unit): String =
      val saved = sb
      sb = new StringBuilder
      body
      val out = sb.toString
      sb = saved
      out

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
          if skipIntro && introBases(base) then
            // Drop the whole introduction: skip to the first chapter, taking its tables and prose with it.
            while i < buf.length && !(buf(i) match { case Marker(nm, false) => splitLevel(nm)._1 == "c"; case _ => false }) do
              i += 1
          else if base.isEmpty || ignore(base) then
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
          else if base == "tr" then
            captureTable()
          else if base == "fv" || charBases(base) then
            i += 1
            sb.append(s"\\usfm$base{")
            captureSpan(name)
            sb.append("}")
          else if bodyBases(base) then
            i += 1
            // \b (stanza break) and \ib (intro blank line) are level-less breaks; the rest carry their level.
            if base == "b" || base == "ib" then sb.append(s"\\usfm$base")
            else sb.append(s"\\usfm$base{$level}")
          else
            // An unrecognised marker loses its own effect but keeps the text it introduced, degrading rather than
            // swallowing content.
            i += 1

    // A USFM table: a run of \tr rows, each a series of \tc / \th cells (right-aligned \tcr / \thr variants). The
    // whole run is gathered so the column count is known, then emitted as one \usfmtable{colspec}{body} — the
    // colspec is one letter per column, the body the cells joined with & and rows with \\, so it lowers to the
    // engine's \tabular. Header cells become \usfmth, ordinary cells \usfmtc, both redefinable in the package.
    private def captureTable(): Unit =
      val rows = collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[(Boolean, String)]]()
      while i < buf.length && (buf(i) match { case Marker(nm, false) => splitLevel(nm)._1 == "tr"; case _ => false }) do
        i += 1 // consume the \tr
        // A newline between \tr and its first cell tokenises as a whitespace text run; skip it so the cell loop
        // does not stop before reading any cell.
        while i < buf.length && (buf(i) match { case Txt(s) => s.trim.isEmpty; case _ => false }) do i += 1
        val cells = collection.mutable.ArrayBuffer[(Boolean, String)]()
        var more  = true
        while more && i < buf.length do
          buf(i) match
            case Marker(nm, false) if isTableCell(splitLevel(nm)._1) =>
              val header = isTableHeader(splitLevel(nm)._1)
              i += 1
              cells += ((header, capture(captureCell()).trim))
            case _ => more = false
        rows += cells
      val cols = rows.map(_.size).maxOption.getOrElse(0)
      if cols == 0 then return
      sb.append(s"\\usfmtable{${"l" * cols}}{")
      for (row, ri) <- rows.zipWithIndex do
        for c <- 0 until cols do
          if c > 0 then sb.append(" & ")
          if c < row.size then
            val (header, content) = row(c)
            sb.append(if header then s"\\usfmth{$content}" else s"\\usfmtc{$content}")
        if ri < rows.length - 1 then sb.append(" \\\\ ")
      sb.append("}")

    // One table cell: text and inline spans up to the next cell, the next row, or the block that ends the table.
    private def captureCell(): Unit =
      var done = false
      while !done && i < buf.length do
        buf(i) match
          case Txt(s) =>
            sb.append(escape(s))
            i += 1
          case Marker(nm, false) =>
            val (b, _) = splitLevel(nm)
            if isTableCell(b) || b == "tr" || endsCapture(b) then done = true
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
          case Marker(_, true) =>
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
            // USFM 3.0 attributes: a pipe within a character span begins its attribute list (e.g.
            // `\w word|strong="G2532"\w*`), which runs to the span's close and carries no visible content. Keep the
            // word before the pipe and drop the attributes, then run on to the closing marker.
            val bar = s.indexOf('|')
            if bar < 0 then
              sb.append(escape(s))
              i += 1
            else
              sb.append(escape(s.substring(0, bar)))
              i += 1
              var atClose = false
              while i < buf.length && !atClose do
                buf(i) match
                  case Marker(nm2, true) if nm2 == name => atClose = true
                  case _                                => i += 1
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

    // A footnote or cross-reference note. The caller mark (the word right after the opening marker: `+` to
    // auto-number, `-` for none, or a literal caller like Louis Segond's `a`, `b`, `c`) is captured and passed as
    // the macro's first argument, so the package can honour the translation's own scheme. The body is a run of
    // inner segments, each closed implicitly by the next inner marker or the note's own close.
    private def emitNote(wrapper: String, closeBases: Set[String]): Unit =
      val caller =
        if i < buf.length && buf(i).isInstanceOf[Txt] then
          val s       = buf(i).asInstanceOf[Txt].s.dropWhile(_ == ' ')
          val sp      = s.indexOf(' ')
          if sp < 0 then { i += 1; s }
          else { buf(i) = Txt(s.substring(sp + 1)); s.substring(0, sp) }
        else ""
      sb.append(s"\\$wrapper{${escape(caller)}}{")
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
