package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.path.Path
import io.github.edadma.texish.*

// Helper class for simple 0-arg commands
class SimplePrimitive(action: () => Any) extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit = action()

// Helper to evaluate an argument and get its value
private[parser] def evalArg(proc: Processor, pos: CharReader): Value =
  proc.evalArgumentExpr(pos)

/** Hand back a value from a primitive: make it the result, and in document position typeset it — unless it is a
  * container, whose display is debug output rather than content.
  *
  * This is the one rule for where a value goes, and it exists because the primitives disagreed. Most both set and
  * printed, so they worked written on their own as well as inside `\set` and `\if`; but `\mapget`, `\value`,
  * `\cat`, `\contains` and `\maphas` set a result and printed nothing, so `\cat{a}{b}` in a paragraph typeset
  * nothing at all and gave no error — while `\reverse` and `\slice` printed `[b, a]` when handed a sequence,
  * which is the debug form `\sort` and `\filter` were careful not to write.
  *
  * A sequence or a map is therefore silent whatever produced it, and everything else — a number, a string, a
  * boolean, a dimension, an absence — is written. `\the` is how a container is deliberately shown.
  */
private[parser] def valueResult(proc: Processor, v: Value): Unit =
  proc.setResult(v)
  v match
    case Value.Seq(_) | Value.Map(_) => ()
    case _                           => proc.handler.text(Value.display(v))

// An inline bitmap built by \defbitmap: the backend image handle (opaque) plus its pixel size, stored as a
// Value.Native so \usebitmap can place it.
private[parser] case class InlineBitmap(handle: Any, width: Int, height: Int)

// Read an argument and coerce it to an integer (for \defbitmap's width / height / depth).
private[parser] def argInt(proc: Processor, pos: CharReader): Int =
  Value.number(evalArg(proc, pos)).map(_.toInt).getOrElse(0)

// Decode base64 to bytes, ignoring any whitespace and padding. A small self-contained decoder (no java.util),
// so it compiles on every backend. Both the standard (`+/`) and URL-safe (`-_`) alphabets are accepted; embedded
// data uses the URL-safe form so it never contains `//`, which would otherwise be read as a comment.
private[parser] def base64Decode(s: String): Array[Byte] =
  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  val lookup   = Array.fill(128)(-1)
  for i <- alphabet.indices do lookup(alphabet(i).toInt) = i
  lookup('-'.toInt) = 62
  lookup('_'.toInt) = 63
  val out    = scala.collection.mutable.ArrayBuffer[Byte]()
  var buffer = 0
  var bits   = 0
  for c <- s do
    val v = if c.toInt < 128 then lookup(c.toInt) else -1
    if v >= 0 then
      buffer = (buffer << 6) | v
      bits += 6
      if bits >= 8 then
        bits -= 8
        out += ((buffer >> bits) & 0xff).toByte
  out.toArray

// Unpack a packed alpha bitmap into straight ARGB pixels. Each pixel is `depth` bits (MSB-first, row-major),
// giving an alpha level from 0 (transparent) to full (opaque); the colour is black, so the value is the glyph's
// coverage. 1-bit is on/off; 2/4/8-bit carry antialiased grey levels.
private[parser] def unpackBitmapAlpha(bytes: Array[Byte], width: Int, height: Int, depth: Int): Array[Int] =
  val n    = width * height
  val argb = new Array[Int](n)
  val maxv = (1 << depth) - 1
  var bitpos = 0
  var i      = 0
  while i < n do
    var v = 0
    var d = 0
    while d < depth do
      val byteIdx = bitpos >> 3
      val bit     = if byteIdx < bytes.length then (bytes(byteIdx) >> (7 - (bitpos & 7))) & 1 else 0
      v = (v << 1) | bit
      bitpos += 1
      d += 1
    val alpha = if maxv == 0 then 0 else v * 255 / maxv
    argb(i) = alpha << 24
    i += 1
  argb

// A dimension value in big points: Dimen carries its own unit; a bare number means points
private[parser] def points(v: Value): Option[Double] = v match
  case Value.Dimen(p) => Some(p.toDouble)
  case Value.Num(n)   => Some(n.toDouble)
  case _              => None

/** Resolve one axis of `\geometry`: given the sheet extent `P` and whichever of an equal margin, the two edge
  * margins, and the text size were supplied, return the `(offset, textSize)` pair for that axis. The priority
  * mirrors LaTeX's geometry package: an equal margin wins; then a fixed pair of edge margins makes the text the
  * remainder; then a text size is positioned by whichever single edge margin is fixed, or centred if neither is;
  * then a lone edge margin shifts the current text block; then a bare `centering` re-centres it; otherwise the
  * axis is left unchanged. `P − offset − textSize` is the far margin, so the frame always closes. */
private[parser] def resolveGeometryAxis(
    P: Double,
    marginBoth: Option[Double],
    low: Option[Double],
    high: Option[Double],
    text: Option[Double],
    center: Boolean,
    curOffset: Double,
    curSize: Double,
): (Double, Double) =
  marginBoth match
    case Some(m) => (m, P - 2 * m)
    case None =>
      (low, high, text) match
        case (Some(l), Some(h), _)     => (l, P - l - h)        // both edges fixed → text is what's left
        case (Some(l), None, Some(tw)) => (l, tw)               // near edge + width
        case (None, Some(h), Some(tw)) => (P - h - tw, tw)      // far edge + width
        case (None, None, Some(tw))    => ((P - tw) / 2, tw)    // width alone → centred (as in LaTeX)
        case (Some(l), None, None)     => (l, curSize)          // near edge alone → shift, keep width
        case (None, Some(h), None)     => (P - h - curSize, curSize) // far edge alone → keep width
        case (None, None, None)        => if center then ((P - curSize) / 2, curSize) else (curOffset, curSize)

// Text script geometry (\textsub / \textsup): the body is set at scriptScale of the current font size and
// its box shifted by a fraction of that size below (sub) or above (sup) the baseline.
private val scriptScale = 0.7
private val subDrop     = 0.18
private val supRise     = 0.42

// Resolve an image path the way \use resolves a module: an absolute path is used as given, a relative one is
// taken relative to the directory of the document being processed, so \includegraphics{frog.jpg} finds the
// image beside the source file rather than relative to the working directory.
private[parser] def resolveImagePath(proc: Processor, path: String): String =
  val p = Path(path)
  if p.isAbsolute then path else (Path(proc.currentDir) / p).toString

// Parse \includegraphics's optional [width=…,height=…,scale=…] list. The bracket tokens are flattened back to
// their source text (a captured \linewidth survives as the literal "\linewidth"), split on commas into
// key=value entries, and each value resolved as a length or a scale factor. An absent list yields all-None.
private[parser] def parseGraphicsOptions(
    proc: Processor,
    t: Typesetter,
    opts: Option[Vector[Token]],
    pos: CharReader,
): (Option[Double], Option[Double], Option[Double]) =
  opts match
    case None => (None, None, None)
    case Some(tokens) =>
      val raw = tokensToSource(tokens)
      var width: Option[Double]  = None
      var height: Option[Double] = None
      var scale: Option[Double]  = None
      for entry <- raw.split(",") if entry.trim.nonEmpty do
        val eq = entry.indexOf('=')
        if eq < 0 then proc.handler.error(s"\\includegraphics option '${entry.trim}' must be key=value", pos)
        val key   = entry.substring(0, eq).trim
        val value = entry.substring(eq + 1).trim
        key match
          case "width"  => width = Some(resolveGraphicsLength(proc, t, value, pos))
          case "height" => height = Some(resolveGraphicsLength(proc, t, value, pos))
          case "scale" =>
            scale = Some(value.toDoubleOption.getOrElse(proc.handler.error(s"\\includegraphics: scale '$value' is not a number", pos)))
          case other => proc.handler.error(s"\\includegraphics: unknown option '$other'", pos)
      (width, height, scale)

// Flatten a token list back to its source text, the way \includegraphics recovers a length written with a
// control word: a captured \linewidth survives as the literal "\linewidth" so resolveLength can read it. Used
// where an argument is a length expression (\makebox / \parbox / \minipage widths) rather than something to set.
private[parser] def tokensToSource(tokens: Seq[Token]): String =
  tokens.map {
    case Token.Text(s, _)       => s
    case Token.ControlSeq(n, _) => "\\" + n
    case Token.Space(_, _)      => " "
    case Token.Newline(_)       => " "
    case _                      => ""
  }.mkString

// Resolve a length written as a factor times \linewidth or \textwidth (both the current line width, the bare
// command meaning factor 1), or an ordinary dimension like 200pt / 5cm / 0.5in. `cmd` names the command for any
// error. Shared by \includegraphics, \makebox, \parbox and \minipage.
private[parser] def resolveLength(proc: Processor, t: Typesetter, s0: String, pos: CharReader, cmd: String): Double =
  val s = s0.trim
  def factorTimesLineWidth(suffix: String): Double =
    val f = s.dropRight(suffix.length).trim
    val factor =
      if f.isEmpty then 1.0
      else f.toDoubleOption.getOrElse(proc.handler.error(s"$cmd: '$f' is not a number", pos))
    factor * t.getNumber("hsize")

  if s.endsWith("\\linewidth") then factorTimesLineWidth("\\linewidth")
  else if s.endsWith("\\textwidth") then factorTimesLineWidth("\\textwidth")
  else
    parseDimension(s, proc.handler.fontUnit) match
      case Some(Value.Dimen(p)) => p
      case _                    => proc.handler.error(s"$cmd: '$s' is not a length", pos)

private[parser] def resolveGraphicsLength(proc: Processor, t: Typesetter, s: String, pos: CharReader): Double =
  resolveLength(proc, t, s, pos, "\\includegraphics")

// Align a finished \parbox / \minipage vbox on the surrounding baseline: t leaves a \vtop as built (its reference
// is the first line's baseline), b leaves a \vbox as built (reference at the last line's baseline), and c — the
// default — raises the box by half so its vertical centre sits on the baseline, with the box's stated height and
// depth following the shift (a RaiseBox, not a bare \raise) so the surrounding line opens to make room.
private[parser] def alignParbox(vb: VerticalBox, align: Char): Box = align match
  case 't' | 'b' => vb
  case _         => new RaiseBox(vb, (vb.descent - vb.ascent) / 2)

// Open the vertical builder for a \parbox / \minipage. Without a fixed height it is a \vtop when top-aligned and a
// \vbox otherwise, exactly as before. With a fixed `height` it is always a \vbox set to that height, and the content
// is positioned within it by fil glue: this adds the glue ABOVE the content (so `b` sinks it to the bottom, and `c`
// centres it once the caller adds the matching glue below with `closeFixedVbox`). `t` and `s` add nothing here.
private[parser] def openFixedVbox(t: Typesetter, align: Char, height: Option[Double], inner: Char): Unit =
  val toVal: Double | Null = height match
    case Some(h) => h
    case None    => null
  if align == 't' && height.isEmpty then t.vtop(toVal, null) else t.vbox(toVal, null)
  if height.isDefined && (inner == 'b' || inner == 'c') then t.fil

// Add the fil glue BELOW a fixed-height \parbox / \minipage body (after the paragraph is closed), so `t` holds the
// content at the top and `c` completes the centring begun by openFixedVbox. `b` and `s` add nothing here.
private[parser] def closeFixedVbox(t: Typesetter, height: Option[Double], inner: Char): Unit =
  if height.isDefined && (inner == 't' || inner == 'c') then t.fil

// Build an \hbox or \vbox whose command token has already been consumed: read its optional `to:` target
// and braced body, typeset the body into a fresh builder, and return the finished box *without* adding it
// to the current list. Shared by the \hbox / \vbox / \vtop / \setbox primitives and \lower / \raise. `vertical`
// selects a vertical builder, `top` makes it a \vtop (reference point on the first line). The optional `to:` sets
// the final size and `spread:` adds to the natural size; at most one may be given.
private[parser] def buildBox(proc: Processor, t: Typesetter, vertical: Boolean, top: Boolean, pos: CharReader): Box | Null =
  val opts = proc.readOptionalParams(pos)
  val body = proc.readArgument(pos)
  // build Double | Null directly — boxing through java.lang.Double would unbox null to 0.0
  val toVal: Double | Null = opts.get("to").flatMap(points) match
    case Some(d) => d
    case None    => null
  val spreadVal: Double | Null = opts.get("spread").flatMap(points) match
    case Some(d) => d
    case None    => null
  if toVal != null && spreadVal != null then proc.handler.error("a box takes either to: or spread:, not both", pos)
  if vertical then (if top then t.vtop(toVal, spreadVal) else t.vbox(toVal, spreadVal)) else t.hbox(toVal, spreadVal)
  proc.handler.uncaptured(proc.processTokenList(body)) // scoping happens automatically from { } tokens
  t.paragraph() // close any paragraph the body opened in vertical mode, so exit sees the box builder itself
  t.mode.exit

// Read the <box> that follows \lower / \raise / \setbox — the next \hbox, \vbox, or \vtop, built and returned
// without adding it to the current list, so the caller can wrap, shift, or store it. Returns null for
// anything that is not a box command.
private[parser] def readBoxArg(proc: Processor, handler: TypesetterHandler, t: Typesetter, pos: CharReader): Box | Null =
  proc.skipSpaces()
  if !proc.hasMoreTokens then null
  else
    proc.peekToken() match
      case Token.ControlSeq(name, _) if name == "hbox" || name == "vbox" || name == "vtop" =>
        proc.nextToken() // consume the box command
        buildBox(proc, t, vertical = name != "hbox", top = name == "vtop", pos)
      case Token.ControlSeq("vsplit", _) =>
        proc.nextToken() // consume \vsplit; it produces the top piece and leaves the remainder in its register
        vsplitBox(proc, handler, t, pos)
      case _ => null

// Typeset a braced `{…}` argument as a single LR-mode hbox and return it (null when empty) — the way the
// wrapping boxes (\fbox, \colorbox, \rotatebox, \scalebox, \resizebox, \raisebox, …) capture their content,
// matching \underline. The body is read first, then the pending interword space is flushed before the hbox is
// pushed, so a source newline before the command keeps its space.
private[parser] def captureHBox(proc: Processor, t: Typesetter, handler: TypesetterHandler, pos: CharReader): Box | Null =
  val body = proc.readArgument(pos)
  handler.flushPendingSpace()
  t.hbox(null)
  proc.handler.uncaptured(proc.processTokenList(body))
  t.mode.exit

// Read a numeric variable in points, falling back to `default` when it is unset — used for \fboxsep / \fboxrule,
// which a document may set with \set but which have LaTeX-default values otherwise.
private[parser] def numVarOr(t: Typesetter, name: String, default: Double): Double =
  t.get(name).flatMap(points).getOrElse(default)

// Apply a colour primitive's optional [alpha] to a colour, overriding whatever alpha the colour already carries; a
// missing bracket (None) leaves the colour untouched. Shared by \color, \textcolor, \pagecolor and \colorbox.
private[parser] def withAlpha(c: Color, alpha: Option[Double]): Color =
  alpha.fold(c)(a => c.copy(alpha = a))

// Read an optional bracketed number ([0.5], [-1]) following \scalebox, in the style of readPlacementSpec. Returns
// None (leaving the stream untouched) when no '[' run follows.
private[parser] def readOptionalNumber(proc: Processor): Option[Double] =
  proc.skipSpaces()
  proc.peekToken() match
    case Token.Text(s, sp) if s.startsWith("[") =>
      proc.nextToken()
      val out    = new StringBuilder
      var closed = false

      def takeText(str: String, p: CharReader): Unit =
        val idx = str.indexOf(']')
        if idx < 0 then out ++= str
        else
          out ++= str.substring(0, idx)
          val after = str.substring(idx + 1)
          if after.nonEmpty then proc.pushBack(Vector(Token.Text(after, p)))
          closed = true

      takeText(s.substring(1), sp)
      while !closed && proc.hasMoreTokens do
        proc.nextToken() match
          case Token.Text(str, p) => takeText(str, p)
          case Token.EOF(_)       => closed = true
          case _                  => ()
      out.toString.trim.toDoubleOption
    case _ => None

// Read an optional [ … ] argument as a token list, respecting brace nesting so control sequences and groups
// inside it survive to be typeset by the caller (unlike readPlacementSpec / readOptionalNumber, which flatten to
// text). Used for \footnote's explicit marker. Returns None, leaving the stream untouched, when no '[' follows.
private[parser] def readBracketTokens(proc: Processor): Option[Vector[Token]] =
  proc.skipSpaces()
  proc.peekToken() match
    case Token.Text(s, sp) if s.startsWith("[") =>
      proc.nextToken()
      if s.length > 1 then proc.pushBack(Vector(Token.Text(s.substring(1), sp)))
      val toks  = Vector.newBuilder[Token]
      var depth = 0
      var done  = false
      while !done && proc.hasMoreTokens do
        proc.peekToken() match
          case Token.BeginGroup(_) => depth += 1; toks += proc.nextToken()
          case Token.EndGroup(_)   => depth -= 1; toks += proc.nextToken()
          case Token.Text(str, p) if depth == 0 && str.contains(']') =>
            proc.nextToken()
            val idx = str.indexOf(']')
            if idx > 0 then toks += Token.Text(str.substring(0, idx), p)
            val after = str.substring(idx + 1)
            if after.nonEmpty then proc.pushBack(Vector(Token.Text(after, p)))
            done = true
          case Token.EOF(_) => done = true
          case _            => toks += proc.nextToken()
      Some(toks.result())
    case _ => None

// Read a \resizebox dimension argument: a braced dimension, or `!` to mean "keep the aspect ratio set by the
// other dimension" (returned as None).
private[parser] def resizeDim(proc: Processor, pos: CharReader): Option[Double] =
  evalArg(proc, pos) match
    case Value.Text(s) if s.trim == "!" => None
    case v                              => points(v)

// Typeset a braced `{…}` argument as horizontal material and return its boxes — used for the three parts of a
// \discretionary. The content is set in a throwaway \hbox so it goes through the normal text path (font, kerning,
// ligatures); the resulting boxes are handed back individually so the line breaker can place them on either side
// of a break. An empty group yields no boxes.
private[parser] def typesetGroupBoxes(proc: Processor, t: Typesetter, pos: CharReader): Seq[Box] =
  val body = proc.readArgument(pos)
  t.hbox()
  proc.processTokenList(body)
  t.mode.exit match
    case null    => Seq.empty
    case h: HBox => h.boxes
    case b: Box  => Seq(b)

// Split a vertical list for a target height, returning the boxes above the break and the boxes from the break
// onward. When the whole list already fits the target it is taken intact (the end of a list is always a valid
// break) and the remainder is empty; otherwise the cut is the latest legal interior breakpoint whose prefix is
// no taller than the target — the same first-fit rule and legal-breakpoint test the page builder uses
// (PageMode.breakPage), and, if none fits, the latest legal break regardless of height. A break is legal at a
// penalty below the inhibit threshold, or at breakable glue whose predecessor is not itself discardable. The
// break item and any discardables that follow it vanish at the top of the remainder, so the lower piece never
// begins with stray space. With no legal breakpoint at all the whole list is the top and the remainder is empty.
private[parser] def splitVList(boxes: Seq[Box], target: Double): (Seq[Box], Seq[Box]) =
  val v = boxes.toVector

  // A break is legal at a penalty below the inhibit threshold or at discardable vertical space whose predecessor
  // is not itself discardable. The space appears as live Glue while a list is still being built (the page builder
  // breaks such a list) and as a resolved VSpaceBox once a vbox has been finalized into a register (what \vsplit
  // is handed), so both forms count.
  def legal(i: Int): Boolean =
    v(i) match
      case p: Penalty   => p.penalty < Penalty.Inhibit
      case g: Glue      => !g.nobreak && i > 0 && !v(i - 1).isSpace
      case _: VSpaceBox => i > 0 && !v(i - 1).isSpace
      case _            => false

  val heights = v.scanLeft(0.0)(_ + _.height) // measure in vertical mode is the box height

  if heights(v.length) <= target then (v, Vector.empty)
  else
    val candidates = (v.length - 1) to 1 by -1

    candidates.find(i => legal(i) && heights(i) <= target).orElse(candidates.find(legal)) match
      case None    => (v, Vector.empty)
      case Some(i) => (v.take(i), v.drop(i).dropWhile(_.isSpace))

// Read the `name to:<height>` that follows \vsplit, split that vbox register, leave the remainder in the register
// (emptied when nothing is left), and return the top piece as a \vbox. Shared by the \vsplit primitive and by
// readBoxArg, so \setbox top \vsplit src to:144pt captures the top while src keeps the rest.
private[parser] def vsplitBox(proc: Processor, handler: TypesetterHandler, t: Typesetter, pos: CharReader): Box | Null =
  val name = proc.readIdentifier(pos)
  proc.readOptionalParams(pos).get("to").flatMap(points) match
    case None => handler.error("\\vsplit expects a target height, as in \\vsplit name to:144pt", pos)
    case Some(target) =>
      boxRegister(proc, handler, name, "vsplit", pos) match
        case vb: VerticalBox =>
          val (topBoxes, rest) = splitVList(vb.boxes, target)
          proc.handler.set(name, if rest.isEmpty then Value.Undefined else Value.Native(new VBox(rest)))
          new VBox(topBoxes)
        case _ => handler.error(s"\\vsplit: '$name' is not a \\vbox or \\vtop", pos)

// Fetch a box stored in a register by \setbox. Errors (rather than returning a sentinel) when the register
// is empty or holds a non-box, so a misused box command points at the offending name.
private[parser] def boxRegister(proc: Processor, handler: TypesetterHandler, name: String, cmd: String, pos: CharReader): Box =
  proc.handler.get(name) match
    case Value.Native(b: Box) => b
    case Value.Undefined      => handler.error(s"\\$cmd: box register '$name' is empty", pos)
    case _                    => handler.error(s"\\$cmd: '$name' is not a box", pos)

// Resolve a glue argument: a braced glue spec ({12pt plus 2pt}), a glue-valued variable, or a bare dimension
// optionally continued by `plus`/`minus` keywords in the token stream (\vskip 12pt plus 2pt minus 1fil)
// The position of the next argument token (after any intervening spaces), or the command's own
// position when nothing follows. An "expects an argument" error reports this, so the caret points
// at the offending argument rather than at the start of the command.
private[parser] def argumentPos(proc: Processor, command: CharReader): CharReader =
  proc.skipSpaces()
  if proc.hasMoreTokens then Token.pos(proc.peekToken()) else command

private[parser] def glueArg(proc: Processor, pos: CharReader): Option[Glue] =
  proc.evalArgumentExpr(pos) match
    case Value.Glue(n, st, sh, sto, sho) => Some(Glue(n, st, sh, sto, sho))
    case Value.Native(g: Glue)           => Some(g)
    case Value.Dimen(p)                  => Some(glueContinuation(proc, p, pos))
    case Value.Num(n)                    => Some(glueContinuation(proc, n, pos))
    case _                               => None

private[parser] def glueContinuation(proc: Processor, natural: Double, pos: CharReader): Glue =
  proc.readGlueContinuation(natural, pos) match
    case Value.Glue(n, st, sh, sto, sho) => Glue(n, st, sh, sto, sho)
    case _                               => Glue(natural)

// Read an optional [htb] placement specifier following a float command, in the style of \sqrt's [degree]. Brackets
// are ordinary text, so the opening '[' may begin a text token and the closing ']' may sit mid-token; this scans
// across tokens, splitting text runs at the brackets and pushing back any tail after ']'. The result keeps only the
// placement letters h/t/b, in order. Returns None when no '[' run follows, leaving the stream untouched.
private[parser] def readPlacementSpec(proc: Processor): Option[String] =
  proc.skipSpaces()
  proc.peekToken() match
    case Token.Text(s, sp) if s.startsWith("[") =>
      proc.nextToken()
      val out    = new StringBuilder
      var closed = false

      def takeText(str: String, p: CharReader): Unit =
        val idx = str.indexOf(']')
        if idx < 0 then out ++= str
        else
          out ++= str.substring(0, idx)
          val after = str.substring(idx + 1)
          if after.nonEmpty then proc.pushBack(Vector(Token.Text(after, p)))
          closed = true

      takeText(s.substring(1), sp)
      while !closed && proc.hasMoreTokens do
        proc.nextToken() match
          case Token.Text(str, p) => takeText(str, p)
          case Token.EOF(_)       => closed = true
          case _                  => ()
      Some(out.toString.filter("htb".contains(_)))
    case _ => None
