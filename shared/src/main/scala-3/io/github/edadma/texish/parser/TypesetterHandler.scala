package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.{Box, CharBox, CodeHighlight, Color, HBox, HorizontalMode, MathMode, MathStyle, Mode, ParagraphMode, PictureMode, Typesetter, VerticalMode}

/** Handler that connects the parser language layer to a Typesetter.
  *
  * Variables live in the typesetter's scope, which stores Value directly — the parser and the layout engine read and
  * write the same map with no translation. Text, spaces and newlines are routed into the current typesetting mode.
  */
class TypesetterHandler(val typesetter: Typesetter) extends Handler:
  private var newlineCount: Int   = 0
  private var suppressed: Boolean = false

  // The paragraph a pending single newline belongs to. A lone newline is a deferred interword space, added
  // when the next text arrives — but only if it is the *same* paragraph. If that paragraph has since closed
  // (a vertical command, or a group/environment ending), the deferred space is stale and must be dropped, or
  // it opens the next paragraph one space too far in. Identity, not "still horizontal", is the test: the new
  // paragraph after the close is horizontal too, so a class check would wrongly keep the stale space.
  private var pendingNewlineMode: Mode | Null = null

  def text(s: String): Unit =
    if captureSink != null then captureSink.nn.append(s)
    else if !suppressed then
      typesetter.mode match
        case m: MathMode =>
          // in math, each character is a symbol classified into its own atom — not a string to shape
          var i = 0
          while i < s.length do { m.addChar(s.charAt(i).toInt); i += 1 }
        case _: PictureMode =>
          // a picture body is drawing commands, not prose; stray text between them is insignificant
          ()
        case _ =>
          // A single pending newline is an interword space, but only while we are still in the paragraph it
          // was seen in (see pendingNewlineMode). A vertical command or a closing group/environment that ended
          // that paragraph makes the space stale; the new paragraph is horizontal too, so identity is the test.
          if newlineCount == 1 && (typesetter.mode eq pendingNewlineMode) then typesetter add " "
          typesetter.start add s
          newlineCount = 0
          pendingNewlineMode = null

  def space(): Unit =
    if captureSink != null then captureSink.nn.append(" ")
    else if !suppressed then
      // spaces are ignored in math (atom spacing is computed, not typed) and in a picture body (drawing commands,
      // not prose); otherwise add a space unless in vertical mode (halign cells are not HorizontalMode but accept
      // spaces)
      if typesetter.mode.isInstanceOf[MathMode] || typesetter.mode.isInstanceOf[PictureMode] then ()
      else if !typesetter.mode.isInstanceOf[VerticalMode] && newlineCount == 0 then
        // A space at the very start of a paragraph — nothing on the line yet but the optional indent box — is a
        // leading space and is discarded, so a paragraph opened by a command that emits nothing (\noindent,
        // \indent, …) and then a space starts flush rather than one interword space in.
        typesetter.mode match
          case pm: ParagraphMode if !pm.hasContent => ()
          case _                                   => typesetter.start add " "

  def newline(): Unit =
    if captureSink != null then captureSink.nn.append("\n")
    else if !suppressed then
      typesetter.mode match
        case _: HorizontalMode if newlineCount == 0 =>
          newlineCount += 1
          pendingNewlineMode = typesetter.mode
        case _: HorizontalMode if newlineCount == 1 =>
          newlineCount += 1
          typesetter.paragraph()
        case _ =>
        // ignore newlines in vertical mode or after paragraph

  def get(name: String): Value = typesetter.get(name).getOrElse(Value.Undefined)

  def set(name: String, value: Value): Unit = typesetter.set(name, value)

  def setGlobal(name: String, value: Value): Unit = typesetter.setGlobal(name, value)

  /** Open a group `{`. Besides the variable scope every group opens, a group inside math begins a sub-formula:
    * a `{…}` in math is a self-contained math list set as a single ordinary atom, exactly as in TeX — so font
    * or class changes inside it stay inside, and an infix `\over`/`\atop` is scoped to the braces. The nested
    * mode inherits the enclosing style and the root base font, so its size cascades correctly. */
  def enterScope(): Unit =
    typesetter.enter()
    typesetter.mode match
      case parent: MathMode =>
        typesetter.push(new MathMode(typesetter, parent.baseMathFont, parent.style, grouped = true))
      case _ =>

  /** Close a group `}`. A grouped math sub-formula is laid out and dropped into its parent list as one ordinary
    * atom before the variable scope is closed; any other mode just closes its scope. */
  def exitScope(): Unit =
    typesetter.mode match
      case m: MathMode if m.grouped =>
        val box = m.exit // pop the sub-formula's mode and lay its list out

        typesetter.exit()
        if box ne null then typesetter add box // enters the parent math list as an Ord atom
      case _ => typesetter.exit()

  override def endParagraph(): Unit = typesetter.paragraph()

  /** The built-in raw-capture environments: `verbatim` (set literally) and `code` (set as a syntax-highlighted
    * listing, language from `\set codelang`). Both capture their body straight from the input. */
  override def rawEnvironment(name: String): Boolean = name == "verbatim" || name == "code"

  /** Set the body of a raw-capture environment. `verbatim` is set line for line in the typewriter face; `code`
    * is set as a highlighted listing using the language named by the `codelang` variable. */
  override def rawEnvironmentBody(name: String, body: String): Unit =
    if !suppressed then
      name match
        case "code" => placeCode(varString("codelang"), body, null)
        case _ =>
          // verbatim: the typewriter role of the current family, one uncoloured run per source line
          placeLines(() => typesetter.mono(), verbatimLines(body).map(line => Seq((line, typesetter.currentColor))))

  /** Typeset a code listing: each source line on its own output line, set in the JetBrains Mono code face with
    * leading and interior spaces preserved and no line breaking. With a `lang` the body is syntax-highlighted
    * using that language's bundled grammar and the current code theme (`codetheme`, defaulting to a light theme
    * that reads on a white page); with an empty `lang` it is set plain. An unknown language is an error. */
  def placeCode(lang: String, body: String, pos: CharReader): Unit =
    if !suppressed then
      val lines = verbatimLines(body)
      val styled: Seq[Seq[(String, Color)]] =
        if lang.isEmpty then lines.map(line => Seq((line, typesetter.currentColor)))
        else
          CodeHighlight.forLanguage(lang) match
            case Right(hl) =>
              val themeName = varString("codetheme")
              val theme     = CodeHighlight.themes.getOrElse(if themeName.isEmpty then "onelight" else themeName,
                                CodeHighlight.themes("onelight"))
              CodeHighlight.styledLines(hl, theme, lines.mkString("\n"))
            case Left(msg) => error(msg, pos)
      placeLines(() => typesetter.typeface("jetbrains"), styled)

  /** Stack a block of pre-styled lines onto the vertical list as fixed-line boxes: close any open paragraph,
    * select the listing font, then add one HBox per line of coloured runs (an empty line gets a single space so
    * it still occupies a line), and restore the running font. No line breaking — one input line is one output
    * line; leading spaces survive because each run is set as a literal fixed-width string. */
  private def placeLines(selectFont: () => Unit, lines: Seq[Seq[(String, Color)]]): Unit =
    val t = typesetter
    t.paragraph() // close any open paragraph: the block rides the vertical list
    resetNewlineCount()
    val saved = t.currentFont
    selectFont()
    for runs <- lines do
      val nonEmpty = runs.filter(_._1.nonEmpty)
      val boxes: Seq[Box] =
        if nonEmpty.isEmpty then Seq(new CharBox(t, " ", t.currentFont, t.currentColor))
        else nonEmpty.map((text, color) => new CharBox(t, text, t.currentFont, color))
      t add new HBox(boxes)
    t.currentFont = saved

  /** A variable's value as a trimmed string, or "" if it is unset — for reading the `codelang` / `codetheme`
    * selections that a `\code` block consults. */
  private def varString(name: String): String =
    get(name) match
      case Value.Undefined => ""
      case v               => Value.display(v).trim

  /** Split a raw verbatim/code body into its source lines: drop the newline that follows `\begin{…}` and the one
    * just before `\end{…}` (neither is part of the content), keep every interior blank line, and tolerate CRLF
    * endings. */
  private def verbatimLines(body: String): Seq[String] =
    var s = body
    if s.startsWith("\r\n") then s = s.substring(2)
    else if s.startsWith("\n") then s = s.substring(1)
    if s.endsWith("\r\n") then s = s.dropRight(2)
    else if s.endsWith("\n") then s = s.dropRight(1)
    s.split("\n", -1).toIndexedSeq.map(l => if l.endsWith("\r") then l.dropRight(1) else l)

  def suppressOutput(suppress: Boolean): Unit = suppressed = suppress

  override def outputSuppressed: Boolean = suppressed

  override def fontUnit(unit: String): Option[Double] =
    val font = typesetter.currentFont

    if font == null then None
    else
      unit match
        case "em" => Some(font.size)
        case "ex" => Some(font.xHeight)
        case _    => None

  def command(name: String, args: Seq[Value], pos: CharReader): Value =
    typesetter.mode match
      case m: MathMode =>
        // in math, an unknown control sequence is looked up in the math symbol tables before it is an error
        if m.addCommand(name) then Value.Nil
        else error(s"Unknown math symbol: \\$name", pos)
      case _ =>
        // Typesetting commands are registered as Primitives; unknown commands are errors
        error(s"Unknown command: \\$name", pos)

  /** Toggle math at a `$` (inline) or `$$` (display); the active-character handler has already recognised and
    * consumed the second `$` of a display delimiter and passes `display` accordingly. Entering inline flushes
    * any pending interword space (a newline just before the `$` is a space, as in text) and clears the
    * pending-newline state so the math box joins the line cleanly; entering display ends the paragraph and
    * sets the formula on its own line. Exiting lays the math list out and places the resulting box. */
  def toggleMath(display: Boolean): Unit =
    if typesetter.mode.isInstanceOf[MathMode] then
      typesetter.exitMath()
      newlineCount = 0
    else
      if !display && newlineCount == 1 && typesetter.mode.isInstanceOf[HorizontalMode] then typesetter add " "
      newlineCount = 0
      typesetter.enterMath(display)

  /** Typeset a math sub-formula — a script field, a fraction numerator, a radicand — by running its tokens
    * through a nested [[MathMode]] at the given style (so it is set at the right smaller size), and return
    * the laid-out box. Must be called with a math mode on top, which it scales from. */
  def mathSubFormula(proc: Processor, style: MathStyle, body: Vector[Token]): Box | Null =
    typesetter.mode match
      case parent: MathMode =>
        typesetter.push(new MathMode(typesetter, parent.baseMathFont, style))
        proc.processTokenList(body)
        typesetter.mode.exit // pop the sub-formula's mode and lay its list out
      case _ => null

  /** Attach a super- or subscript at a `^` / `_`. Inside math, the script field (the next group or token) is
    * read, typeset by a nested math mode one style smaller, and attached to the most recent atom. Outside
    * math the marker is just its literal character, so prose containing `^` or `_` is left untouched. */
  def mathScript(proc: Processor, superscript: Boolean, pos: CharReader): Unit =
    if !suppressed then
      typesetter.mode match
        case parent: MathMode =>
          val field = proc.readScriptField(pos)
          val box   = mathSubFormula(proc, parent.scriptStyle(superscript), field)

          if box ne null then parent.addScript(superscript, box)
        case _ =>
          text(if superscript then "^" else "_")

  /** Add a Box directly to the current mode */
  def addBox(box: Box): Unit =
    if !suppressed then
      if newlineCount == 1 && typesetter.mode.isInstanceOf[HorizontalMode] then typesetter add " "
      typesetter add box
      newlineCount = 0

  /** Emit a pending interword space (from a source newline before this point) into the current paragraph now.
    * A primitive that builds a sub-box — \href, \url, \underline — pushes a fresh hbox mode and typesets its
    * body into it; the body text would otherwise clear the pending-newline state in the wrong mode, so the
    * space before the box is dropped. Calling this while still in the paragraph (before the hbox is pushed)
    * flushes that space first. With no space pending it is a no-op. */
  def flushPendingSpace(): Unit =
    if !suppressed then
      if newlineCount == 1 && (typesetter.mode eq pendingNewlineMode) then typesetter add " "
      newlineCount = 0
      pendingNewlineMode = null

  /** Output a numeric value as text */
  def outputNumber(d: Double): Unit =
    val s = if d % 1 == 0 then d.toInt.toString else d.toString
    text(s)

  /** Reset newline count (useful after certain operations) */
  def resetNewlineCount(): Unit = newlineCount = 0

  /** Run out-of-band material (running headers, footers) with a clean inline state: a newline pending in the
    * document at shipout time must not leak a space into the header, and building the header must not disturb
    * the document's own pending state.
    */
  def isolated[A](body: => A): A =
    val saved = newlineCount

    newlineCount = 0

    try body
    finally newlineCount = saved
