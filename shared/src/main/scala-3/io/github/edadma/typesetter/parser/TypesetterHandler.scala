package io.github.edadma.typesetter.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.typesetter.{Box, HorizontalMode, MathMode, Typesetter, VerticalMode}

/** Handler that connects the parser language layer to a Typesetter.
  *
  * Variables live in the typesetter's scope, which stores Value directly — the parser and the layout engine read and
  * write the same map with no translation. Text, spaces and newlines are routed into the current typesetting mode.
  */
class TypesetterHandler(val typesetter: Typesetter) extends Handler:
  private var newlineCount: Int   = 0
  private var suppressed: Boolean = false

  def text(s: String): Unit =
    if !suppressed then
      typesetter.mode match
        case m: MathMode =>
          // in math, each character is a symbol classified into its own atom — not a string to shape
          var i = 0
          while i < s.length do { m.addChar(s.charAt(i).toInt); i += 1 }
        case _ =>
          // A single pending newline is an interword space — but only while we are still in the
          // paragraph. If a vertical command (\vskip, \vfill, …) closed the paragraph since the
          // newline, the pending space is stale and must be dropped, or it leaks a stray box into
          // the vertical list before the next paragraph starts.
          if newlineCount == 1 && typesetter.mode.isInstanceOf[HorizontalMode] then typesetter add " "
          typesetter.start add s
          newlineCount = 0

  def space(): Unit =
    if !suppressed then
      // spaces are ignored in math (atom spacing is computed, not typed); otherwise add a space unless in
      // vertical mode (halign cells are not HorizontalMode but accept spaces)
      if typesetter.mode.isInstanceOf[MathMode] then ()
      else if !typesetter.mode.isInstanceOf[VerticalMode] && newlineCount == 0 then typesetter.start add " "

  def newline(): Unit =
    if !suppressed then
      typesetter.mode match
        case _: HorizontalMode if newlineCount == 0 =>
          newlineCount += 1
        case _: HorizontalMode if newlineCount == 1 =>
          newlineCount += 1
          typesetter.paragraph()
        case _ =>
        // ignore newlines in vertical mode or after paragraph

  def get(name: String): Value = typesetter.get(name).getOrElse(Value.Undefined)

  def set(name: String, value: Value): Unit = typesetter.set(name, value)

  def enterScope(): Unit = typesetter.enter()

  def exitScope(): Unit = typesetter.exit()

  def suppressOutput(suppress: Boolean): Unit = suppressed = suppress

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

  /** Toggle inline math at a `$`. Entering flushes any pending interword space (a newline just before the
    * `$` is a space, as in text) and clears the pending-newline state so the math box joins the line
    * cleanly; exiting lays the math list out and drops the resulting box back into the line. */
  def toggleMath(): Unit =
    if typesetter.mode.isInstanceOf[MathMode] then
      typesetter.exitMath()
      newlineCount = 0
    else
      if newlineCount == 1 && typesetter.mode.isInstanceOf[HorizontalMode] then typesetter add " "
      newlineCount = 0
      typesetter.enterMath()

  /** Attach a super- or subscript at a `^` / `_`. Inside math, the script field (the next group or token) is
    * read, typeset by a nested math mode one style smaller, and attached to the most recent atom. Outside
    * math the marker is just its literal character, so prose containing `^` or `_` is left untouched. */
  def mathScript(proc: Processor, superscript: Boolean, pos: CharReader): Unit =
    if !suppressed then
      typesetter.mode match
        case parent: MathMode =>
          val field = proc.readScriptField(pos)

          typesetter.push(new MathMode(typesetter, parent.baseMathFont, parent.scriptStyle(superscript)))
          proc.processTokenList(field)
          val box = typesetter.mode.exit // pop the script mode and lay its list out

          if box ne null then parent.addScript(superscript, box)
        case _ =>
          text(if superscript then "^" else "_")

  /** Add a Box directly to the current mode */
  def addBox(box: Box): Unit =
    if !suppressed then
      if newlineCount == 1 && typesetter.mode.isInstanceOf[HorizontalMode] then typesetter add " "
      typesetter add box
      newlineCount = 0

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
