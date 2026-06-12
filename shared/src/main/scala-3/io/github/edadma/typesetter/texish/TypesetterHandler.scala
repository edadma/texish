package io.github.edadma.typesetter.texish

import io.github.edadma.char_reader.CharReader
import io.github.edadma.typesetter.{Box, HorizontalMode, Typesetter, VerticalMode}

/** Handler that connects the texish language layer to a Typesetter.
  *
  * Variables live in the typesetter's scope, which stores Value directly — the parser and the layout engine read and
  * write the same map with no translation. Text, spaces and newlines are routed into the current typesetting mode.
  */
class TypesetterHandler(val typesetter: Typesetter) extends Handler:
  private var newlineCount: Int   = 0
  private var suppressed: Boolean = false

  def text(s: String): Unit =
    if !suppressed then
      if newlineCount == 1 then typesetter add " "
      typesetter.start add s
      newlineCount = 0

  def space(): Unit =
    if !suppressed then
      // Add space unless in vertical mode (halign cells are not HorizontalMode but accept spaces)
      if !typesetter.mode.isInstanceOf[VerticalMode] && newlineCount == 0 then typesetter.start add " "

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

  def command(name: String, args: Seq[Value], pos: CharReader): Value =
    // Typesetting commands are registered as Primitives; unknown commands are errors
    error(s"Unknown command: \\$name", pos)

  /** Add a Box directly to the current mode */
  def addBox(box: Box): Unit =
    if !suppressed then
      if newlineCount == 1 then typesetter add " "
      typesetter add box
      newlineCount = 0

  /** Output a numeric value as text */
  def outputNumber(d: Double): Unit =
    val s = if d % 1 == 0 then d.toInt.toString else d.toString
    text(s)

  /** Reset newline count (useful after certain operations) */
  def resetNewlineCount(): Unit = newlineCount = 0
