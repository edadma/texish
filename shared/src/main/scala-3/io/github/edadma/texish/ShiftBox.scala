package io.github.edadma.texish

class ShiftBox(val box: Box, val shift: Double) extends ContentBox:

  val ascent: Double = box.ascent // The original box's ascent remains unchanged
  val descent: Double = box.descent // The original box's descent remains unchanged
  val width: Double = box.width // The width is unchanged
  val xAdvance: Double = box.xAdvance // xAdvance remains the same

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    // Apply the vertical shift by adding the `amount` to the y-coordinate
    box.draw(t, x, y + shift)

  override def toString: String = s"ShiftBox(amount=$shift, box=$box)"
