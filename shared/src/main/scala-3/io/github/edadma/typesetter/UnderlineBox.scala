package io.github.edadma.typesetter

class UnderlineBox(t: Typesetter, box: Box, thickness: Double = 0.8, color: Color = null) extends SameBox(box):
  val underlineColor: Color = Option(color) getOrElse t.currentColor

  def draw(comp: Typesetter, x: Double, y: Double): Unit =
    box.draw(comp, x, y)
    t.setLineWidth(thickness)
    t.setColor(underlineColor)
    t.drawLine(x, y + 1, x + width, y + 1)
