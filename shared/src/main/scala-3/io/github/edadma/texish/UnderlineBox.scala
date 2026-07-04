package io.github.edadma.texish

class UnderlineBox(t: Typesetter, box: Box, thickness: Double = 0.8, color: Color = null) extends SameBox(box):
  val underlineColor: Color = Option(color) getOrElse t.currentColor

  def draw(comp: Typesetter, x: Double, y: Double): Unit =
    box.draw(comp, x, y)
    // bracket the pen change so the underline's width and colour don't leak into later strokes (a picture
    // drawn after an underline would inherit the 0.8 pen, differently per backend)
    t.gsave()
    t.setLineWidth(thickness)
    t.setColor(underlineColor)
    t.drawLine(x, y + 1, x + width, y + 1)
    t.grestore()
