package io.github.edadma.texish

trait Box:
  def ascent: Double

  def descent: Double

  def height: Double = ascent + descent

  def width: Double

  def isSpace: Boolean

  def xAdvance: Double

  def draw(t: Typesetter, x: Double, y: Double): Unit

  def box(t: Typesetter, x: Double, y: Double, color: String = "red"): Unit =
    if t.debug then
      t.setColor(Color("blue"))
      t.drawLine(x, y, x + width - 1, y)
      t.setColor(Color(color))
      t.drawRect(x, y - ascent, width - 1, height - 1)
end Box
