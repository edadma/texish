package io.github.edadma.typesetter

class CharBox(t: Typesetter, val text: String, val font: Font, val color: Color) extends ContentBox:
  def this(t: Typesetter, text: String) = this(t, text, t.currentFont, t.currentColor)

  val TextExtents(_, yBearing, width, heightValue, xAdvance, _) =
    t.getTextExtents(text, font.renderFont.asInstanceOf[t.RenderFont])

  override val height: Double = heightValue
  val ascent: Double          = -yBearing       // Ascent is the negative yBearing
  val descent: Double         = height - ascent // Descent is height minus ascent

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y, "purple")

    if text.nonEmpty then
      t.setFont(font)
      t.setColor(color)
      t.drawString(text, x, y)

  def newCharBox(s: String): CharBox = new CharBox(t, s, font, color)

  override def toString: String =
    s"CharBox(ascent=$ascent, descent=$descent, width=$width, typeface=${font.typeface}, \"$text\")"
