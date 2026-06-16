package io.github.edadma.texish

class CharBox(t: Typesetter, val text: String, val font: Font, val color: Color) extends ContentBox:
  def this(t: Typesetter, text: String) = this(t, text, t.currentFont, t.currentColor)

  val TextExtents(_, yBearing, _, heightValue, advance, _) =
    t.getTextExtents(text, font.renderFont.asInstanceOf[t.RenderFont])

  // The box width is the pen advance, not the ink bounding box. The advance carries the glyphs' side
  // bearings, so one box sets flush after the next exactly as the font intends — and manual \kern between
  // pieces of text (as in the \TeX logo) lands on the same metrics it would in TeX. Using the ink width
  // instead silently dropped the leading and trailing side bearings of every run; justification mostly hid
  // it, but exact character spacing did not survive. The ink box still gives the vertical extents below.
  override val width: Double    = advance
  override val xAdvance: Double = advance
  override val height: Double   = heightValue
  val ascent: Double            = -yBearing       // Ascent is the negative yBearing
  val descent: Double           = height - ascent // Descent is height minus ascent

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y, "purple")

    if text.nonEmpty then
      t.setFont(font)
      t.setColor(color)
      t.drawString(text, x, y)

  def newCharBox(s: String): CharBox = new CharBox(t, s, font, color)

  override def toString: String =
    s"CharBox(ascent=$ascent, descent=$descent, width=$width, typeface=${font.typeface}, \"$text\")"
