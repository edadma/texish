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
      val rf = font.renderFont.asInstanceOf[t.RenderFont]
      t.markShaper(rf) match
        case Some(shaper) if Bidi.hasMarks(text) => drawMarked(t, rf, shaper, x, y)
        case _                                   => t.drawString(text, x, y)

  /** Draw a run that carries combining marks (Hebrew niqqud) glyph by glyph, positioning each mark by its
    * font anchor instead of letting it fall at the pen after its base. Base letters advance the pen and
    * are drawn at the cursor; a mark advances nothing and is drawn relative to the origin of the glyph it
    * attaches to, so a vowel point sits under (or a dagesh inside) its consonant. The text is already in
    * visual order, so a left-to-right pass over the glyphs is correct. */
  private def drawMarked(t: Typesetter, rf: t.RenderFont, shaper: io.github.edadma.texish.opentype.Gpos, x: Double, y: Double): Unit =
    val cpBuf = scala.collection.mutable.ArrayBuffer.empty[Int]
    var ci    = 0
    while ci < text.length do
      val cp = text.codePointAt(ci)
      cpBuf += cp
      ci += Character.charCount(cp)
    val glyphs = cpBuf.toArray.map(cp => t.glyphIndex(rf, cp))
    val places = shaper.position(glyphs)
    val originX = new Array[Double](glyphs.length)
    val originY = new Array[Double](glyphs.length)
    var cursor  = x
    var i       = 0
    while i < glyphs.length do
      val p = places(i)
      val (ox, oy) =
        if p.attach >= 0 then (originX(p.attach) + p.dxEm * font.size, originY(p.attach) - p.dyEm * font.size)
        else (cursor, y)
      t.drawGlyph(rf, glyphs(i), ox, oy)
      originX(i) = ox
      originY(i) = oy
      if !p.isMark then cursor += t.glyphExtents(rf, glyphs(i)).xAdvance
      i += 1

  def newCharBox(s: String): CharBox = new CharBox(t, s, font, color)

  override def toString: String =
    s"CharBox(ascent=$ascent, descent=$descent, width=$width, typeface=${font.typeface}, \"$text\")"
