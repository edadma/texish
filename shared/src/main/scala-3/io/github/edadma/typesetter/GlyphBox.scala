package io.github.edadma.typesetter

/** A box holding a single glyph, identified by its index in [[font]], drawn through the backend's glyph
  * seam rather than the string seam. This is the placement primitive math mode is built on: the engine
  * computes every position itself, so glyphs go in by index — not as a string — and the backend does no
  * shaping or kerning of its own. Ordinary text uses [[CharBox]] and the string path; the two box kinds
  * coexist in the same list and on the same page without interacting, beyond sharing a font.
  */
class GlyphBox(t: Typesetter, val glyph: Int, val font: Font, val color: Color) extends ContentBox:
  def this(t: Typesetter, glyph: Int) = this(t, glyph, t.currentFont, t.currentColor)

  val TextExtents(_, yBearing, width, heightValue, xAdvance, _) =
    t.glyphExtents(font.renderFont.asInstanceOf[t.RenderFont], glyph)

  override val height: Double = heightValue
  val ascent: Double          = -yBearing       // ascent is the negative yBearing (part above the baseline)
  val descent: Double         = height - ascent

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y, "purple")
    t.setColor(color)
    t.drawGlyph(font.renderFont.asInstanceOf[t.RenderFont], glyph, x, y)

  override def toString: String =
    s"GlyphBox(ascent=$ascent, descent=$descent, width=$width, typeface=${font.typeface}, glyph=$glyph)"
