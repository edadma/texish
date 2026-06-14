package io.github.edadma.typesetter

import io.github.edadma.typesetter.opentype.MathTable

/** The font math mode sets type in, paired with its parsed OpenType `MATH` table. A genuine math font
  * (Latin Modern Math, STIX Two, …) supplies the table; an ordinary text font has none, in which case
  * TeX's traditional defaults stand in for the few constants Stage 2 needs. Font-relative `MATH` constants
  * are returned already scaled to points by the font size, so callers work in points throughout.
  */
class MathFont(val t: Typesetter, val font: Font, val math: Option[MathTable]):
  private def renderFont: t.RenderFont = font.renderFont.asInstanceOf[t.RenderFont]

  /** The math font's size in points — one em. */
  def size: Double = font.size

  /** The glyph index for a Unicode codepoint in this font (0, the .notdef glyph, when the font lacks it). */
  def glyphIndex(codepoint: Int): Int = t.glyphIndex(renderFont, codepoint)

  /** The math axis height in points: the line relations, binary operators, fraction bars and fences centre
    * on. Comes from the `MATH` table when present, otherwise a quarter em — TeX's `axis_height` for
    * Computer Modern at text size. */
  def axisHeight: Double = math.map(_.constants.axisHeight).getOrElse(0.25) * font.size

  /** A single-glyph box for a codepoint, set in this font in the current colour. */
  def glyphBox(codepoint: Int): GlyphBox = new GlyphBox(t, glyphIndex(codepoint), font, t.currentColor)
