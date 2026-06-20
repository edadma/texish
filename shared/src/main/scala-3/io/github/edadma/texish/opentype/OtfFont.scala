package io.github.edadma.texish.opentype

/** A parsed OpenType/TrueType font, read from its raw bytes in pure Scala so the same code runs on every
  * platform — including Scala.js, where there is no system font engine to fall back on. Tables are located
  * through [[Sfnt]] and parsed on demand: the character map ([[glyphIndex]]), the horizontal metrics
  * ([[advanceWidthEm]]), and the design-grid resolution ([[unitsPerEm]]). Glyph outlines are read by the
  * outline parsers layered on top (TrueType `glyf`, CFF `CFF `), which consume the glyph indices this returns.
  *
  * Metric accessors named `…Em` report values in em — design units divided by [[unitsPerEm]] — so multiplying
  * by the point size gives points, matching the rest of the engine's point-space convention.
  */
final class OtfFont(val data: Array[Byte]):
  private val sfnt = Sfnt(data)

  /** The font's design-grid resolution (e.g. 1000 for PostScript/CFF outlines, 2048 for many TrueType fonts). */
  val unitsPerEm: Int = sfnt.unitsPerEm

  def table(tag: String): Option[TableRecord] = sfnt.table(tag)

  /** The raw bytes of one SFNT table by four-character tag, for the MATH layer and the outline readers. */
  def tableBytes(tag: String): Option[Array[Byte]] =
    sfnt.table(tag).map(r => data.slice(r.offset, r.offset + r.length))

  /** Whether the outlines are PostScript (CFF) rather than TrueType (`glyf`); selects the outline reader. */
  val isCff: Boolean = sfnt.table("CFF ").isDefined

  /** The number of glyphs in the font, from `maxp`. */
  val numGlyphs: Int = sfnt.table("maxp").map(r => ByteCursor(data, r.offset + 4).u16).getOrElse(0)

  /** The font bounding box in em, from `head` (xMin, yMin, xMax, yMax). A loose outer bound on every glyph. */
  val bboxEm: (Double, Double, Double, Double) =
    sfnt.table("head") match
      case Some(r) =>
        val c = ByteCursor(data, r.offset + 36)
        val xmin = c.i16; val ymin = c.i16; val xmax = c.i16; val ymax = c.i16
        (xmin.toDouble / unitsPerEm, ymin.toDouble / unitsPerEm, xmax.toDouble / unitsPerEm, ymax.toDouble / unitsPerEm)
      case None => (0.0, 0.0, 0.0, 0.0)

  /** The `head` table's indexToLocFormat (0 = short `loca` offsets, 1 = long), needed by the `glyf` reader. */
  val indexToLocFormat: Int =
    sfnt.table("head").map(r => ByteCursor(data, r.offset + 50).i16).getOrElse(0)

  // ─── horizontal metrics (hhea + hmtx) ─────────────────────────────────────────

  private val numHMetrics: Int =
    sfnt.table("hhea").map(r => ByteCursor(data, r.offset + 34).u16).getOrElse(0)

  /** The advance width of a glyph, in em. The `hmtx` table stores `numHMetrics` (advance, bearing) pairs; glyphs
    * beyond that share the last advance (monospaced trailing run), as the OpenType spec prescribes. */
  def advanceWidthEm(glyph: Int): Double =
    sfnt.table("hmtx") match
      case None => 0.0
      case Some(r) =>
        val i  = if numHMetrics == 0 then 0 else math.min(glyph, numHMetrics - 1)
        val au = ByteCursor(data, r.offset + i * 4).u16
        au.toDouble / unitsPerEm

  // ─── character map (cmap) ─────────────────────────────────────────────────────

  private lazy val cmap: Cmap = Cmap.parse(data, sfnt.table("cmap"))

  /** Map a Unicode codepoint to a glyph index, or 0 (.notdef) when the font has no glyph for it. */
  def glyphIndex(codepoint: Int): Int = cmap.glyphIndex(codepoint)

end OtfFont
