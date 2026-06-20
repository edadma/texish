package io.github.edadma.texish.opentype

/** One private-use assignment in a SMaFL-converted font: a math glyph that the OpenType MATH table addresses
  * only by glyph index — a size variant of a growing delimiter/radical/brace, or a piece of an assembled
  * (stretchy) glyph — given a stable codepoint in the private-use area so it can be drawn as text. `base` is
  * the Unicode codepoint of the symbol it grows from, `axis` is `'v'` or `'h'`, `kind` is `"variant"` or
  * `"part"`, and `index` is its position within that base's variant list or assembly. */
final case class SmaflEntry(
    pua:        Int,
    glyph:      Int,
    base:       Int,
    axis:       Char,
    kind:       String,
    index:      Int,
    advance:    Double,
    isExtender: Boolean,
)

/** The result of a SMaFL conversion: the repackaged font bytes (the original font with its `cmap` extended to
  * map each private-use codepoint to its variant/assembly glyph) and the table of what was assigned. */
final case class SmaflResult(font: Array[Byte], entries: Vector[SmaflEntry])

/** SMaFL — Standard Math Font Layout. SMuFL (the music-font standard the engine's `music` package already
  * uses) hand-curates a private-use codepoint for every notation glyph; SMaFL does the same for math, but
  * mechanically: a math font's OpenType MATH table already enumerates, per base symbol, the size variants and
  * assembly pieces that grow a delimiter, radical, brace, or big operator — glyphs that carry no Unicode
  * codepoint and so can only be selected by index. This converter walks that table, assigns each such glyph a
  * private-use codepoint, and rewrites the font's `cmap` so they become addressable as ordinary text.
  *
  * Why it matters: the in-browser renderer draws glyphs that have a codepoint through the browser's hinted
  * `fillText` (sharp) and only falls back to filling raw outlines for the ones that don't. After conversion the
  * tall surd, the big integral, and the extensible brace all have codepoints, so the whole formula renders
  * through the hinted path.
  */
object Smafl:

  /** The base of the private-use area assignments. The Basic Multilingual Plane PUA is U+E000–U+F8FF (6400
    * slots); a math font's variant/assembly glyphs number in the low hundreds, so they fit with room to spare. */
  val PuaBase: Int = 0xe000

  /** Convert a math font's bytes to its SMaFL form. Returns `None` if the font has no MATH table (nothing to
    * assign). The assignment is deterministic: vertical constructions first then horizontal, each in ascending
    * base-codepoint order, each construction's precomposed variants before its assembly parts, so re-running
    * the converter on the same font yields the same layout. */
  def convert(fontBytes: Array[Byte]): Option[SmaflResult] =
    MathTable.fromFontBytes(fontBytes).map { math =>
      val font = new OtfFont(fontBytes)

      // Enumerate the font's existing character map by forward-scanning Unicode, so we know both every mapping
      // to preserve and exactly which glyphs already have a codepoint (and so must not be reassigned).
      val existing  = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
      val hasCp     = scala.collection.mutable.HashSet.empty[Int]
      var cp        = 0
      while cp <= 0x10ffff do
        val g = font.glyphIndex(cp)
        if g != 0 then { existing += (cp -> g); hasCp += g }
        cp += 1

      // Walk the MATH constructions in a fixed order, giving each no-codepoint variant/assembly glyph the next
      // private-use codepoint. A glyph shared between constructions is assigned once (first occurrence wins).
      val assigned = scala.collection.mutable.LinkedHashMap.empty[Int, SmaflEntry] // glyph → entry
      var next     = PuaBase

      def baseCodepoint(gid: Int): Int = font.codepointForGlyph(gid).getOrElse(0x110000 + gid)

      def take(glyph: Int, base: Int, axis: Char, kind: String, index: Int, advance: Double, ext: Boolean): Unit =
        if glyph != 0 && !hasCp.contains(glyph) && !assigned.contains(glyph) then
          assigned(glyph) = SmaflEntry(next, glyph, base, axis, kind, index, advance, ext)
          next += 1
          if next > 0xf8ff then sys.error("SMaFL: ran out of private-use codepoints (U+E000–F8FF)")

      def walk(constructions: Map[Int, GlyphConstruction], axis: Char): Unit =
        for (baseGid, gc) <- constructions.toVector.sortBy((bg, _) => baseCodepoint(bg)) do
          val base = baseCodepoint(baseGid)
          gc.variants.zipWithIndex.foreach { case (v, i) => take(v.glyph, base, axis, "variant", i, v.advance, false) }
          gc.assembly.foreach { asm =>
            asm.parts.zipWithIndex.foreach { case (p, i) =>
              take(p.glyph, base, axis, "part", i, p.fullAdvance, p.isExtender)
            }
          }

      walk(math.variants.vertical, 'v')
      walk(math.variants.horizontal, 'h')

      val entries = assigned.values.toVector
      val merged  = existing.toVector ++ entries.map(e => e.pua -> e.glyph)
      val newCmap = SfntWriter.cmapFormat12(merged)
      val newFont = SfntWriter.rebuild(fontBytes, Map("cmap" -> newCmap), drop = Set("DSIG"))

      SmaflResult(newFont, entries)
    }
