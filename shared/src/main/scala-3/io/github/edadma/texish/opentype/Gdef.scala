package io.github.edadma.texish.opentype

/** A reader for the OpenType `GDEF` table's glyph classifications — the data GSUB/GPOS lookup flags filter
  * matching by. Three pieces are read: the glyph class definition (1 base, 2 ligature, 3 mark, 4 component),
  * which the IGNORE_BASE_GLYPHS/IGNORE_LIGATURES/IGNORE_MARKS flag bits key on; the mark attachment class
  * definition, which the MarkAttachmentType flag byte keys on; and the mark glyph sets, which a lookup with
  * USE_MARK_FILTERING_SET names one of — the lookup then skips every mark *not* in its set. A font without
  * GDEF (or without a piece of it) classifies every glyph 0, so nothing is filtered and matching degrades to
  * plain adjacency.
  *
  * The parser is pure: it consumes the raw `GDEF` bytes a backend hands back through `sfntTable`, so it runs
  * identically on every platform, including the in-browser build with no system font engine.
  */
final class Gdef private (data: Array[Byte]):

  private val (glyphClassOff, markAttachOff, markGlyphSetsOff) =
    if data.length < 12 then (0, 0, 0)
    else
      val c = ByteCursor(data, 0)
      c.u16 // majorVersion (1)
      val minor = c.u16
      val gco   = c.u16
      c.u16 // attachListOffset
      c.u16 // ligCaretListOffset
      val mac = c.u16
      val mgs = if minor >= 2 && data.length >= 14 then c.u16 else 0
      (gco, mac, mgs)

  /** glyph → GDEF glyph class (1 base, 2 ligature, 3 mark, 4 component); 0 when unclassified. */
  val glyphClass: Int => Int = classDef(glyphClassOff)

  /** glyph → mark attachment class, for the MarkAttachmentType lookup-flag filter; 0 when unclassified. */
  val markAttachClass: Int => Int = classDef(markAttachOff)

  /** The mark glyph sets, in definition order; a USE_MARK_FILTERING_SET lookup names one by index. */
  val markGlyphSets: Vector[Set[Int]] =
    if markGlyphSetsOff == 0 then Vector.empty
    else
      val c = ByteCursor(data, markGlyphSetsOff)
      c.u16 // format (1)
      val n = c.u16
      Vector.fill(n) { Coverage.parse(data, markGlyphSetsOff + c.u32.toInt).keySet }

  // An OpenType ClassDef at `off`, as a glyph → class function defaulting to 0.
  private def classDef(off: Int): Int => Int =
    if off == 0 then _ => 0
    else
      val c = ByteCursor(data, off)
      c.u16 match
        case 1 =>
          val start = c.u16
          val n     = c.u16
          val m     = scala.collection.mutable.HashMap.empty[Int, Int]
          for i <- 0 until n do
            val v = c.u16
            if v != 0 then m(start + i) = v
          (g: Int) => m.getOrElse(g, 0)
        case 2 =>
          val n = c.u16
          val m = scala.collection.mutable.HashMap.empty[Int, Int]
          for _ <- 0 until n do
            val s   = c.u16
            val e   = c.u16
            val cls = c.u16
            var g   = s
            while g <= e do { if cls != 0 then m(g) = cls; g += 1 }
          (g: Int) => m.getOrElse(g, 0)
        case _ => _ => 0

object Gdef:
  /** The classification of a font with no GDEF: every glyph class 0, no mark sets — nothing filtered. */
  val empty: Gdef = new Gdef(Array.empty)

  /** Parse a font's `GDEF` bytes, or [[empty]] when the font has none. */
  def from(data: Option[Array[Byte]]): Gdef = data.map(new Gdef(_)).getOrElse(empty)
