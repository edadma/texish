package io.github.edadma.texish.opentype

/** A small reader for the OpenType `GPOS` table's mark-attachment lookups — enough to place Hebrew niqqud
  * (and, later, Arabic marks) under, over and inside their base letters. Ordinary text needs no
  * positioning beyond the advance widths the engine already uses; combining marks do, because they carry
  * zero advance and a font positions them through GPOS anchors rather than through their own metrics.
  *
  * Two lookup kinds are read: mark-to-base (type 4), which attaches a vowel point to a consonant, and
  * mark-to-mark (type 6), which stacks one mark on another. Both are matched by anchor: the mark and its
  * base each name an anchor for the mark's class, and the mark is shifted so the two anchors coincide.
  * Extension lookups (type 9) are followed to the real subtable. Lookup flags, contextual positioning and
  * the cursive/pair kinds are not handled — none are needed for placing Hebrew points.
  *
  * The parser is pure: it consumes the raw `GPOS`, `GDEF` and `head` bytes a backend hands back through
  * `sfntTable`, so it runs identically on every platform, including the in-browser build with no system
  * font engine.
  */

/** One anchor point in font design units, y up from the baseline. */
final case class Anchor(x: Int, y: Int)

/** A mark's class and its own anchor, from a MarkArray. */
private final case class MarkRecord(classId: Int, anchor: Anchor)

/** A mark-to-base subtable: the marks it positions, the bases they attach to, and the anchor each base
  * offers for each mark class (null where a base has no anchor for a class). */
private final class MarkBaseSubtable(
    val markCov: Map[Int, Int],
    val baseCov: Map[Int, Int],
    val marks: Array[MarkRecord],
    val bases: Array[Array[Anchor]],
)

/** A mark-to-mark subtable, structured like a mark-to-base one but attaching mark1 to mark2. */
private final class MarkMarkSubtable(
    val mark1Cov: Map[Int, Int],
    val mark2Cov: Map[Int, Int],
    val mark1s: Array[MarkRecord],
    val mark2s: Array[Array[Anchor]],
)

/** Where to draw one glyph of a shaped run. A base advances the pen and attaches to nothing
  * (`attach == -1`); a mark advances the pen by nothing and is drawn at `(dxEm, dyEm)` in em, relative to
  * the origin of the glyph at index `attach`. */
final case class GlyphPlacement(isMark: Boolean, attach: Int, dxEm: Double, dyEm: Double)

object Gpos:
  /** Build a shaper from a font's raw tables, or None when the font carries no GPOS mark positioning (most
    * Latin text fonts) — in which case the caller keeps the plain advance-only path. */
  def from(gpos: Option[Array[Byte]], gdef: Option[Array[Byte]], unitsPerEm: Int): Option[Gpos] =
    gpos.flatMap { data =>
      val g = new Gpos(data, gdef, unitsPerEm)
      if g.hasMarkPositioning then Some(g) else None
    }

/** Parses the mark lookups of `data` (a `GPOS` table) on construction. */
final class Gpos(data: Array[Byte], gdefData: Option[Array[Byte]], val unitsPerEm: Int):

  private val markBaseSubs = scala.collection.mutable.ArrayBuffer.empty[MarkBaseSubtable]
  private val markMarkSubs = scala.collection.mutable.ArrayBuffer.empty[MarkMarkSubtable]

  // The set of glyphs any subtable treats as marks, used to classify glyphs when GDEF is absent.
  private val markGlyphs = scala.collection.mutable.HashSet.empty[Int]

  parseLookups()

  /** Whether the font actually has mark-positioning lookups worth running. */
  def hasMarkPositioning: Boolean = markBaseSubs.nonEmpty || markMarkSubs.nonEmpty

  // ─── GDEF glyph classes (to tell a mark from a base) ──────────────────────────

  // glyph → GDEF class (1 base, 2 ligature, 3 mark, 4 component); absent → 0.
  private val glyphClass: Int => Int =
    gdefData match
      case Some(g) if g.length >= 6 =>
        val classDefOff = ByteCursor(g, 4).u16 // GlyphClassDef offset
        if classDefOff != 0 then parseClassDef(g, classDefOff) else (_ => 0)
      case _ => (_ => 0)

  /** Whether a glyph is a combining mark — by GDEF class 3 when the font says so, otherwise by appearing
    * as a mark in some lookup. */
  def isMark(glyph: Int): Boolean =
    glyphClass(glyph) match
      case 0 => markGlyphs.contains(glyph)
      case c => c == 3

  // ─── shaping ──────────────────────────────────────────────────────────────────

  /** Resolve where each glyph of a run is drawn. Walks left to right tracking the current base and the
    * most recent mark; each mark attaches first to the base (type 4) and, failing that, to the preceding
    * mark (type 6), taking the anchor offset for its class. Bases and unmatched marks get no offset. */
  def position(glyphs: Array[Int]): Array[GlyphPlacement] =
    val n     = glyphs.length
    val out   = new Array[GlyphPlacement](n)
    val scale = 1.0 / unitsPerEm
    var lastBase = -1
    var lastMark = -1
    var i        = 0
    while i < n do
      val g = glyphs(i)
      if isMark(g) then
        var placed: GlyphPlacement = null

        if lastBase >= 0 then
          val base = glyphs(lastBase)
          var s    = 0
          while placed == null && s < markBaseSubs.length do
            val st = markBaseSubs(s)
            (st.markCov.get(g), st.baseCov.get(base)) match
              case (Some(mi), Some(bi)) =>
                val mr = st.marks(mi)
                val ba = st.bases(bi)(mr.classId)
                if ba != null then
                  placed = GlyphPlacement(true, lastBase, (ba.x - mr.anchor.x) * scale, (ba.y - mr.anchor.y) * scale)
              case _ =>
            s += 1

        if placed == null && lastMark >= 0 then
          val prev = glyphs(lastMark)
          var s    = 0
          while placed == null && s < markMarkSubs.length do
            val st = markMarkSubs(s)
            (st.mark1Cov.get(g), st.mark2Cov.get(prev)) match
              case (Some(mi), Some(m2)) =>
                val mr = st.mark1s(mi)
                val ba = st.mark2s(m2)(mr.classId)
                if ba != null then
                  placed = GlyphPlacement(true, lastMark, (ba.x - mr.anchor.x) * scale, (ba.y - mr.anchor.y) * scale)
              case _ =>
            s += 1

        out(i) = if placed != null then placed else GlyphPlacement(true, -1, 0.0, 0.0)
        lastMark = i
      else
        out(i) = GlyphPlacement(false, -1, 0.0, 0.0)
        lastBase = i
        lastMark = -1
      i += 1
    out

  // ─── parsing ────────────────────────────────────────────────────────────────

  private def anchorAt(off: Int): Anchor =
    val c = ByteCursor(data, off)
    c.u16 // anchorFormat (1, 2 or 3 — all begin with x, y; the extra fields refine for screens)
    Anchor(c.i16, c.i16)

  // A MarkArray: each covered mark's class and anchor. Offsets are from the array's own start.
  private def parseMarkArray(off: Int): Array[MarkRecord] =
    val c = ByteCursor(data, off)
    val n = c.u16
    Array.tabulate(n) { _ =>
      val cls = c.u16
      val ao  = c.u16
      MarkRecord(cls, if ao == 0 then Anchor(0, 0) else anchorAt(off + ao))
    }

  // A BaseArray (or Mark2Array): per covered glyph, one anchor per mark class (null where absent).
  private def parseBaseArray(off: Int, classCount: Int): Array[Array[Anchor]] =
    val c = ByteCursor(data, off)
    val n = c.u16
    Array.tabulate(n) { _ =>
      Array.tabulate(classCount) { _ =>
        val ao = c.u16
        if ao == 0 then null else anchorAt(off + ao)
      }
    }

  private def parseMarkBase(off: Int): Unit =
    val c = ByteCursor(data, off)
    c.u16 // posFormat (1)
    val markCovOff   = off + c.u16
    val baseCovOff   = off + c.u16
    val classCount   = c.u16
    val markArrayOff = off + c.u16
    val baseArrayOff = off + c.u16
    val markCov      = Coverage.parse(data, markCovOff)
    val baseCov      = Coverage.parse(data, baseCovOff)
    val marks        = parseMarkArray(markArrayOff)
    val bases        = parseBaseArray(baseArrayOff, classCount)
    markCov.keysIterator.foreach(markGlyphs.add)
    markBaseSubs += new MarkBaseSubtable(markCov, baseCov, marks, bases)

  private def parseMarkMark(off: Int): Unit =
    val c = ByteCursor(data, off)
    c.u16 // posFormat (1)
    val mark1CovOff   = off + c.u16
    val mark2CovOff   = off + c.u16
    val classCount    = c.u16
    val mark1ArrayOff = off + c.u16
    val mark2ArrayOff = off + c.u16
    val mark1Cov      = Coverage.parse(data, mark1CovOff)
    val mark2Cov      = Coverage.parse(data, mark2CovOff)
    val mark1s        = parseMarkArray(mark1ArrayOff)
    val mark2s        = parseBaseArray(mark2ArrayOff, classCount)
    mark1Cov.keysIterator.foreach(markGlyphs.add)
    markMarkSubs += new MarkMarkSubtable(mark1Cov, mark2Cov, mark1s, mark2s)

  // Dispatch one subtable by its lookup type, following an extension (type 9) to the wrapped subtable.
  private def parseSubtable(lookupType: Int, off: Int): Unit =
    lookupType match
      case 4 => parseMarkBase(off)
      case 6 => parseMarkMark(off)
      case 9 =>
        val c       = ByteCursor(data, off)
        c.u16 // posFormat (1)
        val extType = c.u16
        val extOff  = off + c.u32.toInt
        if extType == 4 || extType == 6 then parseSubtable(extType, extOff)
      case _ =>

  // Read the LookupList and parse every mark-positioning subtable. Script and feature selection is
  // skipped: a font's only type-4/6 lookups are its mark and mkmk lookups, which apply to the marks
  // regardless of script, so running them all in list order is correct for placing points.
  private def parseLookups(): Unit =
    if data.length < 10 then return
    val c             = ByteCursor(data, 0)
    c.u16; c.u16 // major, minor version
    c.u16        // scriptList offset (unused)
    c.u16        // featureList offset (unused)
    val lookupListOff = c.u16
    if lookupListOff == 0 then return
    val lc        = ByteCursor(data, lookupListOff)
    val lookupCnt = lc.u16
    val lookupOffsets = Array.fill(lookupCnt)(lookupListOff + lc.u16)
    for lo <- lookupOffsets do
      val l          = ByteCursor(data, lo)
      val lookupType = l.u16
      l.u16 // lookupFlag
      val subCnt = l.u16
      val subs   = Array.fill(subCnt)(lo + l.u16)
      if lookupType == 4 || lookupType == 6 || lookupType == 9 then
        for so <- subs do parseSubtable(lookupType, so)

  // An OpenType ClassDef, as a glyph → class function defaulting to 0.
  private def parseClassDef(d: Array[Byte], off: Int): Int => Int =
    val c = ByteCursor(d, off)
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
      case _ => (_ => 0)
