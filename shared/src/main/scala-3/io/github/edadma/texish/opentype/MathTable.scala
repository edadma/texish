package io.github.edadma.texish.opentype

/** A parser for the OpenType `MATH` table — the font-supplied metric data math typesetting needs that
  * ordinary text metrics don't carry: the math axis height, fraction/rule thicknesses and the dozens of
  * other layout constants, per-glyph italic corrections and top-accent attachment points, and the
  * recipes for size variants and extensible (stretchy) glyphs. FreeType has no structured API for this
  * table, so the engine reads it as raw bytes and parses it here.
  *
  * Everything is pure: the parser consumes a byte array and produces immutable data, so it runs and is
  * tested identically on every backend. Lengths are reported in em units (design units divided by the
  * font's unitsPerEm), so a consumer multiplies by the font size in points to get points.
  */

/** A big-endian cursor over a byte slice. All OpenType scalar types are big-endian. */
final class ByteCursor(val data: Array[Byte], var pos: Int):
  def u8: Int   = { val v = data(pos) & 0xff; pos += 1; v }
  def u16: Int  = { val v = ((data(pos) & 0xff) << 8) | (data(pos + 1) & 0xff); pos += 2; v }
  def i16: Int  = u16.toShort.toInt
  def u32: Long = { val v = ((u16.toLong) << 16) | u16.toLong; v }
  def skip(n: Int): Unit = pos += n
  def seek(p: Int): Unit = pos = p

  /** A MathValueRecord is an int16 value followed by an Offset16 to an optional device table. The device
    * table refines the value for specific pixel sizes on a screen; vector output ignores it. */
  def mathValue: Int = { val v = i16; skip(2); v }

private def tagString(tag: Long): String =
  String(Array(((tag >> 24) & 0xff).toChar, ((tag >> 16) & 0xff).toChar, ((tag >> 8) & 0xff).toChar, (tag & 0xff).toChar))

/** An OpenType Coverage table: which glyphs a subtable applies to, and each glyph's index into the
  * subtable's parallel arrays. Format 1 lists glyphs; format 2 lists ranges. */
object Coverage:
  def parse(data: Array[Byte], off: Int): Map[Int, Int] =
    val c = ByteCursor(data, off)

    c.u16 match
      case 1 =>
        val n = c.u16
        (0 until n).map(i => c.u16 -> i).toMap
      case 2 =>
        val n   = c.u16
        val out = scala.collection.mutable.Map.empty[Int, Int]
        for _ <- 0 until n do
          val start    = c.u16
          val end      = c.u16
          val startIdx = c.u16
          for g <- start to end do out(g) = startIdx + (g - start)
        out.toMap
      case _ => Map.empty

  /** The covered glyphs in coverage-index order — the order the subtable's parallel arrays follow. */
  def ordered(data: Array[Byte], off: Int): Vector[Int] =
    parse(data, off).toVector.sortBy(_._2).map(_._1)

/** One SFNT table's location within a font file. */
final case class TableRecord(tag: String, offset: Int, length: Int)

/** The SFNT table directory of a font file: locates any table by its four-character tag, and reads the
  * unitsPerEm from the `head` table so design-unit metrics can be scaled to em. Handles a TrueType
  * Collection ('ttcf') by reading its first font's directory.
  */
final class Sfnt(font: Array[Byte]):
  private val base: Int =
    val c = ByteCursor(font, 0)
    if c.u32 == 0x74746366L then { c.seek(12); c.u32.toInt } // 'ttcf' → first font's offset-table offset
    else 0

  val tables: Map[String, TableRecord] =
    val c = ByteCursor(font, base)
    c.u32        // sfntVersion
    val n = c.u16
    c.skip(6)    // searchRange, entrySelector, rangeShift
    (0 until n).map { _ =>
      val tag = tagString(c.u32)
      c.u32      // checksum
      val off = c.u32.toInt
      val len = c.u32.toInt
      tag -> TableRecord(tag, off, len)
    }.toMap

  def table(tag: String): Option[TableRecord] = tables.get(tag)

  /** The font's design grid resolution; 1000 (PostScript) and 2048 (TrueType) are typical. The `head`
    * table's unitsPerEm field sits at a fixed offset of 18 bytes. Defaults to 1000 if `head` is absent. */
  val unitsPerEm: Int =
    table("head").map(r => ByteCursor(font, r.offset + 18).u16).getOrElse(1000)

/** The math layout constants (MathConstants subtable). The percentage fields are stored as fractions
  * (80% → 0.8); every length field is stored in em. Length fields are accessed by name through
  * [[value]] (all are present after a successful parse) with named accessors for the common ones.
  */
final class MathConstants(
    val scriptPercentScaleDown:        Double, // fraction
    val scriptScriptPercentScaleDown:  Double, // fraction
    val delimitedSubFormulaMinHeight:  Double, // em
    val displayOperatorMinHeight:      Double, // em
    val radicalDegreeBottomRaisePercent: Double, // fraction
    private val values: Map[String, Double],   // em, keyed by MathValueRecord field name
):
  /** A length constant by its MathConstants field name (e.g. "subscriptShiftDown"), in em. */
  def value(name: String): Double = values.getOrElse(name, sys.error(s"unknown math constant '$name'"))

  def axisHeight: Double            = values("axisHeight")
  def accentBaseHeight: Double      = values("accentBaseHeight")
  def fractionRuleThickness: Double = values("fractionRuleThickness")
  def fractionNumeratorGapMin: Double   = values("fractionNumeratorGapMin")
  def fractionDenominatorGapMin: Double = values("fractionDenominatorGapMin")
  def overbarRuleThickness: Double  = values("overbarRuleThickness")
  def underbarRuleThickness: Double = values("underbarRuleThickness")
  def radicalRuleThickness: Double  = values("radicalRuleThickness")
  def subscriptShiftDown: Double    = values("subscriptShiftDown")
  def superscriptShiftUp: Double    = values("superscriptShiftUp")
  def superscriptShiftUpCramped: Double = values("superscriptShiftUpCramped")
  def subSuperscriptGapMin: Double  = values("subSuperscriptGapMin")
  def stackGapMin: Double           = values("stackGapMin")
  def stackDisplayStyleGapMin: Double = values("stackDisplayStyleGapMin")
  def mathLeading: Double           = values("mathLeading")

object MathConstants:
  /** The MathValueRecord fields, in the exact order they appear in the MathConstants subtable after the
    * four leading non-record fields and before the trailing radicalDegreeBottomRaisePercent. */
  val valueFields: Vector[String] = Vector(
    "mathLeading", "axisHeight", "accentBaseHeight", "flattenedAccentBaseHeight",
    "subscriptShiftDown", "subscriptTopMax", "subscriptBaselineDropMin",
    "superscriptShiftUp", "superscriptShiftUpCramped", "superscriptBottomMin",
    "superscriptBaselineDropMax", "subSuperscriptGapMin", "superscriptBottomMaxWithSubscript",
    "spaceAfterScript", "upperLimitGapMin", "upperLimitBaselineRiseMin",
    "lowerLimitGapMin", "lowerLimitBaselineDropMin",
    "stackTopShiftUp", "stackTopDisplayStyleShiftUp", "stackBottomShiftDown",
    "stackBottomDisplayStyleShiftDown", "stackGapMin", "stackDisplayStyleGapMin",
    "stretchStackTopShiftUp", "stretchStackBottomShiftDown",
    "stretchStackGapAboveMin", "stretchStackGapBelowMin",
    "fractionNumeratorShiftUp", "fractionNumeratorDisplayStyleShiftUp",
    "fractionDenominatorShiftDown", "fractionDenominatorDisplayStyleShiftDown",
    "fractionNumeratorGapMin", "fractionNumDisplayStyleGapMin",
    "fractionRuleThickness", "fractionDenominatorGapMin", "fractionDenomDisplayStyleGapMin",
    "skewedFractionHorizontalGap", "skewedFractionVerticalGap",
    "overbarVerticalGap", "overbarRuleThickness", "overbarExtraAscender",
    "underbarVerticalGap", "underbarRuleThickness", "underbarExtraDescender",
    "radicalVerticalGap", "radicalDisplayStyleVerticalGap", "radicalRuleThickness",
    "radicalExtraAscender", "radicalKernBeforeDegree", "radicalKernAfterDegree",
  )

/** Per-glyph data from the MathGlyphInfo subtable. Italic corrections and top-accent attachment points
  * are in em, keyed by glyph index; extendedShapes is the set of glyphs whose vertical extent should be
  * treated as an extended shape (e.g. for cross-height accent placement). */
final case class MathGlyphInfo(
    italicsCorrection:   Map[Int, Double],
    topAccentAttachment: Map[Int, Double],
    extendedShapes:      Set[Int],
)

/** One alternate glyph for a base glyph, and the size it reaches. `advance` is the variant's measurement
  * along the stretch axis (height for vertical, width for horizontal), in em. */
final case class MathVariantRecord(glyph: Int, advance: Double)

/** One piece of an extensible (assembled) glyph — a left/top end cap, a repeatable extender, a joint, or
  * a right/bottom end cap. Connector lengths and full advance are in em. */
final case class GlyphPart(
    glyph:          Int,
    startConnector: Double,
    endConnector:   Double,
    fullAdvance:    Double,
    partFlags:      Int,
):
  /** This part may be repeated to reach an arbitrary size (a middle/extender piece). */
  def isExtender: Boolean = (partFlags & 0x0001) != 0

/** How to build a glyph too large for any single variant by assembling parts, with the assembled shape's
  * italic correction (em). */
final case class GlyphAssembly(italicsCorrection: Double, parts: Vector[GlyphPart])

/** The ways a base glyph can grow: a list of progressively larger precomposed variants, and optionally an
  * assembly recipe for sizes beyond the largest variant. */
final case class GlyphConstruction(assembly: Option[GlyphAssembly], variants: Vector[MathVariantRecord])

/** The MathVariants subtable: how delimiters, radicals, braces, arrows and big operators grow. Keyed by
  * base glyph index, split into vertically-growing and horizontally-growing constructions.
  * `minConnectorOverlap` (em) is the least an assembly's parts may overlap at a joint. */
final case class MathVariants(
    minConnectorOverlap: Double,
    vertical:            Map[Int, GlyphConstruction],
    horizontal:          Map[Int, GlyphConstruction],
)

/** A fully parsed OpenType MATH table. */
final case class MathTable(
    unitsPerEm: Int,
    constants:  MathConstants,
    glyphInfo:  MathGlyphInfo,
    variants:   MathVariants,
)

object MathTable:
  private val EmptyGlyphInfo = MathGlyphInfo(Map.empty, Map.empty, Set.empty)
  private val EmptyVariants  = MathVariants(0.0, Map.empty, Map.empty)

  /** Parse the MATH table out of a whole font file's bytes, or `None` if the font has no MATH table. */
  def fromFontBytes(font: Array[Byte]): Option[MathTable] =
    val sfnt = Sfnt(font)

    sfnt.table("MATH").map { rec =>
      parse(font.slice(rec.offset, rec.offset + rec.length), sfnt.unitsPerEm)
    }

  /** Parse a MATH table given its own bytes (offsets within the table are relative to its start) and the
    * font's unitsPerEm for scaling design units to em. Exposed directly so the parser can be tested on a
    * hand-built table without a whole font around it. */
  def parse(math: Array[Byte], unitsPerEm: Int): MathTable =
    def em(designUnits: Int): Double = designUnits.toDouble / unitsPerEm

    val c = ByteCursor(math, 0)
    c.u16 // majorVersion
    c.u16 // minorVersion
    val constOff = c.u16
    val giOff    = c.u16
    val varOff   = c.u16

    MathTable(
      unitsPerEm,
      parseConstants(math, constOff, em),
      if giOff != 0 then parseGlyphInfo(math, giOff, em) else EmptyGlyphInfo,
      if varOff != 0 then parseVariants(math, varOff, em) else EmptyVariants,
    )

  private def parseConstants(math: Array[Byte], off: Int, em: Int => Double): MathConstants =
    val c              = ByteCursor(math, off)
    val scriptPct      = c.i16 / 100.0
    val scriptScriptPct = c.i16 / 100.0
    val delimMinHeight = em(c.u16)
    val dispOpMinHeight = em(c.u16)
    val values         = MathConstants.valueFields.map(name => name -> em(c.mathValue)).toMap
    val radicalPct     = c.i16 / 100.0

    MathConstants(scriptPct, scriptScriptPct, delimMinHeight, dispOpMinHeight, radicalPct, values)

  private def parseGlyphInfo(math: Array[Byte], off: Int, em: Int => Double): MathGlyphInfo =
    val c            = ByteCursor(math, off)
    val italicOff    = c.u16
    val topAccentOff = c.u16
    val extShapeOff  = c.u16
    c.u16 // mathKernInfo — kerning cut-ins; not yet consumed by the engine

    MathGlyphInfo(
      if italicOff != 0 then valuesByCoverage(math, off + italicOff, em) else Map.empty,
      if topAccentOff != 0 then valuesByCoverage(math, off + topAccentOff, em) else Map.empty,
      if extShapeOff != 0 then Coverage.parse(math, off + extShapeOff).keySet else Set.empty,
    )

  /** Parse a subtable that is a Coverage plus a parallel array of MathValueRecords (MathItalicsCorrection
    * info and MathTopAccentAttachment share this shape), into a glyph → em map. */
  private def valuesByCoverage(math: Array[Byte], off: Int, em: Int => Double): Map[Int, Double] =
    val c       = ByteCursor(math, off)
    val covOff  = c.u16
    val count   = c.u16
    val records = (0 until count).map(_ => em(c.mathValue)).toVector
    val cov     = Coverage.parse(math, off + covOff)

    cov.collect { case (g, idx) if idx < records.length => g -> records(idx) }

  private def parseVariants(math: Array[Byte], off: Int, em: Int => Double): MathVariants =
    val c           = ByteCursor(math, off)
    val minOverlap  = em(c.u16)
    val vertCovOff  = c.u16
    val horizCovOff = c.u16
    val vertCount   = c.u16
    val horizCount  = c.u16
    val vertOffs    = (0 until vertCount).map(_ => c.u16).toVector
    val horizOffs   = (0 until horizCount).map(_ => c.u16).toVector

    def constructions(covOff: Int, offs: Vector[Int]): Map[Int, GlyphConstruction] =
      if covOff == 0 then Map.empty
      else
        Coverage
          .ordered(math, off + covOff)
          .zip(offs)
          .map((g, o) => g -> parseConstruction(math, off + o, em))
          .toMap

    MathVariants(minOverlap, constructions(vertCovOff, vertOffs), constructions(horizCovOff, horizOffs))

  private def parseConstruction(math: Array[Byte], off: Int, em: Int => Double): GlyphConstruction =
    val c            = ByteCursor(math, off)
    val assemblyOff  = c.u16
    val variantCount = c.u16
    val variants     = (0 until variantCount).map(_ => MathVariantRecord(c.u16, em(c.u16))).toVector

    GlyphConstruction(if assemblyOff != 0 then Some(parseAssembly(math, off + assemblyOff, em)) else None, variants)

  private def parseAssembly(math: Array[Byte], off: Int, em: Int => Double): GlyphAssembly =
    val c         = ByteCursor(math, off)
    val italic    = em(c.mathValue)
    val partCount = c.u16
    val parts     = (0 until partCount).map(_ => GlyphPart(c.u16, em(c.u16), em(c.u16), em(c.u16), c.u16)).toVector

    GlyphAssembly(italic, parts)
