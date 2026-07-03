package io.github.edadma.texish.opentype

/** A font's character-to-glyph map, parsed from the `cmap` table into a single lookup. A font carries several
  * encoding subtables (full Unicode, BMP-only, Mac-Roman, symbol); [[Cmap.parse]] picks the best Unicode one and
  * reads whichever subtable format it uses. The four formats here cover every font the engine bundles:
  *
  *   - **format 12** — segmented coverage over the full Unicode range (codepoints past the BMP);
  *   - **format 4**  — segment mapping for the BMP, the workhorse of most fonts;
  *   - **format 6**  — a trimmed contiguous run;
  *   - **format 0**  — a flat 256-byte table (old Mac-Roman / symbol fonts).
  *
  * An unmapped codepoint yields glyph 0, the `.notdef` glyph, matching the rest of the glyph seam.
  */
trait Cmap:
  def glyphIndex(codepoint: Int): Int

object Cmap:
  private object Empty extends Cmap:
    def glyphIndex(codepoint: Int): Int = 0

  /** Parse the `cmap` table, choosing the most capable Unicode subtable available. Preference, best first:
    * Windows full-Unicode (3,10), Windows BMP (3,1), Unicode platform (0,*), then Windows symbol (3,0). */
  def parse(data: Array[Byte], record: Option[TableRecord]): Cmap =
    record match
      case None => Empty
      case Some(r) =>
        val base = r.offset
        val c    = ByteCursor(data, base)
        c.u16 // version
        val n = c.u16

        // Read the encoding records, scoring each so we can pick the best Unicode subtable.
        case class Enc(score: Int, subOffset: Int)
        val encs =
          (0 until n).map { _ =>
            val platform = c.u16
            val encoding = c.u16
            val off      = c.u32.toInt
            val score = (platform, encoding) match
              case (3, 10) => 5 // Windows, full Unicode (UCS-4)
              case (0, 6)  => 5 // Unicode, full repertoire
              case (0, 4)  => 5
              case (3, 1)  => 4 // Windows, BMP (UCS-2)
              case (0, _)  => 3 // Unicode platform, BMP
              case (3, 0)  => 2 // Windows, symbol
              case _       => 1
            Enc(score, base + off)
          }.toVector

        encs.sortBy(-_.score).iterator.map(e => parseSubtable(data, e.subOffset)).find(_.isDefined).flatten
          .getOrElse(Empty)

  /** Parse one cmap subtable by its format; None for a format we do not handle. */
  private def parseSubtable(data: Array[Byte], off: Int): Option[Cmap] =
    val format = ByteCursor(data, off).u16
    format match
      case 0  => Some(parseFormat0(data, off))
      case 4  => Some(parseFormat4(data, off))
      case 6  => Some(parseFormat6(data, off))
      case 12 => Some(parseFormat12(data, off))
      case _  => None

  /** Format 0: a 256-entry byte array mapping single-byte codes directly to glyph ids. */
  private def parseFormat0(data: Array[Byte], off: Int): Cmap =
    val c = ByteCursor(data, off + 6) // skip format, length, language
    val ids = Array.tabulate(256)(_ => c.u8)
    (cp: Int) => if cp >= 0 && cp < 256 then ids(cp) else 0

  /** Format 6: a contiguous run of `entryCount` glyph ids starting at `firstCode`. */
  private def parseFormat6(data: Array[Byte], off: Int): Cmap =
    val c         = ByteCursor(data, off + 6) // skip format, length, language
    val firstCode = c.u16
    val count     = c.u16
    val ids       = Array.tabulate(count)(_ => c.u16)
    (cp: Int) =>
      val i = cp - firstCode
      if i >= 0 && i < count then ids(i) else 0

  /** Format 4: BMP segment mapping. Each segment [startCode, endCode] maps either by a flat delta or, when
    * `idRangeOffset` is non-zero, through the trailing `glyphIdArray`. The standard, slightly fiddly, layout. */
  private def parseFormat4(data: Array[Byte], off: Int): Cmap =
    val c = ByteCursor(data, off)
    c.u16 // format
    c.u16 // length
    c.u16 // language
    val segCount = c.u16 / 2
    c.skip(6) // searchRange, entrySelector, rangeShift

    val endCode      = Array.tabulate(segCount)(_ => c.u16)
    c.u16            // reservedPad
    val startCode    = Array.tabulate(segCount)(_ => c.u16)
    val idDelta      = Array.tabulate(segCount)(_ => c.i16)
    val rangeOffPos  = c.pos // byte offset of idRangeOffset[0], the anchor its values are relative to
    val idRangeOff   = Array.tabulate(segCount)(_ => c.u16)

    (cp: Int) =>
      if cp < 0 || cp > 0xffff then 0
      else
        // binary search for the first segment whose endCode covers cp — this lookup sits in the
        // per-character hot path, and a text font can carry hundreds of segments
        var lo = 0
        var hi = segCount - 1
        while lo < hi do
          val mid = (lo + hi) >>> 1
          if endCode(mid) < cp then lo = mid + 1 else hi = mid
        val seg = lo
        if seg >= segCount || endCode(seg) < cp || startCode(seg) > cp then 0
        else if idRangeOff(seg) == 0 then (cp + idDelta(seg)) & 0xffff
        else
          // idRangeOffset indexes into glyphIdArray, which directly follows the idRangeOffset array; the offset
          // is measured in bytes from the idRangeOffset entry itself.
          val giPos = rangeOffPos + seg * 2 + idRangeOff(seg) + (cp - startCode(seg)) * 2
          val g     = ByteCursor(data, giPos).u16
          if g == 0 then 0 else (g + idDelta(seg)) & 0xffff

  /** Format 12: segmented coverage of the full Unicode range. Each group maps a contiguous codepoint span to a
    * contiguous glyph-id span. */
  private def parseFormat12(data: Array[Byte], off: Int): Cmap =
    val c = ByteCursor(data, off + 12) // skip format, reserved, length, language
    val numGroups = c.u32.toInt
    val startChar = new Array[Int](numGroups)
    val endChar   = new Array[Int](numGroups)
    val startGid  = new Array[Int](numGroups)
    for i <- 0 until numGroups do
      startChar(i) = c.u32.toInt
      endChar(i)   = c.u32.toInt
      startGid(i)  = c.u32.toInt

    (cp: Int) =>
      var lo = 0
      var hi = numGroups - 1
      var g  = 0
      while lo <= hi do
        val mid = (lo + hi) >>> 1
        if cp < startChar(mid) then hi = mid - 1
        else if cp > endChar(mid) then lo = mid + 1
        else { g = startGid(mid) + (cp - startChar(mid)); lo = hi + 1 }
      g
