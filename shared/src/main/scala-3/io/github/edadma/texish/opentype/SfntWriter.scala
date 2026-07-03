package io.github.edadma.texish.opentype

/** A growable big-endian byte buffer — the write-side counterpart to [[ByteCursor]]. Every OpenType scalar is
  * big-endian, so the primitives here emit the high byte first. Used to assemble new SFNT tables and whole
  * font files in pure Scala, so the same writer runs on every backend (including Scala.js). */
final class ByteBuf:
  private val out = new java.io.ByteArrayOutputStream

  def u8(v: Int): this.type  = { out.write(v & 0xff); this }
  def u16(v: Int): this.type = { u8((v >> 8) & 0xff); u8(v & 0xff); this }
  def u32(v: Long): this.type = { u16(((v >> 16) & 0xffff).toInt); u16((v & 0xffff).toInt); this }
  def i16(v: Int): this.type = u16(v & 0xffff)
  def bytes(a: Array[Byte]): this.type = { out.write(a, 0, a.length); this }

  def length: Int           = out.size
  def result(): Array[Byte] = out.toByteArray

/** Writers for the SFNT container and the `cmap` table. The reader side ([[Sfnt]], [[Cmap]]) locates and
  * decodes tables; this side builds a new `cmap` and repackages a font file around it with every offset and
  * checksum recomputed. It exists for SMaFL — assigning private-use codepoints to a math font's size-variant
  * and assembly glyphs so they become addressable as text — but the machinery is general: replace or add any
  * table and the directory, lengths, padding, table checksums, and `head.checkSumAdjustment` are rebuilt.
  */
object SfntWriter:

  /** Build a complete `cmap` table from a codepoint→glyph map using a single format-12 subtable (segmented
    * coverage of the full Unicode range). Format 12 covers the BMP — where the private-use area U+E000–F8FF
    * lives — as cleanly as the supplementary planes, and coalesces naturally from a sorted map. The subtable
    * is referenced by the two encoding records that conventionally carry format 12: Unicode full repertoire
    * (0, 4) and Windows full Unicode (3, 10), the pair every modern shaper and browser reads first. */
  def cmapFormat12(mappings: Iterable[(Int, Int)]): Array[Byte] =
    val sorted = mappings.toVector.distinct.sortBy(_._1)

    // Coalesce consecutive (codepoint, glyph) pairs — both advancing by one — into contiguous groups.
    val groups = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, Int)] // startChar, endChar, startGid
    for (cp, gid) <- sorted do
      groups.lastOption match
        case Some((start, end, startGid)) if cp == end + 1 && gid == startGid + (end - start) + 1 =>
          groups(groups.length - 1) = (start, cp, startGid)
        case _ =>
          groups += ((cp, cp, gid))

    val subtable = new ByteBuf
    subtable.u16(12).u16(0)                       // format, reserved
    subtable.u32(16L + groups.length * 12L)       // length: header(16) + 12 per group
    subtable.u32(0)                               // language
    subtable.u32(groups.length.toLong)
    for (start, end, startGid) <- groups do
      subtable.u32(start.toLong).u32(end.toLong).u32(startGid.toLong)
    val sub = subtable.result()

    val numEnc      = 2
    val subOffset   = 4 + numEnc * 8 // cmap header (4) + encoding records (8 each)
    val table = new ByteBuf
    table.u16(0).u16(numEnc)                       // version, numTables
    table.u16(0).u16(4).u32(subOffset.toLong)      // (platform 0, encoding 4) → subtable
    table.u16(3).u16(10).u32(subOffset.toLong)     // (platform 3, encoding 10) → subtable
    table.bytes(sub)
    table.result()

  /** Repackage a font file, replacing and/or adding the named tables and dropping any in `drop`. Every table
    * is 4-byte aligned and zero-padded; the table directory is rebuilt sorted by tag (required for its binary
    * search fields); each table's checksum and the whole-font `head.checkSumAdjustment` are recomputed so the
    * output is a structurally valid, self-consistent SFNT. The original `sfntVersion` (e.g. 'OTTO' for CFF) is
    * preserved. A `DSIG` digital signature, if present, should be dropped by the caller — editing the font
    * invalidates it. */
  def rebuild(original: Array[Byte], replace: Map[String, Array[Byte]], drop: Set[String] = Set.empty): Array[Byte] =
    val sfnt = Sfnt(original)
    // The version tag of the FONT's offset table. For a TrueType Collection Sfnt reads the first font's
    // directory, so the version must come from that same offset table — the file's first four bytes would be
    // 'ttcf', which is not a valid sfntVersion for the flattened single-font output built here.
    val sfntVersion =
      val c = ByteCursor(original, 0)
      if c.u32 == 0x74746366L then { c.seek(12); ByteCursor(original, c.u32.toInt).u32 } // 'ttcf' → first font
      else ByteCursor(original, 0).u32

    // Assemble the final tag → bytes set: originals (minus dropped), with replacements overlaid, plus any
    // brand-new tables the caller supplied.
    val tags = scala.collection.mutable.TreeMap.empty[String, Array[Byte]] // TreeMap → sorted-by-tag directory
    for (tag, rec) <- sfnt.tables if !drop.contains(tag) do
      tags(tag) = replace.getOrElse(tag, original.slice(rec.offset, rec.offset + rec.length))
    for (tag, data) <- replace if !sfnt.tables.contains(tag) && !drop.contains(tag) do
      tags(tag) = data

    val n          = tags.size
    val headerSize = 12 + 16 * n

    // Lay each table out at a 4-byte boundary, recording its offset and padded bytes.
    var offset      = headerSize
    val placed      = scala.collection.mutable.LinkedHashMap.empty[String, (Int, Array[Byte])] // tag → (offset, padded)
    var headOffset  = -1
    for (tag, raw) <- tags do
      val data = if tag == "head" then zeroCheckSumAdjustment(raw) else raw
      val padded = pad4(data)
      if tag == "head" then headOffset = offset
      placed(tag) = (offset, padded)
      offset += padded.length
    val totalSize = offset

    // SFNT header: offset table + sorted table directory with per-table checksum, offset, and true length.
    val file = new ByteBuf
    file.u32(sfntVersion)
    file.u16(n)
    val (searchRange, entrySelector, rangeShift) = searchParams(n)
    file.u16(searchRange).u16(entrySelector).u16(rangeShift)
    for (tag, (off, padded)) <- placed do
      val realLength = tags(tag).length // directory length is the true (unpadded) table length
      file.bytes(tag.getBytes("ISO-8859-1"))
      file.u32(checksum(padded))
      file.u32(off.toLong)
      file.u32(realLength.toLong)
    for (_, (_, padded)) <- placed do file.bytes(padded)

    val bytes = file.result()
    require(bytes.length == totalSize, s"sfnt layout mismatch: ${bytes.length} != $totalSize")

    // head.checkSumAdjustment = 0xB1B0AFBA − (checksum of the whole file with that field zero). It was written
    // as zero above (zeroCheckSumAdjustment), so the file checksum is taken as-is, then the field is patched.
    if headOffset >= 0 then
      val adjustment = (0xb1b0afbaL - checksum(bytes)) & 0xffffffffL
      writeU32(bytes, headOffset + 8, adjustment)
    bytes

  /** Pad a table to a multiple of four bytes with trailing zeros, per the SFNT alignment rule. */
  private def pad4(a: Array[Byte]): Array[Byte] =
    val rem = a.length % 4
    if rem == 0 then a
    else a ++ Array.fill(4 - rem)(0.toByte)

  /** A copy of the `head` table with checkSumAdjustment (bytes 8–11) zeroed, the state its own checksum and the
    * whole-font checksum are computed in. */
  private def zeroCheckSumAdjustment(head: Array[Byte]): Array[Byte] =
    val c = head.clone()
    if c.length >= 12 then { c(8) = 0; c(9) = 0; c(10) = 0; c(11) = 0 }
    c

  /** The OpenType table checksum: the sum, modulo 2^32, of the table read as big-endian uint32s. The argument
    * must already be padded to a 4-byte boundary. */
  private def checksum(padded: Array[Byte]): Long =
    var sum = 0L
    var i   = 0
    while i < padded.length do
      val v = ((padded(i) & 0xffL) << 24) | ((padded(i + 1) & 0xffL) << 16) |
        ((padded(i + 2) & 0xffL) << 8) | (padded(i + 3) & 0xffL)
      sum = (sum + v) & 0xffffffffL
      i += 4
    sum

  private def writeU32(a: Array[Byte], i: Int, v: Long): Unit =
    a(i) = ((v >> 24) & 0xff).toByte
    a(i + 1) = ((v >> 16) & 0xff).toByte
    a(i + 2) = ((v >> 8) & 0xff).toByte
    a(i + 3) = (v & 0xff).toByte

  /** The table directory's binary-search hint fields: searchRange = 16 × 2^⌊log2 n⌋, entrySelector = ⌊log2 n⌋,
    * rangeShift = 16n − searchRange. */
  private def searchParams(n: Int): (Int, Int, Int) =
    var pow = 1
    var sel = 0
    while pow * 2 <= n do { pow *= 2; sel += 1 }
    val searchRange = pow * 16
    (searchRange, sel, n * 16 - searchRange)
