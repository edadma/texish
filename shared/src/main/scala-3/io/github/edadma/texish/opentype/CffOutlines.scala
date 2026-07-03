package io.github.edadma.texish.opentype

import scala.collection.mutable.ArrayBuffer

/** Reads glyph outlines from a PostScript-flavoured OpenType font's `CFF ` table by interpreting each glyph's
  * Type-2 charstring. This is the headline path: the engine's body face (Latin Modern Roman), its math face
  * (Latin Modern Math) and the music fonts (Bravura/Petaluma) are all CFF, so faithful web output depends on it.
  *
  * The CFF container is a stack of INDEX structures and DICTs that locate the charstrings, the global and local
  * subroutines, and — for CID-keyed fonts — a per-glyph font-dictionary select. A Type-2 charstring is a tiny
  * stack machine whose operators trace the outline as relative moves, lines and Bézier curves; this interpreter
  * executes those into the shared cubic [[PathSeg]] outline, in raw design units ([[OtfFont.outline]] scales to em).
  *
  * Hints are consumed but ignored geometrically — they tune rasterisation at small sizes, which vector output
  * does not do — and the optional leading glyph width each charstring may carry is parsed past, not used.
  */
final class CffOutlines(data: Array[Byte], cffOffset: Int):

  // ─── INDEX and DICT primitives ────────────────────────────────────────────────

  /** A CFF INDEX: a count-prefixed array of variable-length blobs, addressed by 1-based offsets. */
  private final class Index(val offsets: Array[Int], val dataBase: Int):
    def count: Int                  = offsets.length - 1
    def range(i: Int): (Int, Int)   = (dataBase + offsets(i), dataBase + offsets(i + 1))

  private val emptyIndex = Index(Array(0), 0)

  private def readOffset(c: ByteCursor, offSize: Int): Int =
    var v = 0
    var k = 0
    while k < offSize do { v = (v << 8) | c.u8; k += 1 }
    v

  /** Parse an INDEX at `pos`, returning it and the byte position just past it. */
  private def readIndex(pos: Int): (Index, Int) =
    val c     = ByteCursor(data, pos)
    val count = c.u16
    if count == 0 then (emptyIndex, c.pos)
    else
      val offSize = c.u8
      val offsets = new Array[Int](count + 1)
      var i       = 0
      while i <= count do { offsets(i) = readOffset(c, offSize); i += 1 }
      val dataBase = c.pos - 1 // offsets are relative to the byte before the data
      (Index(offsets, dataBase), dataBase + offsets(count))

  /** Parse a DICT into operator → operands. Two-byte operators (escape `12 x`) are keyed as `1200 + x`. */
  private def parseDict(start: Int, end: Int): Map[Int, Vector[Double]] =
    val c        = ByteCursor(data, start)
    val operands = ArrayBuffer.empty[Double]
    val result   = scala.collection.mutable.Map.empty[Int, Vector[Double]]
    while c.pos < end do
      val b0 = c.u8
      if b0 <= 21 then
        val op = if b0 == 12 then 1200 + c.u8 else b0
        result(op) = operands.toVector
        operands.clear()
      else if b0 == 28 then operands += c.u16.toShort.toDouble
      else if b0 == 29 then operands += c.u32.toInt.toDouble
      else if b0 == 30 then operands += parseReal(c)
      else if b0 >= 32 && b0 <= 246 then operands += (b0 - 139).toDouble
      else if b0 >= 247 && b0 <= 250 then operands += ((b0 - 247) * 256 + c.u8 + 108).toDouble
      else if b0 >= 251 && b0 <= 254 then operands += (-(b0 - 251) * 256 - c.u8 - 108).toDouble
      // b0 == 31 / 255 are not valid DICT bytes; ignore defensively
    result.toMap

  /** A real operand is a string of 4-bit nibbles: digits, '.', exponent markers, sign, terminated by 0xf. */
  private def parseReal(c: ByteCursor): Double =
    val sb   = new StringBuilder
    var done = false
    while !done do
      val b  = c.u8
      val hi = (b >> 4) & 0xf
      val lo = b & 0xf
      for nib <- Seq(hi, lo) if !done do
        nib match
          case d if d <= 9 => sb.append(('0' + d).toChar)
          case 0xa         => sb.append('.')
          case 0xb         => sb.append('E')
          case 0xc         => sb.append("E-")
          case 0xe         => sb.append('-')
          case 0xf         => done = true
          case _           => () // 0xd reserved
    sb.toString.toDoubleOption.getOrElse(0.0)

  // ─── container layout ─────────────────────────────────────────────────────────

  private val header     = ByteCursor(data, cffOffset)
  private val _major     = header.u8
  private val _minor     = header.u8
  private val hdrSize    = header.u8
  private val _offSize   = header.u8

  private val (_nameIndex, afterName)     = readIndex(cffOffset + hdrSize)
  private val (topIndex, afterTop)        = readIndex(afterName)
  private val (_stringIndex, afterString) = readIndex(afterTop)
  private val (globalSubrs, _)            = readIndex(afterString)
  private val globalBias                  = subrBias(globalSubrs.count)

  private val top: Map[Int, Vector[Double]] =
    val (s, e) = topIndex.range(0)
    parseDict(s, e)

  private val charStrings: Index =
    readIndex(cffOffset + top(17).head.toInt)._1

  /** The number of glyphs equals the CharStrings count. */
  val numGlyphs: Int = charStrings.count

  private val isCID: Boolean = top.contains(1230) // ROS operator marks a CID-keyed font

  // Local subroutines depend on the glyph's font dictionary. A plain font has one set; a CID font selects per
  // glyph through FDSelect into FDArray. Each entry here is (local-subr INDEX, its bias).
  private val (fdSelect, fdLocals): (Array[Int], Array[(Index, Int)]) =
    if isCID then
      val fdArray = readIndex(cffOffset + top(1236).head.toInt)._1
      val locals = Array.tabulate(fdArray.count) { i =>
        val (s, e) = fdArray.range(i)
        privateLocalSubrs(parseDict(s, e))
      }
      (parseFdSelect(cffOffset + top(1237).head.toInt), locals)
    else
      (Array.empty[Int], Array(privateLocalSubrs(top)))

  /** Read a font/Top DICT's Private DICT and its Local Subr INDEX, returning the subrs and their bias. */
  private def privateLocalSubrs(dict: Map[Int, Vector[Double]]): (Index, Int) =
    dict.get(18) match
      case Some(Vector(size, off, _*)) =>
        val privOff = cffOffset + off.toInt
        val priv    = parseDict(privOff, privOff + size.toInt)
        priv.get(19) match
          case Some(rel +: _) =>
            val li = readIndex(privOff + rel.toInt)._1
            (li, subrBias(li.count))
          case _ => (emptyIndex, subrBias(0))
      case _ => (emptyIndex, subrBias(0))

  /** FDSelect maps each glyph to a font-dictionary index. Formats 0 (a flat array) and 3 (ranges) cover the
    * fonts the engine bundles. */
  private def parseFdSelect(off: Int): Array[Int] =
    val c   = ByteCursor(data, off)
    val out = new Array[Int](numGlyphs)
    c.u8 match
      case 0 =>
        var i = 0
        while i < numGlyphs do { out(i) = c.u8; i += 1 }
      case 3 =>
        val nRanges = c.u16
        var first   = c.u16
        for _ <- 0 until nRanges do
          val fd   = c.u8
          val next = c.u16
          var g    = first
          while g < next && g < numGlyphs do { out(g) = fd; g += 1 }
          first = next
      case _ => ()
    out

  private def localsFor(glyph: Int): (Index, Int) =
    if isCID then
      val fd = if glyph < fdSelect.length then fdSelect(glyph) else 0
      if fd < fdLocals.length then fdLocals(fd) else fdLocals(0)
    else fdLocals(0)

  private def subrBias(n: Int): Int = if n < 1240 then 107 else if n < 33900 then 1131 else 32768

  // ─── Type-2 charstring interpreter ────────────────────────────────────────────

  /** The outline of one glyph in design units, by executing its charstring. */
  def segments(glyph: Int): Vector[PathSeg] =
    if glyph < 0 || glyph >= numGlyphs then Vector.empty
    else new Interp(localsFor(glyph)).run(glyph)

  private final class Interp(locals: (Index, Int)):
    private val (localSubrs, localBias) = locals
    private val stack                   = ArrayBuffer.empty[Double]
    private val out                     = Vector.newBuilder[PathSeg]
    private var x                       = 0.0
    private var y                       = 0.0
    private var nStems                  = 0
    private var widthParsed             = false
    private var open                    = false
    private var stopped                 = false

    def run(glyph: Int): Vector[PathSeg] =
      val (s, e) = charStrings.range(glyph)
      exec(s, e, depth = 0)
      if open then out += PathSeg.Close
      out.result()

    private def closeOpen(): Unit =
      if open then out += PathSeg.Close
      open = false

    private def moveTo(nx: Double, ny: Double): Unit =
      closeOpen()
      x = nx; y = ny
      out += PathSeg.MoveTo(x, y)
      open = true

    private def lineTo(nx: Double, ny: Double): Unit =
      x = nx; y = ny
      out += PathSeg.LineTo(x, y)

    private def curveTo(c1x: Double, c1y: Double, c2x: Double, c2y: Double, nx: Double, ny: Double): Unit =
      out += PathSeg.CurveTo(c1x, c1y, c2x, c2y, nx, ny)
      x = nx; y = ny

    /** On the first stem/move/endchar operator, a leftover leading operand is the glyph's advance width. */
    private def takeWidth(expected: Int): Unit =
      if !widthParsed && stack.length > expected then stack.remove(0)
      widthParsed = true

    private def countStemsAndClear(): Unit =
      if !widthParsed && (stack.length % 2) == 1 then stack.remove(0)
      widthParsed = true
      nStems += stack.length / 2
      stack.clear()

    private def exec(start: Int, end: Int, depth: Int): Unit =
      if depth > 10 then return
      val c = ByteCursor(data, start)
      while c.pos < end && !stopped do
        val b0 = c.u8
        if b0 >= 32 || b0 == 28 then stack += readNumber(c, b0)
        else
          b0 match
            case 1 | 3 | 18 | 23 => countStemsAndClear() // hstem/vstem/hstemhm/vstemhm
            case 19 | 20 => // hintmask / cntrmask: pending stems then skip the mask bytes
              countStemsAndClear()
              c.skip((nStems + 7) / 8)
            case 21 => takeWidth(2); moveTo(x + stack(0), y + stack(1)); stack.clear()
            case 22 => takeWidth(1); moveTo(x + stack(0), y); stack.clear()
            case 4  => takeWidth(1); moveTo(x, y + stack(0)); stack.clear()
            case 5  => rlineto(); stack.clear()
            case 6  => alternatingLines(startHoriz = true); stack.clear()
            case 7  => alternatingLines(startHoriz = false); stack.clear()
            case 8  => rrcurveto(0); stack.clear()
            case 24 => rcurveline(); stack.clear()
            case 25 => rlinecurve(); stack.clear()
            case 26 => vvcurveto(); stack.clear()
            case 27 => hhcurveto(); stack.clear()
            case 30 => alternatingCurves(startHoriz = false); stack.clear()
            case 31 => alternatingCurves(startHoriz = true); stack.clear()
            case 10 => // callsubr
              val idx = stack.remove(stack.length - 1).toInt + localBias
              if idx >= 0 && idx < localSubrs.count then
                val (s, e) = localSubrs.range(idx); exec(s, e, depth + 1)
            case 29 => // callgsubr
              val idx = stack.remove(stack.length - 1).toInt + globalBias
              if idx >= 0 && idx < globalSubrs.count then
                val (s, e) = globalSubrs.range(idx); exec(s, e, depth + 1)
            case 11 => return // return from subr
            case 14 => // endchar. The deprecated seac form (endchar with adx/ady/bchar/achar operands, composing
              // an accented glyph from two standard-encoded glyphs) is not implemented: such a glyph renders
              // empty. No bundled CFF font contains one (verified across Latin Modern, Bravura and Petaluma);
              // supporting it would need the charset and the Standard Encoding table parsed.
              if !widthParsed && (stack.length == 1 || stack.length == 5) then stack.remove(0)
              widthParsed = true
              closeOpen()
              stopped = true
            case 12 =>
              c.u8 match
                case 34 => hflex(); stack.clear()
                case 35 => flex(); stack.clear()
                case 36 => hflex1(); stack.clear()
                case 37 => flex1(); stack.clear()
                case _  => stack.clear()
            case _ => stack.clear()

    /** Read a Type-2 numeric operand: small ints inline, 28 a signed short, 255 a 16.16 fixed. */
    private def readNumber(c: ByteCursor, b0: Int): Double =
      if b0 == 28 then c.u16.toShort.toDouble
      else if b0 < 247 then (b0 - 139).toDouble
      else if b0 < 251 then ((b0 - 247) * 256 + c.u8 + 108).toDouble
      else if b0 < 255 then (-(b0 - 251) * 256 - c.u8 - 108).toDouble
      else c.u32.toInt / 65536.0 // 255: 16.16 fixed-point

    private def rlineto(): Unit =
      var i = 0
      while i + 1 < stack.length do { lineTo(x + stack(i), y + stack(i + 1)); i += 2 }

    private def alternatingLines(startHoriz: Boolean): Unit =
      var horiz = startHoriz
      var i     = 0
      while i < stack.length do
        if horiz then lineTo(x + stack(i), y) else lineTo(x, y + stack(i))
        horiz = !horiz
        i += 1

    /** Consume groups of six relative curve deltas starting at `from`; returns the index past the last group. */
    private def rrcurveto(from: Int): Int =
      var i = from
      while i + 5 < stack.length do
        val c1x = x + stack(i);       val c1y = y + stack(i + 1)
        val c2x = c1x + stack(i + 2); val c2y = c1y + stack(i + 3)
        val ex  = c2x + stack(i + 4); val ey  = c2y + stack(i + 5)
        curveTo(c1x, c1y, c2x, c2y, ex, ey)
        i += 6
      i

    private def rcurveline(): Unit =
      val i = rrcurveto(0)
      if i + 1 < stack.length then lineTo(x + stack(i), y + stack(i + 1))

    private def rlinecurve(): Unit =
      var i = 0
      while stack.length - i > 6 do { lineTo(x + stack(i), y + stack(i + 1)); i += 2 }
      rrcurveto(i)

    private def vvcurveto(): Unit =
      var i   = 0
      var dx1 = 0.0
      if (stack.length % 4) == 1 then { dx1 = stack(0); i = 1 }
      while i + 3 < stack.length do
        val c1x = x + dx1;            val c1y = y + stack(i)
        val c2x = c1x + stack(i + 1); val c2y = c1y + stack(i + 2)
        val ex  = c2x;               val ey  = c2y + stack(i + 3)
        curveTo(c1x, c1y, c2x, c2y, ex, ey)
        dx1 = 0.0
        i += 4

    private def hhcurveto(): Unit =
      var i   = 0
      var dy1 = 0.0
      if (stack.length % 4) == 1 then { dy1 = stack(0); i = 1 }
      while i + 3 < stack.length do
        val c1x = x + stack(i);      val c1y = y + dy1
        val c2x = c1x + stack(i + 1); val c2y = c1y + stack(i + 2)
        val ex  = c2x + stack(i + 3); val ey  = c2y
        curveTo(c1x, c1y, c2x, c2y, ex, ey)
        dy1 = 0.0
        i += 4

    /** vhcurveto / hvcurveto: curves whose tangents alternate vertical/horizontal; a lone trailing operand on
      * the final curve is the otherwise-implied second coordinate of its endpoint. */
    private def alternatingCurves(startHoriz: Boolean): Unit =
      val n     = stack.length
      var i     = 0
      var horiz = startHoriz
      while i + 4 <= n do
        val last = (n - i) == 5
        if horiz then
          val c1x = x + stack(i);       val c1y = y
          val c2x = c1x + stack(i + 1); val c2y = c1y + stack(i + 2)
          val ey  = c2y + stack(i + 3)
          val ex  = if last then c2x + stack(i + 4) else c2x
          curveTo(c1x, c1y, c2x, c2y, ex, ey)
        else
          val c1x = x;                  val c1y = y + stack(i)
          val c2x = c1x + stack(i + 1); val c2y = c1y + stack(i + 2)
          val ex  = c2x + stack(i + 3)
          val ey  = if last then c2y + stack(i + 4) else c2y
          curveTo(c1x, c1y, c2x, c2y, ex, ey)
        horiz = !horiz
        i += 4

    private def flex(): Unit =
      val c1x = x + stack(0);       val c1y = y + stack(1)
      val c2x = c1x + stack(2);     val c2y = c1y + stack(3)
      val jx  = c2x + stack(4);     val jy  = c2y + stack(5)
      curveTo(c1x, c1y, c2x, c2y, jx, jy)
      val d1x = jx + stack(6);      val d1y = jy + stack(7)
      val d2x = d1x + stack(8);     val d2y = d1y + stack(9)
      val ex  = d2x + stack(10);    val ey  = d2y + stack(11)
      curveTo(d1x, d1y, d2x, d2y, ex, ey)

    private def hflex(): Unit =
      val startY = y
      val c1x = x + stack(0);       val c1y = y
      val c2x = c1x + stack(1);     val c2y = c1y + stack(2)
      val jx  = c2x + stack(3);     val jy  = c2y
      curveTo(c1x, c1y, c2x, c2y, jx, jy)
      val d1x = jx + stack(4);      val d1y = jy
      val d2x = d1x + stack(5);     val d2y = startY
      val ex  = d2x + stack(6);     val ey  = startY
      curveTo(d1x, d1y, d2x, d2y, ex, ey)

    private def hflex1(): Unit =
      val startY = y
      val c1x = x + stack(0);       val c1y = y + stack(1)
      val c2x = c1x + stack(2);     val c2y = c1y + stack(3)
      val jx  = c2x + stack(4);     val jy  = c2y
      curveTo(c1x, c1y, c2x, c2y, jx, jy)
      val d1x = jx + stack(5);      val d1y = jy
      val d2x = d1x + stack(6);     val d2y = d1y + stack(7)
      val ex  = d2x + stack(8);     val ey  = startY
      curveTo(d1x, d1y, d2x, d2y, ex, ey)

    private def flex1(): Unit =
      val startX = x; val startY = y
      val c1x = x + stack(0);       val c1y = y + stack(1)
      val c2x = c1x + stack(2);     val c2y = c1y + stack(3)
      val jx  = c2x + stack(4);     val jy  = c2y + stack(5)
      curveTo(c1x, c1y, c2x, c2y, jx, jy)
      val d1x = jx + stack(6);      val d1y = jy + stack(7)
      val d2x = d1x + stack(8);     val d2y = d1y + stack(9)
      val dxSum = stack(0) + stack(2) + stack(4) + stack(6) + stack(8)
      val dySum = stack(1) + stack(3) + stack(5) + stack(7) + stack(9)
      val (ex, ey) =
        if math.abs(dxSum) > math.abs(dySum) then (d2x + stack(10), startY)
        else (startX, d2y + stack(10))
      curveTo(d1x, d1y, d2x, d2y, ex, ey)
