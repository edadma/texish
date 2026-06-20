package io.github.edadma.texish.opentype

/** Reads glyph outlines from a TrueType font's `glyf` table, using `loca` to find each glyph's bytes. A simple
  * glyph is a set of closed contours of on- and off-curve points describing quadratic B-splines; a composite
  * glyph references other glyphs under affine transforms. Both resolve to the shared cubic [[PathSeg]] outline.
  *
  * Everything here works in raw design units (the font's integer grid); [[OtfFont.outline]] scales the result to
  * em. Working in design units keeps composite transforms exact, since their offsets are design-unit values and
  * their scales are unitless multipliers — composing children before the single final scale avoids rounding drift.
  */
final class TrueTypeOutlines(
    data: Array[Byte],
    glyfOffset: Int,
    locaOffset: Int,
    indexToLocFormat: Int,
    numGlyphs: Int,
):

  /** The byte span of one glyph in `glyf`, via `loca`. A zero-length span means the glyph has no outline (a
    * space, say), so the two offsets are equal and the caller emits nothing. */
  private def glyphSpan(glyph: Int): Option[(Int, Int)] =
    if glyph < 0 || glyph >= numGlyphs then None
    else
      val (start, end) =
        if indexToLocFormat == 0 then
          // Short loca stores offsets divided by two.
          val c = ByteCursor(data, locaOffset + glyph * 2)
          (c.u16 * 2, c.u16 * 2)
        else
          val c = ByteCursor(data, locaOffset + glyph * 4)
          (c.u32.toInt, c.u32.toInt)
      if end <= start then None else Some((glyfOffset + start, glyfOffset + end))

  /** The outline of a glyph in design units, resolving composite components recursively. */
  def segments(glyph: Int): Vector[PathSeg] = segments(glyph, depth = 0)

  private def segments(glyph: Int, depth: Int): Vector[PathSeg] =
    if depth > 8 then Vector.empty // guard against malformed self-referential composites
    else
      glyphSpan(glyph) match
        case None => Vector.empty
        case Some((off, _)) =>
          val c               = ByteCursor(data, off)
          val numberOfContours = c.i16
          c.skip(8) // xMin, yMin, xMax, yMax — recomputed from the outline if ever needed
          if numberOfContours >= 0 then simpleGlyph(c, numberOfContours)
          else compositeGlyph(c, depth)

  /** A simple glyph: contour end-point indices, then flags and delta-encoded x then y coordinates. */
  private def simpleGlyph(c: ByteCursor, numberOfContours: Int): Vector[PathSeg] =
    if numberOfContours == 0 then Vector.empty
    else
      val endPts = Array.tabulate(numberOfContours)(_ => c.u16)
      val numPoints = endPts(numberOfContours - 1) + 1
      val instrLen = c.u16
      c.skip(instrLen) // hinting bytecode — irrelevant to the outline

      // Flags expand a run-length encoding: a flag with REPEAT set is followed by a repeat count.
      val flags = new Array[Int](numPoints)
      var i     = 0
      while i < numPoints do
        val f = c.u8
        flags(i) = f
        i += 1
        if (f & 0x08) != 0 then // REPEAT_FLAG
          var rep = c.u8
          while rep > 0 && i < numPoints do
            flags(i) = f
            i += 1
            rep -= 1

      // X then Y are stored as deltas; the flag bits select 0 / signed-byte / signed-short per coordinate.
      val xs = new Array[Int](numPoints)
      var x  = 0
      i = 0
      while i < numPoints do
        val f = flags(i)
        if (f & 0x02) != 0 then // X_SHORT_VECTOR: one unsigned byte, sign from the SAME/POSITIVE bit
          val dx = c.u8
          x += (if (f & 0x10) != 0 then dx else -dx)
        else if (f & 0x10) == 0 then // not short and not "same": a signed 16-bit delta
          x += c.i16
        // else: short clear and same-bit set ⇒ delta 0
        xs(i) = x
        i += 1

      val ys = new Array[Int](numPoints)
      var y  = 0
      i = 0
      while i < numPoints do
        val f = flags(i)
        if (f & 0x04) != 0 then // Y_SHORT_VECTOR
          val dy = c.u8
          y += (if (f & 0x20) != 0 then dy else -dy)
        else if (f & 0x20) == 0 then y += c.i16
        ys(i) = y
        i += 1

      val out   = Vector.newBuilder[PathSeg]
      var start = 0
      for ci <- 0 until numberOfContours do
        val end = endPts(ci)
        val pts = (start to end).map(j => (xs(j).toDouble, ys(j).toDouble, (flags(j) & 0x01) != 0)).toArray
        out ++= contour(pts)
        start = end + 1
      out.result()

  /** Convert one closed TrueType contour to cubic segments. The contour is a ring of on- and off-curve points;
    * consecutive off-curve points imply an on-curve point at their midpoint, and each off-curve point is the
    * control of a quadratic from the previous on-curve point to the next on-curve point. */
  private def contour(pts: Array[(Double, Double, Boolean)]): Vector[PathSeg] =
    val n = pts.length
    if n == 0 then return Vector.empty

    // Re-anchor the ring so it begins at an on-curve point. With no on-curve point at all, synthesise a start at
    // the midpoint of the first and last points and treat every stored point as an off-curve control after it.
    val onIdx = pts.indexWhere(_._3)
    val (startX, startY, ring) =
      if onIdx >= 0 then
        val rot = Array.tabulate(n)(j => pts((onIdx + j) % n))
        (rot(0)._1, rot(0)._2, rot.drop(1))
      else
        val mx = (pts(0)._1 + pts(n - 1)._1) / 2
        val my = (pts(0)._2 + pts(n - 1)._2) / 2
        (mx, my, pts)

    val out = Vector.newBuilder[PathSeg]
    out += PathSeg.MoveTo(startX, startY)

    var curX    = startX
    var curY    = startY
    var ctrl: Option[(Double, Double)] = None

    def quad(cx: Double, cy: Double, ex: Double, ey: Double): Unit =
      // Elevate the quadratic (cur, ctrl, end) to a cubic so the outline has a single curve kind.
      val c1x = curX + 2.0 / 3.0 * (cx - curX)
      val c1y = curY + 2.0 / 3.0 * (cy - curY)
      val c2x = ex + 2.0 / 3.0 * (cx - ex)
      val c2y = ey + 2.0 / 3.0 * (cy - ey)
      out += PathSeg.CurveTo(c1x, c1y, c2x, c2y, ex, ey)
      curX = ex; curY = ey

    // Walk the points after the start, finishing by wrapping back to the start as the closing on-curve point.
    val walk = ring :+ ((startX, startY, true))
    for (px, py, on) <- walk do
      if on then
        ctrl match
          case None =>
            out += PathSeg.LineTo(px, py); curX = px; curY = py
          case Some((cx, cy)) =>
            quad(cx, cy, px, py); ctrl = None
      else
        ctrl match
          case None => ctrl = Some((px, py))
          case Some((cx, cy)) =>
            val mx = (cx + px) / 2
            val my = (cy + py) / 2
            quad(cx, cy, mx, my)
            ctrl = Some((px, py))

    out += PathSeg.Close
    out.result()

  /** A composite glyph: a chain of component references, each a child glyph placed under a 2×2 scale plus an
    * offset. Only the common XY-offset form is handled; point-matching args (rare, used for diacritics keyed to
    * anchor points) are read past and the child is placed at the origin. */
  private def compositeGlyph(c: ByteCursor, depth: Int): Vector[PathSeg] =
    val out  = Vector.newBuilder[PathSeg]
    var more = true
    while more do
      val flags      = c.u16
      val childGlyph = c.u16

      val argsAreWords = (flags & 0x0001) != 0
      val argsAreXY    = (flags & 0x0002) != 0
      val (arg1, arg2) =
        if argsAreWords then (c.i16, c.i16) else (signedByte(c.u8), signedByte(c.u8))
      val (dx, dy) = if argsAreXY then (arg1.toDouble, arg2.toDouble) else (0.0, 0.0)

      var a = 1.0; var b = 0.0; var cc = 0.0; var d = 1.0
      if (flags & 0x0008) != 0 then // WE_HAVE_A_SCALE
        val s = f2dot14(c.u16); a = s; d = s
      else if (flags & 0x0040) != 0 then // WE_HAVE_AN_X_AND_Y_SCALE
        a = f2dot14(c.u16); d = f2dot14(c.u16)
      else if (flags & 0x0080) != 0 then // WE_HAVE_A_TWO_BY_TWO
        a = f2dot14(c.u16); b = f2dot14(c.u16); cc = f2dot14(c.u16); d = f2dot14(c.u16)

      // Place the child's outline under [[a b; c d]] then the offset; transform applies to every control point.
      for seg <- segments(childGlyph, depth + 1) do
        out += transform(seg, a, b, cc, d, dx, dy)

      more = (flags & 0x0020) != 0 // MORE_COMPONENTS

    out.result()

  private def transform(seg: PathSeg, a: Double, b: Double, c: Double, d: Double, dx: Double, dy: Double): PathSeg =
    def tx(x: Double, y: Double): (Double, Double) = (a * x + c * y + dx, b * x + d * y + dy)
    seg match
      case PathSeg.MoveTo(x, y)   => val (nx, ny) = tx(x, y); PathSeg.MoveTo(nx, ny)
      case PathSeg.LineTo(x, y)   => val (nx, ny) = tx(x, y); PathSeg.LineTo(nx, ny)
      case PathSeg.CurveTo(x1, y1, x2, y2, x, y) =>
        val (a1, b1) = tx(x1, y1); val (a2, b2) = tx(x2, y2); val (a3, b3) = tx(x, y)
        PathSeg.CurveTo(a1, b1, a2, b2, a3, b3)
      case PathSeg.Close => PathSeg.Close

  private def signedByte(u: Int): Int = u.toByte.toInt
  private def f2dot14(u: Int): Double = u.toShort.toDouble / 16384.0
