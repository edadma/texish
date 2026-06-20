package io.github.edadma.texish

import java.awt.Font as JFont
import java.awt.font.FontRenderContext
import java.awt.geom.{AffineTransform, PathIterator}
import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{OtfFont, PathSeg}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The CFF Type-2 charstring interpreter on [[OtfFont]] is checked against AWT, which reads the same `.otf` and
  * traces the same charstrings independently. Glyphs are addressed by glyph index (not codepoint), so the
  * comparison tests outline extraction directly across all three CFF flavours the engine ships: Latin Modern
  * Roman (the body face), Latin Modern Math, and Bravura (a SMuFL music font). Flattened bounding boxes are
  * compared, blind to how each side represents curves.
  */
class CffOutlineTests extends AnyFreeSpec with Matchers:

  private val emSize = 1000.0
  private val frc    = new FontRenderContext(new AffineTransform, true, true)

  private case class Box(minX: Double, minY: Double, maxX: Double, maxY: Double, empty: Boolean)

  private def load(path: String): (OtfFont, JFont) =
    (new OtfFont(Files.readAllBytes(Paths.get(path))), JFont.createFont(JFont.TRUETYPE_FONT, Paths.get(path).toFile))

  /** Our outline's em-space bounding box, sampling each cubic to a polyline. */
  private def myBox(segs: Vector[PathSeg]): Box =
    if segs.isEmpty then return Box(0, 0, 0, 0, empty = true)
    var minX = Double.MaxValue; var minY = Double.MaxValue
    var maxX = Double.MinValue; var maxY = Double.MinValue
    var cx   = 0.0; var cy = 0.0
    def see(x: Double, y: Double): Unit =
      minX = math.min(minX, x); minY = math.min(minY, y)
      maxX = math.max(maxX, x); maxY = math.max(maxY, y)
    for seg <- segs do
      seg match
        case PathSeg.MoveTo(x, y) => cx = x; cy = y; see(x, y)
        case PathSeg.LineTo(x, y) => cx = x; cy = y; see(x, y)
        case PathSeg.CurveTo(x1, y1, x2, y2, x, y) =>
          val (sx, sy) = (cx, cy)
          for i <- 1 to 16 do
            val t = i / 16.0; val u = 1 - t
            see(
              u * u * u * sx + 3 * u * u * t * x1 + 3 * u * t * t * x2 + t * t * t * x,
              u * u * u * sy + 3 * u * u * t * y1 + 3 * u * t * t * y2 + t * t * t * y,
            )
          cx = x; cy = y
        case PathSeg.Close => ()
    Box(minX, minY, maxX, maxY, empty = false)

  /** AWT's outline for a glyph index, flattened to line segments, in em (device units / emSize, y up). */
  private def awtBox(font: JFont, gid: Int): Box =
    val gv = font.deriveFont(emSize.toFloat).createGlyphVector(frc, Array(gid))
    val it = gv.getGlyphOutline(0).getPathIterator(null, 0.01)
    val coords = new Array[Double](6)
    var any  = false
    var minX = Double.MaxValue; var minY = Double.MaxValue
    var maxX = Double.MinValue; var maxY = Double.MinValue
    def see(x: Double, y: Double): Unit =
      any = true
      val ex = x / emSize; val ey = -y / emSize
      minX = math.min(minX, ex); minY = math.min(minY, ey)
      maxX = math.max(maxX, ex); maxY = math.max(maxY, ey)
    while !it.isDone do
      it.currentSegment(coords) match
        case PathIterator.SEG_MOVETO | PathIterator.SEG_LINETO => see(coords(0), coords(1))
        case _                                                 => ()
      it.next()
    if any then Box(minX, minY, maxX, maxY, empty = false) else Box(0, 0, 0, 0, empty = true)

  /** Walk a span of glyph indices, comparing our outline bbox to AWT's; require a healthy count of real matches. */
  private def compareGids(path: String, gids: Range): Unit =
    val (otf, awt) = load(path)
    var matched    = 0
    for g <- gids if g < otf.numGlyphs do
      val mine = myBox(otf.outline(g))
      val ref  = awtBox(awt, g)
      withClue(s"$path gid $g: mine=$mine awt=$ref — ") {
        mine.empty shouldBe ref.empty
        if !mine.empty then
          math.abs(mine.minX - ref.minX) should be < 0.02
          math.abs(mine.minY - ref.minY) should be < 0.02
          math.abs(mine.maxX - ref.maxX) should be < 0.02
          math.abs(mine.maxY - ref.maxY) should be < 0.02
          matched += 1
      }
    withClue(s"$path: too few non-empty glyphs compared — ") { matched should be > (gids.length / 3) }

  "Latin Modern Roman (CFF body face) outlines match AWT across a span of glyphs" in {
    compareGids("fonts/LatinModernRoman/lmroman10-regular.otf", 1 to 120)
  }

  "Latin Modern Math (CFF math face) outlines match AWT across a span of glyphs" in {
    compareGids("fonts/LatinModernMath/LatinModernMath-Regular.otf", 1 to 200)
  }

  "Bravura (SMuFL CFF music font) outlines match AWT across a span of glyphs" in {
    compareGids("fonts/Bravura/Bravura.otf", 1 to 200)
  }

  "a known letter's outline is a non-empty, closed path" in {
    val (otf, _) = load("fonts/LatinModernRoman/lmroman10-regular.otf")
    val segs     = otf.outline(otf.glyphIndex('g'.toInt))
    segs should not be empty
    segs.head shouldBe a[PathSeg.MoveTo]
    segs.last shouldBe PathSeg.Close
  }
