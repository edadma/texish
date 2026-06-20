package io.github.edadma.texish

import java.awt.Font as JFont
import java.awt.font.FontRenderContext
import java.awt.geom.{AffineTransform, PathIterator}
import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{OtfFont, PathSeg}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The TrueType (`glyf`) outline reader on [[OtfFont]] is checked against AWT, which reads the same `.ttf` and
  * produces the same glyph outlines through an independent code path. Comparing flattened bounding boxes — both
  * sides reduced to polylines so the comparison is blind to how each represents curves — confirms the reader
  * extracts the right contours, scaled correctly to em, for simple and composite (accented) glyphs alike.
  */
class GlyfOutlineTests extends AnyFreeSpec with Matchers:

  private val path = "fonts/NotoSerif/NotoSerif-Regular.ttf"
  private val noto = new OtfFont(Files.readAllBytes(Paths.get(path)))
  private val awt  = JFont.createFont(JFont.TRUETYPE_FONT, Paths.get(path).toFile)

  private val emSize = 1000.0 // a big point size so AWT's device units divide back to em cleanly
  private val frc  = new FontRenderContext(new AffineTransform, true, true)

  private case class Box(minX: Double, minY: Double, maxX: Double, maxY: Double)

  /** Flatten our outline by sampling each cubic, returning the em-space bounding box of the sampled points. */
  private def myBox(segs: Vector[PathSeg]): Box =
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
            val bx = u * u * u * sx + 3 * u * u * t * x1 + 3 * u * t * t * x2 + t * t * t * x
            val by = u * u * u * sy + 3 * u * u * t * y1 + 3 * u * t * t * y2 + t * t * t * y
            see(bx, by)
          cx = x; cy = y
        case PathSeg.Close => ()
    Box(minX, minY, maxX, maxY)

  /** AWT's outline for the same character, flattened to line segments, in em (device units / emSize, y up). */
  private def awtBox(ch: Char): Box =
    val sized = awt.deriveFont(emSize.toFloat)
    val gv    = sized.createGlyphVector(frc, ch.toString)
    val it    = gv.getGlyphOutline(0).getPathIterator(null, 0.01)
    val coords = new Array[Double](6)
    var minX = Double.MaxValue; var minY = Double.MaxValue
    var maxX = Double.MinValue; var maxY = Double.MinValue
    def see(x: Double, y: Double): Unit =
      val ex = x / emSize; val ey = -y / emSize // AWT is y-down; the font grid is y-up
      minX = math.min(minX, ex); minY = math.min(minY, ey)
      maxX = math.max(maxX, ex); maxY = math.max(maxY, ey)
    while !it.isDone do
      it.currentSegment(coords) match
        case PathIterator.SEG_MOVETO | PathIterator.SEG_LINETO => see(coords(0), coords(1))
        case _                                                 => ()
      it.next()
    Box(minX, minY, maxX, maxY)

  private def assertBoxClose(ch: Char): Unit =
    val mine = myBox(noto.outline(noto.glyphIndex(ch.toInt)))
    val ref  = awtBox(ch)
    withClue(s"char '$ch': mine=$mine awt=$ref — ") {
      math.abs(mine.minX - ref.minX) should be < 0.01
      math.abs(mine.minY - ref.minY) should be < 0.01
      math.abs(mine.maxX - ref.maxX) should be < 0.01
      math.abs(mine.maxY - ref.maxY) should be < 0.01
    }

  "a simple glyph's outline is non-empty, starts with a move, and is closed" in {
    val segs = noto.outline(noto.glyphIndex('I'.toInt))
    segs should not be empty
    segs.head shouldBe a[PathSeg.MoveTo]
    segs.last shouldBe PathSeg.Close
  }

  "a glyph with no outline (the space) yields no segments" in {
    noto.outline(noto.glyphIndex(' '.toInt)) shouldBe empty
  }

  "straight-edged glyph bounding boxes match AWT" in {
    for ch <- "ILHTEF".toCharArray do assertBoxClose(ch)
  }

  "curved glyph bounding boxes match AWT" in {
    for ch <- "OoesGc".toCharArray do assertBoxClose(ch)
  }

  "composite (accented) glyph bounding boxes match AWT" in {
    // é, à, ñ, ç are built as composites: a base letter plus a referenced diacritic glyph.
    for ch <- "éàñç".toCharArray do assertBoxClose(ch)
  }
