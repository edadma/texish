package io.github.edadma.texish

import io.github.edadma.qr.{Ecc, QrCode}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The QR box: a square of filled cells drawn from a matrix produced by the standalone qr library. Its metrics are
  * fixed by the module count plus the quiet zone, and its draw paints a light background followed by one filled
  * cell per dark module. A [[HeadlessTypesetter]] subclass records the rectangles so the drawing can be checked
  * without a backend.
  */
class QrBoxTests extends AnyFreeSpec with Matchers:

  private class RecordingTypesetter extends HeadlessTypesetter:
    val rects = ArrayBuffer[(Double, Double, Double, Double)]()
    override def fillRect(x: Double, y: Double, w: Double, h: Double): Unit = rects += ((x, y, w, h))

  "the box side is (module count + 2*quiet) * cell, all ascent" in {
    val qr  = QrCode.encodeText("HELLO123", Ecc.Quartile)
    val box = new QrBox(qr, cell = 2.0, quiet = 4, dark = Color("black"), light = Color("white"))
    val side = (qr.size + 8) * 2.0
    box.width shouldBe side
    box.height shouldBe side
    box.ascent shouldBe side
    box.descent shouldBe 0.0
    box.xAdvance shouldBe side
  }

  "draw paints a background plus one cell per dark module" in {
    val qr  = QrCode.encodeText("HELLO123", Ecc.Quartile)
    val box = new QrBox(qr, cell = 2.0, quiet = 4, dark = Color("black"), light = Color("white"))
    val t   = new RecordingTypesetter
    box.draw(t, 0, box.ascent) // baseline at the bottom, so the box top-left lands at (0,0)

    val darkModules =
      (for y <- 0 until qr.size; x <- 0 until qr.size if qr(x, y) yield 1).sum
    t.rects.length shouldBe darkModules + 1 // + the background rectangle

    // The background is the full square at the origin.
    t.rects.head shouldBe ((0.0, 0.0, box.width, box.height))

    // Every cell is one module square, inset by the quiet zone, and lands inside the box.
    for (x, y, w, h) <- t.rects.tail do
      w shouldBe 2.0
      h shouldBe 2.0
      x should be >= 8.0
      y should be >= 8.0
      (x + w) should be <= box.width - 8.0 + 0.001
  }

  "a transparent light colour skips the background rectangle" in {
    val qr  = QrCode.encodeText("HELLO123", Ecc.Quartile)
    val box = new QrBox(qr, cell = 2.0, quiet = 4, dark = Color("black"), light = Color.TRANSPARENT)
    val t   = new RecordingTypesetter
    box.draw(t, 0, box.ascent)

    val darkModules =
      (for y <- 0 until qr.size; x <- 0 until qr.size if qr(x, y) yield 1).sum
    t.rects.length shouldBe darkModules // no background
  }
