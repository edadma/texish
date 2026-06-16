package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 0 of math support: the additive glyph seam and the box that rides on it. These run on the
  * fixed-metric [[HeadlessTypesetter]], so they exercise the engine wiring (glyphIndex → GlyphBox metrics →
  * drawGlyph) without needing a font or a rendering backend.
  */
class GlyphBoxTests extends AnyFreeSpec with Matchers:

  "a GlyphBox reports the glyph's metrics from the glyph seam, not the string seam" in {
    val t  = new HeadlessTypesetter
    val rf = t.currentFont.renderFont.asInstanceOf[t.RenderFont]

    val box = new GlyphBox(t, t.glyphIndex(rf, 'A'.toInt))

    box.width shouldBe 6.0
    box.xAdvance shouldBe 6.0
    box.ascent shouldBe 8.0    // -yBearing
    box.descent shouldBe 2.0   // height - ascent
    box.height shouldBe 10.0
  }

  "drawing a GlyphBox emits exactly one drawGlyph at the box origin" in {
    val rec = new RecordingGlyphTypesetter
    val rf  = rec.currentFont.renderFont.asInstanceOf[rec.RenderFont]
    val g   = rec.glyphIndex(rf, 'M'.toInt)

    new GlyphBox(rec, g).draw(rec, 12.0, 34.0)

    rec.drawn shouldBe List((g, 12.0, 34.0))
  }

  "the glyph seam leaves the string seam untouched — a CharBox still draws as a string" in {
    val rec = new RecordingGlyphTypesetter
    new CharBox(rec, "hi").draw(rec, 1.0, 2.0)

    rec.drawn shouldBe empty                 // no glyph drawing on the string path
    rec.strings shouldBe List(("hi", 1.0, 2.0))
  }

/** A HeadlessTypesetter that records what gets placed through each seam, so a test can assert which path a
  * box used and where it drew.
  */
class RecordingGlyphTypesetter extends HeadlessTypesetter:
  var drawn: List[(Int, Double, Double)]      = Nil
  var strings: List[(String, Double, Double)] = Nil

  override def drawGlyph(font: RenderFont, glyph: Int, x: Double, y: Double): Unit =
    drawn = drawn :+ (glyph, x, y)

  override def drawString(text: String, x: Double, y: Double): Unit =
    strings = strings :+ (text, x, y)
