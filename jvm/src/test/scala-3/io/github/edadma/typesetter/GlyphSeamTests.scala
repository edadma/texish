package io.github.edadma.typesetter

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.awt.image.BufferedImage

/** Stage 0 against a real font on the Graphics2D backend: proves the glyph seam produces sane metrics and
  * agrees with the string seam (the precision question — both paths measure the same font, so a single
  * glyph and the corresponding single character must come out the same width).
  */
class GlyphSeamTests extends AnyFreeSpec with Matchers:

  "glyphIndex maps a codepoint to a real glyph" in {
    val t  = new Graphics2DTypesetter(dpi = 72)
    val rf = t.currentFont.renderFont.asInstanceOf[t.RenderFont]

    t.glyphIndex(rf, 'A'.toInt) should be > 0
  }

  "the glyph seam's advance agrees with the string seam's advance for the same character" in {
    val t  = new Graphics2DTypesetter(dpi = 72)
    val rf = t.currentFont.renderFont.asInstanceOf[t.RenderFont]

    val ge         = t.glyphExtents(rf, t.glyphIndex(rf, 'A'.toInt))
    val strAdvance = t.getTextExtents("A", rf).xAdvance

    ge.xAdvance should be > 0.0
    ge.width should be > 0.0
    ge.xAdvance shouldBe strAdvance +- 0.5
  }

  "a GlyphBox from a descender glyph has positive ascent and descent and draws ink" in {
    val t  = new Graphics2DTypesetter(dpi = 100)
    val rf = t.currentFont.renderFont.asInstanceOf[t.RenderFont]
    val g  = t.glyphIndex(rf, 'g'.toInt) // 'g' descends below the baseline

    val box = new GlyphBox(t, g)
    box.width should be > 0.0
    box.ascent should be > 0.0
    box.descent should be > 0.0

    Console.withOut(new java.io.ByteArrayOutputStream) {
      t.add(box)
      t.end()
    }

    val img = t.getDocument.printedPages.head.asInstanceOf[BufferedImage]
    val px  = img.getRGB(0, 0, img.getWidth, img.getHeight, null, 0, img.getWidth)

    // the page is filled white; a drawn glyph leaves dark ink somewhere
    px.exists { p =>
      val r = (p >> 16) & 0xff
      val gg = (p >> 8) & 0xff
      val b = p & 0xff
      (r + gg + b) / 3 < 128
    } shouldBe true
  }
