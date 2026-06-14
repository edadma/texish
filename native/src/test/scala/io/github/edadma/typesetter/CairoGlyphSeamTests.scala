package io.github.edadma.typesetter

import io.github.edadma.libcairo.Surface
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 0 against Cairo at runtime: proves the native glyph seam works end to end — the retained FreeType
  * face maps a codepoint to a glyph index, `cairo_glyph_extents` measures it (agreeing with the string seam
  * on the same font), and `cairo_show_glyphs` draws ink for it.
  */
class CairoGlyphSeamTests extends AnyFreeSpec with Matchers:

  private def inkPixels(s: Surface): Int =
    s.flush()

    val data   = s.getData
    val stride = s.getStride
    val w      = s.getWidth
    val h      = s.getHeight
    var count  = 0
    var y      = 0

    while y < h do
      var x = 0

      while x < w do
        val p = data + y.toLong * stride + x.toLong * 4

        if !((p(0) & 0xff) == 0xff && (p(1) & 0xff) == 0xff && (p(2) & 0xff) == 0xff && (p(3) & 0xff) == 0xff) then
          count += 1
        x += 1
      y += 1
    count

  "the Cairo glyph seam maps, measures, and draws a glyph, agreeing with the string seam" in {
    val t  = new CairoImageTypesetter(100)
    val rf = t.currentFont.renderFont.asInstanceOf[t.RenderFont]

    val g = t.glyphIndex(rf, 'A'.toInt)
    g should be > 0

    val ge = t.glyphExtents(rf, g)
    ge.xAdvance should be > 0.0
    ge.width should be > 0.0
    ge.xAdvance shouldBe t.getTextExtents("A", rf).xAdvance +- 0.5

    t.add(new GlyphBox(t, g))
    t.end()

    inkPixels(t.getDocument.printedPages.head.asInstanceOf[Surface]) should be > 0
  }
