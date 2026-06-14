package io.github.edadma.typesetter

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 2 against the real Latin Modern Math font on the Graphics2D backend: proves the bundled math font
  * loads, its OpenType MATH table reaches the parser through the SFNT seam, and the math-italic/symbol
  * codepoints the symbol tables use actually resolve to glyphs in the font (rather than .notdef).
  */
class MathFontTests extends AnyFreeSpec with Matchers:

  def lmmath(t: Graphics2DTypesetter): Font = t.makeFont("lmmath", 12, Set.empty)

  "the bundled math font carries a MATH table that the engine parses" in {
    val t    = new Graphics2DTypesetter(dpi = 72)
    val math = t.mathTableFor(lmmath(t))

    math shouldBe defined
    math.get.unitsPerEm shouldBe 1000
    math.get.constants.axisHeight shouldBe (0.25 +- 0.001)               // 250/1000 em
    math.get.constants.fractionRuleThickness should be > 0.0
  }

  "MathFont scales the axis height by the font size" in {
    val t  = new Graphics2DTypesetter(dpi = 72)
    val mf = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))

    mf.axisHeight shouldBe (0.25 * 12 +- 0.01)
  }

  "the math-italic and symbol codepoints resolve to real glyphs in the math font" in {
    val t  = new Graphics2DTypesetter(dpi = 72)
    val mf = new MathFont(t, lmmath(t), None)

    mf.glyphIndex(0x1D6FC) should not be 0 // mathematical italic alpha
    mf.glyphIndex(0x1D44E) should not be 0 // mathematical italic a
    mf.glyphIndex(0x2264)  should not be 0 // ≤
    mf.glyphIndex(0x2211)  should not be 0 // ∑
  }

  "a glyph box is as wide as the advance, not the ink box, so glyphs don't crowd" in {
    val t  = new Graphics2DTypesetter(dpi = 72)
    val mf = new MathFont(t, lmmath(t), None)
    val rf = mf.font.renderFont.asInstanceOf[t.RenderFont]
    val e  = t.glyphExtents(rf, mf.glyphIndex(0x2B)) // '+', which carries wide side bearings in a math font

    e.xAdvance should be > e.width             // precondition: advance genuinely exceeds the ink width here
    mf.glyphBox(0x2B).width shouldBe e.xAdvance // the box advances by the font's advance, not the ink box
  }

  "a real math atom has positive, sensible extents" in {
    val t  = new Graphics2DTypesetter(dpi = 72)
    val mf = new MathFont(t, lmmath(t), None)
    val a  = mf.glyphBox(0x1D44E) // italic a

    a.width should be > 0.0
    a.ascent should be > 0.0
  }

  "an inline math list lays out left to right with positive total width" in {
    val t  = new Graphics2DTypesetter(dpi = 72)
    val mf = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))
    val m  = new MathMode(t, mf)

    m.addChar('a'); m.addCommand("leq") shouldBe true; m.addChar('b')
    val box = m.result.asInstanceOf[HBox]

    box.boxes should have length 5         // a, thick, ≤, thick, b
    box.width should be > 0.0
  }
