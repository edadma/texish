package io.github.edadma.typesetter

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 2 against the real Latin Modern Math font on the Cairo backend, where the MATH table bytes come
  * from FreeType's FT_Load_Sfnt_Table. Mirrors the JVM MathFontTests so both backends are proven to load
  * the OpenType math font, parse its MATH table, and resolve the math-italic/symbol codepoints to glyphs.
  */
class CairoMathFontTests extends AnyFreeSpec with Matchers:

  def lmmath(t: CairoImageTypesetter): Font = t.makeFont("lmmath", 12, Set.empty)

  "the bundled math font carries a MATH table that the engine parses" in {
    val t    = new CairoImageTypesetter(100)
    val math = t.mathTableFor(lmmath(t))

    math shouldBe defined
    math.get.unitsPerEm shouldBe 1000
    math.get.constants.axisHeight shouldBe (0.25 +- 0.001)
    math.get.constants.fractionRuleThickness should be > 0.0
  }

  "the math-italic and symbol codepoints resolve to real glyphs in the math font" in {
    val t  = new CairoImageTypesetter(100)
    val mf = new MathFont(t, lmmath(t), None)

    mf.glyphIndex(0x1D6FC) should not be 0 // mathematical italic alpha
    mf.glyphIndex(0x1D44E) should not be 0 // mathematical italic a
    mf.glyphIndex(0x2264)  should not be 0 // ≤
    mf.glyphIndex(0x2211)  should not be 0 // ∑
  }

  "an inline math list lays out left to right with positive total width" in {
    val t  = new CairoImageTypesetter(100)
    val mf = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))
    val m  = new MathMode(t, mf)

    m.addChar('a'); m.addCommand("leq") shouldBe true; m.addChar('b')
    val box = m.result.asInstanceOf[HBox]

    box.boxes should have length 5
    box.width should be > 0.0
  }
