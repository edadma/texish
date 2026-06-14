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

  "a glyph box is as wide as the advance, not the ink box, so glyphs don't crowd" in {
    val t  = new CairoImageTypesetter(100)
    val mf = new MathFont(t, lmmath(t), None)
    val rf = mf.font.renderFont.asInstanceOf[t.RenderFont]

    // the box must advance by the glyph's advance width (what keeps glyphs from crowding), not its ink box
    for cp <- Seq('a'.toInt, 0x1D44E, 0x2B, 0x222B) do
      mf.glyphBox(cp).width shouldBe t.glyphExtents(rf, mf.glyphIndex(cp)).xAdvance
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

  "x^2 sets the superscript smaller and raises it above the nucleus" in {
    val t    = new CairoImageTypesetter(100)
    val full = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))
    val m    = new MathMode(t, full, MathStyle.Text)

    val nucleusAscent = full.glyphBox(0x1D465).ascent // math-italic x
    m.addChar('x')

    val sup = new MathMode(t, full, MathStyle.Text.sup)
    sup.mathFont.size shouldBe (full.size * full.scriptPercentScaleDown +- 0.001) // script style is 70%
    sup.addChar('2')
    m.addScript(superscript = true, sup.result.asInstanceOf[Box])

    val sb = m.result.asInstanceOf[HBox].boxes.head.asInstanceOf[MathScriptBox]
    sb.ascent should be > nucleusAscent      // the superscript pushes the box's top up
    sb.width should be > full.glyphBox(0x1D465).width
  }

  "a stretchy delimiter grows past the base glyph to span a tall target, centred on the axis" in {
    val t    = new CairoImageTypesetter(100)
    val full = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))

    val baseParen = full.glyphBox(0x28).height

    val tall = full.verticalVariant(0x28, baseParen * 4)
    tall.height should be > baseParen

    val fence = full.delimiter(0x28, baseParen * 4).asInstanceOf[AxisCenteredBox]
    (fence.ascent - fence.descent) shouldBe (2 * full.axisHeight +- 0.001)
  }
