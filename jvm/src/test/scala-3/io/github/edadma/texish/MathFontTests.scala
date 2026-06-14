package io.github.edadma.texish

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

  "the math font reports its script scale-downs and a positive italic correction for the integral sign" in {
    val t  = new Graphics2DTypesetter(dpi = 72)
    val mf = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))

    mf.scriptPercentScaleDown should (be > 0.0 and be < 1.0)        // ~0.7 in LM Math
    mf.scriptScriptPercentScaleDown should be < mf.scriptPercentScaleDown
    mf.italicCorrection(0x222B) should be > 0.0                     // ∫ leans hard right
  }

  "x^2 sets the superscript smaller and raises it above the nucleus" in {
    val t    = new Graphics2DTypesetter(dpi = 72)
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

  "fraction parameters come from the MATH table, and a fraction straddles the axis" in {
    val t    = new Graphics2DTypesetter(dpi = 72)
    val full = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))
    val fp   = full.fractionParams(display = false)

    fp.ruleThickness should be > 0.0
    fp.axisHeight shouldBe (0.25 * full.size +- 0.5) // the bar sits on the math axis, ~quarter em
    fp.numShiftUp should be > 0.0
    fp.denomShiftDown should be > 0.0

    val m   = new MathMode(t, full, MathStyle.Text)
    val num = { val s = new MathMode(t, full, MathStyle.Text.num); s.addChar('1'); s.result.asInstanceOf[Box] }
    val den = { val s = new MathMode(t, full, MathStyle.Text.denom); s.addChar('2'); s.result.asInstanceOf[Box] }
    val frac = m.makeFraction(num, den).asInstanceOf[FractionBox]

    frac.ascent should be > full.axisHeight  // numerator rises above the axis
    frac.descent should be > 0.0             // denominator drops below the baseline
  }

  "the radical glyph grows with the target height, and a radical sets a bar above its radicand" in {
    val t    = new Graphics2DTypesetter(dpi = 72)
    val full = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))

    // a taller target selects a surd at least as tall as a short one (LM Math supplies vertical variants)
    val small = full.radicalGlyph(5).height
    val large = full.radicalGlyph(60).height
    small should be > 0.0
    large should be >= small

    val m = new MathMode(t, full, MathStyle.Text)
    val r = { val s = new MathMode(t, full, MathStyle.Text.cramp); s.addChar('x'); s.result.asInstanceOf[Box] }
    val rad = m.makeRadical(r).asInstanceOf[RadicalBox]

    rad.ascent should be > r.ascent                  // the vinculum rises above the radicand
    rad.width should be > full.glyphBox(0x1D465).width // surd plus the radicand
  }

  "a radical degree (a cube root's index) widens the radical and rides above its baseline" in {
    val t    = new Graphics2DTypesetter(dpi = 72)
    val full = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))

    val m   = new MathMode(t, full, MathStyle.Text)
    val r   = { val s = new MathMode(t, full, MathStyle.Text.cramp); s.addChar('x'); s.result.asInstanceOf[Box] }
    val deg = { val s = new MathMode(t, full, MathStyle.Text.rootDegree); s.addChar('3'); s.result.asInstanceOf[Box] }

    val plain  = m.makeRadical(r).asInstanceOf[RadicalBox]
    val rooted = m.makeRadical(r, Some(deg)).asInstanceOf[RadicalBox]

    // the index never shrinks the radical, and Latin Modern's degree kerns/raise produce a finite, sane box;
    // the precise left-of-stem, raised placement is pinned on the fixed-metric stub in MathRadicalTests
    rooted.width should be >= plain.width
    rooted.height should be >= plain.height
    rooted.height.isFinite shouldBe true
  }

  "a large operator grows in display style, and its limits stack over and under" in {
    val t    = new Graphics2DTypesetter(dpi = 72)
    val full = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))

    val textSum    = full.glyphBox(0x2211).height        // ∑ at text size
    val displaySum = full.largeOperator(0x2211, display = true).height
    displaySum should be > textSum                       // display style picks a taller variant

    // \sum\limits_0^n lays out as a limits box taller than the bare operator
    val m = new MathMode(t, full, MathStyle.Text)
    m.addCommand("sum") shouldBe true
    m.setLimits(true)
    m.addScript(superscript = false, { val s = new MathMode(t, full, MathStyle.Text.sub); s.addChar('0'); s.result.asInstanceOf[Box] })
    m.addScript(superscript = true, { val s = new MathMode(t, full, MathStyle.Text.sup); s.addChar('n'); s.result.asInstanceOf[Box] })

    val box = m.result.asInstanceOf[HBox].boxes.head
    box shouldBe a[LimitsBox]
    box.height should be > displaySum // the stacked limits make it taller than the operator alone
  }

  "in display style an integral grows even though its bounds stay to the side" in {
    val t    = new Graphics2DTypesetter(dpi = 72)
    val full = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))

    def intWithBounds(style: MathStyle): Box =
      val m = new MathMode(t, full, style)
      m.addCommand("int") shouldBe true
      m.addScript(superscript = false, { val s = new MathMode(t, full, style.sub); s.addChar('0'); s.result.asInstanceOf[Box] })
      m.addScript(superscript = true, { val s = new MathMode(t, full, style.sup); s.addChar('1'); s.result.asInstanceOf[Box] })
      m.result.asInstanceOf[HBox].boxes.head

    val inlineInt = intWithBounds(MathStyle.Text)
    val displayInt = intWithBounds(MathStyle.Display)

    inlineInt shouldBe a[MathScriptBox]  // ∫ keeps side-set bounds in both styles
    displayInt shouldBe a[MathScriptBox]
    displayInt.height should be > inlineInt.height // but the display integral sign is taller
  }

  "an accent rises above its nucleus, and a wide accent spans a multi-letter base" in {
    val t    = new Graphics2DTypesetter(dpi = 72)
    val full = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))

    full.glyphIndex(0x0302) should not be 0 // the font carries a combining circumflex (the \hat accent)

    val m    = new MathMode(t, full, MathStyle.Text)
    val x    = { val s = new MathMode(t, full, MathStyle.Text.cramp); s.addChar('x'); s.result.asInstanceOf[Box] }
    val hatX = m.makeAccent(0x0302, x, wide = false).asInstanceOf[AccentBox]

    hatX.ascent should be > x.ascent // the hat sits above the x
    hatX.width shouldBe x.width      // and does not widen it

    // a wide accent grows a horizontal variant to span a wider nucleus
    val wideBase = {
      val s = new MathMode(t, full, MathStyle.Text.cramp); "ABC".foreach(c => s.addChar(c.toInt)); s.result
        .asInstanceOf[Box]
    }
    full.horizontalVariant(0x0302, wideBase.width).width should be > full.glyphBox(0x0302).width
  }

  "a stretchy delimiter grows past the base glyph to span a tall target, centred on the axis" in {
    val t    = new Graphics2DTypesetter(dpi = 72)
    val full = new MathFont(t, lmmath(t), t.mathTableFor(lmmath(t)))

    val baseParen = full.glyphBox(0x28).height   // an ordinary '(' at text size

    // a target several line-heights tall must select a precomposed size variant (or an assembly) taller than
    // the base glyph — Latin Modern Math supplies a deep stack of '(' variants
    val tall = full.verticalVariant(0x28, baseParen * 4)
    tall.height should be > baseParen
    tall.height should be >= baseParen * 4 * 0.9  // reaches (close to) the target

    // the fence \left( sets is that variant, centred on the math axis
    val fence = full.delimiter(0x28, baseParen * 4).asInstanceOf[AxisCenteredBox]
    (fence.ascent - fence.descent) shouldBe (2 * full.axisHeight +- 0.001) // symmetric about the axis
  }
