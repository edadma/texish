package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 5c: limits over and under large operators. On the fixed-metric [[HeadlessTypesetter]] (glyph 6 wide,
  * ascent 8, descent 2) with no MATH table, limit placement uses [[LimitsParams.texDefaults]] at the 14pt
  * text font, so the geometry is exact; the real-font operator enlargement is covered by the font tests.
  */
class MathLimitsTests extends AnyFreeSpec with Matchers:

  def base(t: HeadlessTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  "a limits box stacks the upper and lower over and under the operator" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val op = bf.glyphBox(0x2211); val up = bf.glyphBox('n'.toInt); val lo = bf.glyphBox('0'.toInt)

    val box = new LimitsBox(t, op, Some(up), Some(lo), LimitsParams.texDefaults(14))

    box.width shouldBe 6.0          // all three are 6 wide
    box.ascent shouldBe 21.6        // op 8 + upperRise 5.6 + upper ascent 8
    box.descent shouldBe 14.8       // op 2 + lowerDrop 10.8 + lower descent 2
  }

  "a limits box draws the upper above and the lower below the operator" in {
    val rec = new RecordingGlyphTypesetter
    val bf  = base(rec)
    val box = new LimitsBox(rec, bf.glyphBox(0x2211), Some(bf.glyphBox('n'.toInt)), Some(bf.glyphBox('0'.toInt)),
      LimitsParams.texDefaults(14))

    box.draw(rec, 0.0, 100.0)

    val opY = rec.drawn.collectFirst { case (g, _, y) if g == 0x2211 => y }.get
    val upY = rec.drawn.collectFirst { case (g, _, y) if g == 'n'.toInt => y }.get
    val loY = rec.drawn.collectFirst { case (g, _, y) if g == '0'.toInt => y }.get
    upY should be < opY // the upper limit is higher on the page (smaller y)
    loY should be > opY // the lower limit is lower
  }

  "\\sum\\limits with scripts lays out as a limits box, not side-set scripts" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)

    m.addCommand("sum") shouldBe true
    m.setLimits(true)
    m.addScript(superscript = false, bf.glyphBox('0'.toInt))
    m.addScript(superscript = true, bf.glyphBox('n'.toInt))

    val box = m.result.asInstanceOf[HBox]
    box.boxes.head shouldBe a[LimitsBox]
  }

  "a \\sum without \\limits keeps side-set scripts inline" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)

    m.addCommand("sum") shouldBe true
    m.addScript(superscript = true, bf.glyphBox('n'.toInt))

    m.result.asInstanceOf[HBox].boxes.head shouldBe a[MathScriptBox]
  }

  "limit controls must follow an operator" in {
    val t = new HeadlessTypesetter
    val m = new MathMode(t, base(t), MathStyle.Text)

    m.addChar('x') // an ordinary atom, not an operator
    an[RuntimeException] should be thrownBy m.setLimits(true)
  }

  "in display style a \\sum stacks its bounds by default, with no \\limits" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Display)

    m.addCommand("sum") shouldBe true
    m.addScript(superscript = false, bf.glyphBox('0'.toInt))
    m.addScript(superscript = true, bf.glyphBox('n'.toInt))

    m.result.asInstanceOf[HBox].boxes.head shouldBe a[LimitsBox]
  }

  "in display style an integral keeps its bounds to the side by default" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Display)

    m.addCommand("int") shouldBe true // ∫ is set side-set even in display, unlike \sum
    m.addScript(superscript = false, bf.glyphBox('0'.toInt))
    m.addScript(superscript = true, bf.glyphBox('n'.toInt))

    m.result.asInstanceOf[HBox].boxes.head shouldBe a[MathScriptBox]
  }

  "\\nolimits overrides the display default back to side-set scripts" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Display)

    m.addCommand("sum") shouldBe true
    m.setLimits(false)
    m.addScript(superscript = true, bf.glyphBox('n'.toInt))

    m.result.asInstanceOf[HBox].boxes.head shouldBe a[MathScriptBox]
  }
