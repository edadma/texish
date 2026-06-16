package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 4: fractions. On the fixed-metric [[HeadlessTypesetter]] (glyph 6 wide, ascent 8, descent 2) with no
  * MATH table, fraction layout uses TeX's Computer Modern defaults ([[FractionParams.texDefaults]]) at the
  * 14pt text font, so the axis (0.25 em = 3.5), rule thickness and gaps are exact.
  */
class MathFractionTests extends AnyFreeSpec with Matchers:

  def base(t: HeadlessTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  def part(t: HeadlessTypesetter, bf: MathFont, style: MathStyle, c: Char): Box =
    val sm = new MathMode(t, bf, style)
    sm.addChar(c)
    sm.result.asInstanceOf[Box]

  "the numerator/denominator transitions step down a size and the denominator cramps" in {
    import MathSize.*

    MathStyle.Text.num shouldBe MathStyle(Script, cramped = false)
    MathStyle.Text.denom shouldBe MathStyle(Script, cramped = true)
    MathStyle.Display.num shouldBe MathStyle(Text, cramped = false)
    MathStyle.Display.isDisplay shouldBe true
    MathStyle.Text.isDisplay shouldBe false
  }

  "a fraction stacks numerator over denominator about the math axis" in {
    val t   = new HeadlessTypesetter
    val bf  = base(t)
    val m   = new MathMode(t, bf, MathStyle.Text)
    val num = part(t, bf, MathStyle.Text.num, 'a')
    val den = part(t, bf, MathStyle.Text.denom, 'b')

    val frac = m.makeFraction(num, den).asInstanceOf[FractionBox]

    // axis 3.5, rule 0.56, gaps 0.56, shifts max'd against the gap clearance: numerator up 6.34, denom down 5.34
    frac.ascent shouldBe (6.34 + 8 +- 0.05)
    frac.descent shouldBe (5.34 + 2 +- 0.05)
    frac.width shouldBe (6.0 +- 0.001) // the wider of the two equal parts
  }

  "the numerator draws above the baseline and the denominator below it" in {
    val rec = new RecordingGlyphTypesetter
    val bf  = base(rec)
    val m   = new MathMode(rec, bf, MathStyle.Text)
    val num = part(rec, bf, MathStyle.Text.num, '7')
    val den = part(rec, bf, MathStyle.Text.denom, '3')

    rec.draw(m.makeFraction(num, den))

    val numY = rec.drawn.collectFirst { case (g, _, y) if g == '7'.toInt => y }.get
    val denY = rec.drawn.collectFirst { case (g, _, y) if g == '3'.toInt => y }.get
    numY should be < denY // smaller y is higher on the page (the numerator sits above the bar)
  }

  "a fraction enters the list as an Inner atom and lays out with positive size" in {
    val t   = new HeadlessTypesetter
    val bf  = base(t)
    val m   = new MathMode(t, bf, MathStyle.Text)
    val num = part(t, bf, MathStyle.Text.num, 'a')
    val den = part(t, bf, MathStyle.Text.denom, 'b')

    m.addNode(MathAtom(MathClass.Inner, m.makeFraction(num, den)))
    val box = m.result.asInstanceOf[HBox]

    box.boxes should have length 1
    box.boxes.head shouldBe a[FractionBox]
    box.height should be > 0.0
  }

  "an infix \\over splits the list into a fraction at exit" in {
    val t = new HeadlessTypesetter
    val m = new MathMode(t, base(t), MathStyle.Text)

    m.addChar('a')
    m.setFraction(bar = true) // the 'a' becomes the numerator
    m.addChar('b')            // and 'b' the denominator

    m.result shouldBe a[FractionBox]
  }

  "\\over draws the fraction rule while \\atop omits it" in {
    def ruleCount(bar: Boolean): Int =
      val rec = new RuleRecordingTypesetter
      val m   = new MathMode(rec, new MathFont(rec, rec.currentFont, None), MathStyle.Text)
      m.addChar('a'); m.setFraction(bar); m.addChar('b')
      rec.draw(m.result.asInstanceOf[Box])
      rec.rules

    ruleCount(bar = true) shouldBe 1  // \over: one rule centred on the axis
    ruleCount(bar = false) shouldBe 0 // \atop: stacked with no rule
  }

  "a second \\over in the same group is an ambiguity error" in {
    val t = new HeadlessTypesetter
    val m = new MathMode(t, base(t), MathStyle.Text)

    m.addChar('a'); m.setFraction(bar = true); m.addChar('b')
    an[RuntimeException] should be thrownBy m.setFraction(bar = false)
  }

/** Records fillRect calls so a test can tell whether a fraction drew its bar. */
class RuleRecordingTypesetter extends HeadlessTypesetter:
  var rules: Int = 0

  override def fillRect(x: Double, y: Double, width: Double, height: Double): Unit = rules += 1
