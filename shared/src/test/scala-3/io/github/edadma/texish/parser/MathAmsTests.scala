package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, FractionBox, HeadlessTypesetter, MathFont, MathMode, MathStyle}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The amsmath-style math constructs the `math` package builds on: the explicit-style fractions
  * (`\dfrac`/`\tfrac`), the binomial family (`\binom`/`\dbinom`/`\tbinom`), and the extra delimited matrix
  * forms (`\vmatrix`/`\Vmatrix`/`\Bmatrix`). The geometry of [[MathMode.makeFractionAt]] is checked on the
  * fixed-metric stub; the primitives are then exercised end to end through the `$…$` parser path.
  */
class MathAmsTests extends AnyFreeSpec with Matchers:

  def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  private def base(t: HeadlessTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  private def part(t: HeadlessTypesetter, bf: MathFont, style: MathStyle, c: Char): Box =
    val sm = new MathMode(t, bf, style)
    sm.addChar(c)
    sm.result.asInstanceOf[Box]

  "makeFractionAt with a bar draws a rule; without a bar it omits it" in {
    val t   = new HeadlessTypesetter
    val bf  = base(t)
    val m   = new MathMode(t, bf, MathStyle.Text)
    val num = part(t, bf, MathStyle.Text.num, 'a')
    val den = part(t, bf, MathStyle.Text.denom, 'b')

    val barred  = m.makeFractionAt(num, den, display = false, bar = true).asInstanceOf[FractionBox]
    val barless = m.makeFractionAt(num, den, display = false, bar = false).asInstanceOf[FractionBox]

    val rec1 = new io.github.edadma.texish.RuleRecordingTypesetter
    rec1.draw(barred)
    rec1.rules shouldBe 1 // \dfrac/\tfrac keep the fraction rule

    val rec2 = new io.github.edadma.texish.RuleRecordingTypesetter
    rec2.draw(barless)
    rec2.rules shouldBe 0 // \binom stacks with no rule
  }

  "forcing display style opens the fraction gaps wider than text style" in {
    val t   = new HeadlessTypesetter
    val bf  = base(t)
    val m   = new MathMode(t, bf, MathStyle.Text)
    val num = part(t, bf, MathStyle.Text.num, 'a')
    val den = part(t, bf, MathStyle.Text.denom, 'b')

    val display = m.makeFractionAt(num, den, display = true, bar = true).asInstanceOf[FractionBox]
    val text    = m.makeFractionAt(num, den, display = false, bar = true).asInstanceOf[FractionBox]

    // the display shifts are larger, so the display fraction reaches higher and lower than the text one
    display.ascent should be > text.ascent
    display.descent should be > text.descent
  }

  "the fraction and binomial primitives parse inside math without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\dfrac{a}{b} + \\tfrac{1}{2}$")
    noException should be thrownBy {
      val (_, p) = fixture()
      p.process("$\\binom{n}{k} = \\dbinom{n}{k} = \\tbinom{n}{k}$")
    }
  }

  "the delimited matrix forms parse inside math without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\vmatrix{a & b \\\\ c & d}$")
    noException should be thrownBy {
      val (_, p) = fixture()
      p.process("$\\Vmatrix{a & b \\\\ c & d}$")
    }
    noException should be thrownBy {
      val (_, p) = fixture()
      p.process("$\\Bmatrix{a & b \\\\ c & d}$")
    }
  }

  "the fraction primitives are math-mode only" in {
    val (_, proc) = fixture()
    the[ParserException] thrownBy proc.process("\\dfrac{a}{b}") // text mode
    the[ParserException] thrownBy {
      val (_, p) = fixture()
      p.process("\\binom{n}{k}")
    }
  }
