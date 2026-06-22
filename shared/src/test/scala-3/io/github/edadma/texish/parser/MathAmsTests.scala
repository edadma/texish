package io.github.edadma.texish.parser

import io.github.edadma.texish.{
  Box,
  ColumnAlign,
  FractionBox,
  GlyphBox,
  HBox,
  HeadlessTypesetter,
  MathFont,
  MathMode,
  MathStyle,
  MatrixBox,
}
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

  "a right-aligned column sits flush right, and a zero-gap seam butts the next column against it" in {
    val rec = new io.github.edadma.texish.RecordingGlyphTypesetter
    def glyph(c: Char): Box = new GlyphBox(rec, c.toInt, rec.currentFont, rec.currentColor)
    // column 0 is right-aligned: a narrow cell (6 wide) over a wide one (12), so the column is 12 wide; column 1
    // is left-aligned and the seam between them is closed up to zero.
    val rows = Vector(
      Vector(glyph('a'), glyph('p')),
      Vector(HBox(Vector(glyph('b'), glyph('c'))), glyph('q')),
    )
    val m = new MatrixBox(rows, axisHeight = 3.5, rowSep = 0.0, Vector(ColumnAlign.Right, ColumnAlign.Left), Vector(0.0))
    rec.draw(m)
    def xOf(c: Char): Double = rec.drawn.collectFirst { case (g, x, _) if g == c.toInt => x }.get

    xOf('a') shouldBe (6.0 +- 1e-9)  // narrow cell pushed flush right under the 12-wide column
    xOf('b') shouldBe (0.0 +- 1e-9)  // the wide cell fills its column
    xOf('p') shouldBe (12.0 +- 1e-9) // column 1 begins right at column 0's right edge — no inter-column gap
  }

  "makeArray lays an aligned block out right-then-left with a wide gap between pairs" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Display)
    val cell = part(t, bf, MathStyle.Text, 'x')
    // two equations, each a right column and a left column: the box is wider than four bare cells because a wide
    // gap opens between the pairs, while the within-pair seam adds nothing.
    val rows  = Vector(Vector(cell, cell, cell, cell))
    val block = m.makeArray(rows, io.github.edadma.texish.MathArrayAlign.Aligned)
    block.width should be > (4 * cell.width)
  }

  "the over/under, substack, boxed and operatorname primitives parse inside math" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\overset{*}{=} \\underset{n}{\\min}$")
    noException should be thrownBy {
      val (_, p) = fixture()
      p.process("$\\sum_{\\substack{0 < i \\\\ i < n}} i + \\boxed{x} + \\operatorname{argmax}$")
    }
  }

  "the aligned-equation and matrix array environments parse inside math" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\begin{aligned} a &= b \\\\ c &= d \\end{aligned}$")
    noException should be thrownBy {
      val (_, p) = fixture()
      p.process("$\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix}$")
    }
    noException should be thrownBy {
      val (_, p) = fixture()
      p.process("$\\begin{smallmatrix} a & b \\\\ c & d \\end{smallmatrix}$")
    }
    noException should be thrownBy {
      val (_, p) = fixture()
      p.process("$\\begin{cases} a & x > 0 \\\\ b & x < 0 \\end{cases}$")
    }
  }

  "an array environment outside math is an error" in {
    val (_, proc) = fixture()
    the[ParserException] thrownBy proc.process("\\begin{pmatrix} a & b \\end{pmatrix}")
  }
