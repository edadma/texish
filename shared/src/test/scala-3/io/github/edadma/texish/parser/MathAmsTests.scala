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
  VBox,
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

  "a forced-style fraction takes its constants from the forced style, not the surrounding script size" in {
    // \dfrac's operands are full-size wherever it sits; the bar thickness, axis and shifts must match them,
    // so a \dfrac inside a superscript is geometrically identical to one in running text
    def frac(parentStyle: MathStyle): FractionBox =
      val t   = new HeadlessTypesetter
      val bf  = base(t)
      val m   = new MathMode(t, bf, parentStyle)
      val num = part(t, bf, MathStyle.Display.num, 'a')
      val den = part(t, bf, MathStyle.Display.denom, 'b')
      m.makeFractionAt(num, den, display = true, bar = true).asInstanceOf[FractionBox]

    val inScript = frac(MathStyle.Text.sup)
    val inText   = frac(MathStyle.Text)

    inScript.ascent shouldBe (inText.ascent +- 0.001)
    inScript.descent shouldBe (inText.descent +- 0.001)
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

  /** The natural width of the whole math formula in `src`. Every horizontal box that holds glyph nuclei
    * directly is a laid-out math list — the outer `$…$` plus any braced sub-formula, which is added separately
    * and is necessarily narrower. The outer formula contains the inner ones, so it is the widest of them; the
    * paragraph line built around it holds no glyphs directly, so it is excluded. Used to see the inter-atom
    * spacing the class-forcing commands induce. */
  private def mathWidth(src: String): Double =
    val captured = scala.collection.mutable.ArrayBuffer[Box]()
    val t = new HeadlessTypesetter:
      override infix def add(box: Box): io.github.edadma.texish.Typesetter =
        captured += box
        super.add(box)
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.process(src)
    captured.collect { case h: HBox if h.boxes.exists(_.isInstanceOf[GlyphBox]) => h.width }.max

  "the class-forcing commands give their atom that class's spacing" in {
    val ord = mathWidth("$axb$")              // three ordinary atoms, no inter-atom space
    val bin = mathWidth("$a\\mathbin{x}b$")   // x forced to a binary operator: a medium space on each side
    val rel = mathWidth("$a\\mathrel{x}b$")   // x forced to a relation: a (wider) thick space on each side
    val forcedOrd = mathWidth("$a\\mathord{x}b$") // x forced back to ordinary: no space, same as bare

    bin should be > ord
    rel should be > bin
    forcedOrd shouldBe (ord +- 0.001)
  }

  "the math low ellipsis sets visible dots, not the missing … glyph" in {
    // the math font carries no … glyph, so \ldots is built from three period glyphs; were it the bare … glyph it
    // would render blank and $1\ldots2$ would be no wider than $12$
    mathWidth("$1\\ldots 2$") should be > (mathWidth("$12$") + 10.0)
  }

  /** Every glyph codepoint drawn anywhere in the formula `src`, in tree order. */
  private def mathGlyphCodes(src: String): List[Int] =
    val captured = scala.collection.mutable.ArrayBuffer[Box]()
    val t = new HeadlessTypesetter:
      override infix def add(box: Box): io.github.edadma.texish.Typesetter =
        captured += box
        super.add(box)
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.process(src)
    def codes(b: Box): List[Int] = b match
      case g: GlyphBox => List(g.glyph)
      case h: HBox     => h.boxes.toList.flatMap(codes)
      case v: VBox     => v.boxes.toList.flatMap(codes)
      case _           => Nil
    captured.toList.flatMap(codes)

  "\\dots is context-sensitive: centred dots before an operator, low dots before a comma" in {
    // amsmath's rule — \cdots (the ⋯ glyph, 0x22EF) between operators, \ldots (period glyphs, 0x2E) in a list
    val beforeOp    = mathGlyphCodes("$a + \\dots + b$").toSet
    val beforeComma = mathGlyphCodes("$a, \\dots, b$").toSet
    beforeOp should contain(0x22EF)        // centred ⋯
    beforeOp should not contain '.'.toInt
    beforeComma should contain('.'.toInt)  // low period dots
    beforeComma should not contain 0x22EF
  }

  // Tokenize with `&` active (as the processor does), so a matrix body splits on the column separator.
  private def tokenize(s: String): Vector[Token] =
    val tz   = Tokenizer(s, Set('~', '&'))
    val out  = Vector.newBuilder[Token]
    var done = false
    while !done do
      tz.next() match
        case Token.EOF(_) => done = true
        case other        => out += other
    out.result()

  "an array environment composes inside \\left…\\right" in {
    // the headline of the token-level body collector: stretchy delimiters around a \begin{matrix}, which the
    // earlier raw-capture implementation could not do (\left pre-collects its body into tokens)
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\left(\\begin{matrix} a & b \\\\ c & d \\end{matrix}\\right)$")
  }

  "an array environment reached through a macro expands" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("\\def m {\\begin{aligned} a &= b \\\\ c &= d \\end{aligned}}$\\m$")
  }

  "a nested environment inside a cell does not split the outer array" in {
    // the inner matrix's own & and \\ must stay inside the one cell that holds it, so the outer body is still a
    // 2×2 array: row 0 = [ a , <a whole matrix> ], row 1 = [ b , c ]
    val rows = splitMatrixBody(tokenize("a & \\begin{matrix} x & y \\end{matrix} \\\\ b & c"))
    rows should have length 2
    rows(0) should have length 2
    rows(1) should have length 2
  }

  "a nested matrix inside an aligned cell parses end to end" in {
    val (_, proc) = fixture()
    noException should be thrownBy
      proc.process("$\\begin{aligned} a &= \\begin{matrix} 1 & 2 \\\\ 3 & 4 \\end{matrix} \\\\ b &= c \\end{aligned}$")
  }

  "\\overline raises the top by a rule and gap; \\underline lowers the bottom; each draws one rule" in {
    val t     = new HeadlessTypesetter
    val bf    = base(t)
    val m     = new MathMode(t, bf, MathStyle.Text)
    val inner = part(t, bf, MathStyle.Text, 'x')

    val over  = m.makeBar(inner, over = true)
    val under = m.makeBar(inner, over = false)

    over.ascent should be > inner.ascent          // the bar and its gap sit above the content
    over.descent shouldBe (inner.descent +- 1e-9) // the bottom is unchanged
    under.descent should be > inner.descent       // the bar and its gap sit below the content
    under.ascent shouldBe (inner.ascent +- 1e-9)  // the top is unchanged
    over.width shouldBe (inner.width +- 1e-9)     // the rule spans exactly the content's width

    val rec = new io.github.edadma.texish.RuleRecordingTypesetter
    rec.draw(over)
    rec.rules shouldBe 1
  }

  "\\overline and \\underline parse inside math" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\overline{x + y} + \\underline{a + b}$")
  }

  "\\underline still wraps text outside math, and \\overline is math-only" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("\\underline{hello}") // the text path is preserved
    the[ParserException] thrownBy {
      val (_, p) = fixture()
      p.process("\\overline{x}")
    }
  }
