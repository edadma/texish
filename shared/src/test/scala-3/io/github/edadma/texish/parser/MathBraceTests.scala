package io.github.edadma.texish.parser

import io.github.edadma.texish.{
  Box,
  HeadlessTypesetter,
  MathFont,
  MathMode,
  MathStyle,
  RuleRecordingTypesetter,
  TexishException,
}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `\overbrace` and `\underbrace` — a horizontal brace grown to span a whole sub-formula and set above or below
  * it — and `\vcenter`, which centres a box on the math axis instead of standing it on the baseline. Both are
  * built on machinery the engine already had: the brace grows along the font's horizontal glyph variants, the
  * same path a wide accent takes, and the axis centring is what a stretchy fence already does.
  *
  * Also here: `\frac`'s `rule:`, which sets the bar thickness and, at zero, gives the bar-less stack.
  */
class MathBraceTests extends AnyFreeSpec with Matchers:

  def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  private def base(t: HeadlessTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  private def formula(t: HeadlessTypesetter, bf: MathFont, s: String): Box =
    val m = new MathMode(t, bf, MathStyle.Text)
    s.foreach(c => m.addChar(c))
    m.result.asInstanceOf[Box]

  "an overbrace grows upward and leaves the depth alone; an underbrace does the reverse" in {
    val t     = new HeadlessTypesetter
    val bf    = base(t)
    val m     = new MathMode(t, bf, MathStyle.Text)
    val inner = formula(t, bf, "a+b")

    val over  = m.makeBrace(inner, over = true)
    val under = m.makeBrace(inner, over = false)

    over.width shouldBe inner.width
    over.ascent should be > inner.ascent
    over.descent shouldBe inner.descent

    under.width shouldBe inner.width
    under.ascent shouldBe inner.ascent
    under.descent should be > inner.descent
  }

  "the brace spans the content, so a wider formula gets a wider brace box" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)

    val narrow = m.makeBrace(formula(t, bf, "a"), over = true)
    val wide   = m.makeBrace(formula(t, bf, "a+b+c"), over = true)

    wide.width should be > narrow.width
  }

  "\\overbrace and \\underbrace set, and take a script over the brace" in {
    val (_, proc) = fixture()
    // the braced formula is an Op atom with its limits set, so ^{n} rides above the brace rather than beside it
    noException should be thrownBy proc.process("$\\overbrace{a + b}^{n}$")
    noException should be thrownBy proc.process("$\\underbrace{x_1 + x_2}_{\\text{terms}}$")
    noException should be thrownBy proc.process("$$\\overbrace{a + b + c}^{n \\text{ of them}}$$")
  }

  "a brace outside math is reported as a math-only command" in {
    for name <- Seq("overbrace", "underbrace") do
      val (_, proc) = fixture()
      val ex        = the[TexishException] thrownBy proc.process(s"\\$name{x}")
      ex.getMessage should include("only allowed in math mode")
  }

  "\\vcenter centres its box on the math axis" in {
    val t   = new HeadlessTypesetter
    val bf  = base(t)
    val m   = new MathMode(t, bf, MathStyle.Text)
    val box = m.makeVcentered(formula(t, bf, "abc"))

    (box.ascent - box.descent) / 2 shouldBe bf.axisHeight +- 1e-9
    box.width shouldBe formula(t, bf, "abc").width
  }

  "\\vcenter sets a vertical box beside a formula" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$x = \\vcenter{\\hbox{a}\\hbox{b}}$")
  }

  "\\vcenter outside math is reported as a math-only command" in {
    val (_, proc) = fixture()
    val ex        = the[TexishException] thrownBy proc.process("\\vcenter{\\hbox{a}}")
    ex.getMessage should include("only allowed in math mode")
  }

  "\\frac rule:0 gives the bar-less stack, and a thicker rule is thicker" in {
    val t   = new HeadlessTypesetter
    val bf  = base(t)
    val m   = new MathMode(t, bf, MathStyle.Text)
    val num = formula(t, bf, "a")
    val den = formula(t, bf, "b")

    val ruled   = m.makeFractionOf(num, den, MathStyle.Text, None)
    val barless = m.makeFractionOf(num, den, MathStyle.Text, Some(0.0))
    val thick   = m.makeFractionOf(num, den, MathStyle.Text, Some(3.0))

    val recRuled = new RuleRecordingTypesetter
    recRuled.draw(ruled)
    recRuled.rules shouldBe 1

    val recBarless = new RuleRecordingTypesetter
    recBarless.draw(barless)
    recBarless.rules shouldBe 0

    // a thicker bar makes a taller stack: the rule's own thickness is part of the fraction's reach
    thick.height should be > ruled.height
  }

  "\\frac rule: is accepted through the parser" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\frac rule:0 {n}{k} + \\frac rule:1.2pt {a}{b}$")
  }
