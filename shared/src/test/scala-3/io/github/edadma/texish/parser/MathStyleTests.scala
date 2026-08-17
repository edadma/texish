package io.github.edadma.texish.parser

import io.github.edadma.texish.{HeadlessTypesetter, MathFont, MathMode, MathSize, MathStyle, TexishException}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The style declarations — `\displaystyle`, `\textstyle`, `\scriptstyle`, `\scriptscriptstyle` — which switch
  * the size a formula is set at for the rest of the enclosing sub-formula, the way `\bfseries` switches weight
  * for the rest of its group. Style is what makes an inline `\sum` set its bounds beside it and a displayed one
  * stack them, so being able to ask for a style is what lets a formula in running text be enlarged.
  *
  * Also here: `\frac`'s `style:` parameter, which sets one fraction at a chosen style without disturbing the
  * list around it, and the `mu` unit — 1/18 em, the unit TeX measures math spacing in.
  */
class MathStyleTests extends AnyFreeSpec with Matchers:

  def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  private def base(t: HeadlessTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  "a style switch changes the size later symbols are set at" in {
    val t    = new HeadlessTypesetter
    val m    = new MathMode(t, base(t), MathStyle.Text)
    val full = m.mathFont.size

    m.setStyle(MathStyle.Script)
    m.mathFont.size should be < full
    m.style.size shouldBe MathSize.Script

    m.setStyle(MathStyle.Display)
    m.mathFont.size shouldBe full // display and text are both the full math size
    m.style.isDisplay shouldBe true
  }

  "scriptscript is smaller than script, and both are absolute from the base rather than compounded" in {
    val t = new HeadlessTypesetter
    val m = new MathMode(t, base(t), MathStyle.Text)

    m.setStyle(MathStyle.Script)
    val script = m.mathFont.size

    m.setStyle(MathStyle.ScriptScript)
    val scriptscript = m.mathFont.size

    scriptscript should be < script

    // stepping back up gives the same size again: the font is scaled from the base every time, so a walk
    // through the styles cannot drift
    m.setStyle(MathStyle.Script)
    m.mathFont.size shouldBe script
  }

  "a style switch keeps the current cramping" in {
    // TeX's switches name the four uncramped styles and so silently uncramp; carrying the flag through means a
    // switch under a radical or in a denominator still sets its superscripts at the cramped height
    MathStyle.Text.cramp.atSize(MathSize.Display).cramped shouldBe true
    MathStyle.Text.atSize(MathSize.Script).cramped shouldBe false
    MathStyle.Text.cramp.atSize(MathSize.Display).size shouldBe MathSize.Display
  }

  "each of the four declarations is accepted in math" in {
    for name <- Seq("displaystyle", "textstyle", "scriptstyle", "scriptscriptstyle") do
      val (_, proc) = fixture()
      noException should be thrownBy proc.process(s"$$x + \\$name y$$")
  }

  "a style declaration outside math is reported as a math-only command" in {
    for name <- Seq("displaystyle", "textstyle", "scriptstyle", "scriptscriptstyle") do
      val (_, proc) = fixture()
      val ex        = the[TexishException] thrownBy proc.process(s"\\$name")
      ex.getMessage should include("only allowed in math mode")
  }

  "braces scope a style declaration to the sub-formula they open" in {
    // {…} in math is its own math list, so the switch inside it cannot leak out — this is what makes
    // {\displaystyle …} the idiom for enlarging one part of a formula
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$a + {\\displaystyle \\sum_{i=1}^{n} x_i} + b$")
  }

  "\\frac style: sets one fraction at a chosen style" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\frac style:display {1}{2} + \\frac style:script {3}{4}$")
  }

  "an unknown style: name leaves the fraction at the surrounding style rather than failing" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\frac style:enormous {1}{2}$")
    fracStyle("enormous") shouldBe None
    fracStyle("Display") shouldBe Some(MathStyle.Display)
    fracStyle(" scriptscript ") shouldBe Some(MathStyle.ScriptScript)
  }

  "the style, fence, brace and axis-box constructs compose in one document" in {
    // the shapes the math demo puts on the page, run together: a style switch around a big operator, a lone
    // fence inside a set-builder, a labelled brace, an \hbox and a \vcenter side by side in a formula
    for snippet <- Seq(
        """The sum $\displaystyle \sum_{i=1}^{n} x_i$ sets big.""",
        """$$\{\, x \fence{|} x > 0 \,\}$$""",
        """$$\fence size:2 {(} \frac{a}{b} \fence size:2 {)}$$""",
        """$$\overbrace{a_1 + a_2}^{n \text{ terms}}$$""",
        """$$\underbrace{x \cdot x}_{k}$$""",
        """$$T = \vcenter{\hbox{first}\hbox{second}}$$""",
        """$$T \ne \hbox{on the baseline} \eqno(1)$$""",
        """$${\scriptstyle \text{small}} \; {\scriptscriptstyle \text{smaller}} \leqno(2)$$""",
      )
    do
      val (_, p) = fixture()
      withClue(s"snippet: $snippet") { noException should be thrownBy p.process(snippet + "\n") }

    val (_, proc) = fixture()

    noException should be thrownBy proc.process(
      """The sum $\displaystyle \sum_{i=1}^{n} x_i$ sets big.
        |
        |$$\{\, x \fence{|} x > 0 \,\} \qquad \fence size:2 {(} \frac{a}{b} \fence size:2 {)}$$
        |
        |$$\overbrace{a_1 + a_2}^{n \text{ terms}} \qquad \underbrace{x \cdot x}_{k}$$
        |
        |$$T = \vcenter{\hbox{first}\hbox{second}} \ne \hbox{on the baseline} \eqno(1)$$
        |
        |$${\scriptstyle \text{small}} \; {\scriptscriptstyle \text{smaller}} \leqno(2)$$
        |""".stripMargin,
    )
  }

  "mu is 1/18 em — the unit TeX measures math spacing in" in {
    val (t, proc) = fixture()
    proc.process("\\set x {18mu}") // a quad
    t.getNumber("x") shouldBe 10.0 +- 1e-9

    proc.process("\\set y {3mu}") // a thin space, what \, sets
    t.getNumber("y") shouldBe (10.0 / 6) +- 1e-9
  }

  "mu works wherever a dimension does, so math spacing needs no separate skip command" in {
    val (t, proc) = fixture()
    noException should be thrownBy proc.process("$x \\hskip 3mu y$")

    // and in a glue spec, where TeX would need a muskip register
    proc.process("\\set g {0mu plus 6mu}")
    t.getVar("g") match
      case Value.Glue(natural, stretch, _, _, _) =>
        natural shouldBe 0.0 +- 1e-9
        stretch shouldBe (10.0 / 3) +- 1e-9 // 6mu = 6/18 em = a third of a 10pt em
      case other => fail(s"expected a glue, got $other")
  }
