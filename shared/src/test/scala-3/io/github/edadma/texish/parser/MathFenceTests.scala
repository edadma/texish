package io.github.edadma.texish.parser

import io.github.edadma.texish.{
  Box,
  HeadlessTypesetter,
  MathAtom,
  MathClass,
  MathDelimiters,
  MathFont,
  MathMode,
  MathStyle,
  TexishException,
}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `\fence`, a delimiter at a size the author chooses rather than one sized to what it encloses. `\left`/`\right`
  * grow their fences to cover the formula between them, which is the right rule when there is a formula between
  * them; a fence standing on its own — an opening bracket whose partner is a line away, the bar of a set-builder,
  * a divider in a piecewise definition — has nothing to take its size from.
  *
  * Also here: the atom class a lone fence takes, which decides the space around it, and the fences `\frac` can
  * put around a stack.
  */
class MathFenceTests extends AnyFreeSpec with Matchers:

  def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  private def base(t: HeadlessTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  "a fence is centred on the math axis, as a \\left/\\right fence is" in {
    val t     = new HeadlessTypesetter
    val bf    = base(t)
    val m     = new MathMode(t, bf, MathStyle.Text)
    val fence = m.makeFenceAt(0x28, 1)

    // an axis-centred box straddles the axis: its ascent exceeds its descent by twice the axis height
    (fence.ascent - fence.descent) / 2 shouldBe bf.axisHeight +- 1e-9
  }

  "without a MATH table every size gives the base glyph, so a fence still sets" in {
    // the stub font carries no MATH table and so has no size variants; the documented fallback is the base
    // glyph at every size, which must not throw or measure differently
    val t  = new HeadlessTypesetter
    val bf = base(t)

    val sizes = (0 to 4).map(n => bf.variantAt(0x28, n).width)

    sizes.distinct.size shouldBe 1
  }

  "a fence's class follows the delimiter: openers open, closers close, symmetric fences relate" in {
    MathDelimiters.classOf(0x28) shouldBe MathClass.Open    // (
    MathDelimiters.classOf(0x5B) shouldBe MathClass.Open    // [
    MathDelimiters.classOf(0x7B) shouldBe MathClass.Open    // {
    MathDelimiters.classOf(0x27E8) shouldBe MathClass.Open  // ⟨
    MathDelimiters.classOf(0x29) shouldBe MathClass.Close   // )
    MathDelimiters.classOf(0x230B) shouldBe MathClass.Close // ⌋
    MathDelimiters.classOf(0x7C) shouldBe MathClass.Rel     // |
    MathDelimiters.classOf(0x2016) shouldBe MathClass.Rel   // ‖
  }

  "the class is what decides the space around a lone fence" in {
    // a relation keeps a thick space on each side; an opener keeps none from what follows it. This is the whole
    // reason a lone fence needs a class at all, so it is worth pinning that the two really do set differently.
    def widthWith(cls: MathClass): Double =
      val t = new HeadlessTypesetter
      val m = new MathMode(t, base(t), MathStyle.Text)

      m.addNode(MathAtom(cls, m.makeFenceAt(0x7C, 1)))
      m.addChar('x')
      m.result.asInstanceOf[Box].width

    widthWith(MathClass.Rel) should be > widthWith(MathClass.Open)
  }

  "class: overrides the inferred class, for a fence used against its usual sense" in {
    fenceClass("open") shouldBe Some(MathClass.Open)
    fenceClass("close") shouldBe Some(MathClass.Close)
    fenceClass("rel") shouldBe Some(MathClass.Rel)
    fenceClass("relation") shouldBe Some(MathClass.Rel)
    fenceClass("ord") shouldBe Some(MathClass.Ord)
    fenceClass("Open") shouldBe Some(MathClass.Open)
    fenceClass("bracket") shouldBe None
  }

  "\\fence sets a character or a named delimiter, with or without a size" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\fence size:2 {(} x \\fence size:2 {)}$")
    noException should be thrownBy proc.process("$\\fence{\\langle} x \\fence{\\rangle}$")
    noException should be thrownBy proc.process("$\\{ x \\fence{|} y \\}$")
  }

  "a null delimiter draws nothing, as it does after \\left" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\fence{.} x$")
  }

  "\\fence outside math is reported as a math-only command" in {
    val (_, proc) = fixture()
    val ex        = the[TexishException] thrownBy proc.process("\\fence{(}")
    ex.getMessage should include("only allowed in math mode")
  }

  "\\fence with something that is not one delimiter is reported" in {
    val (_, proc) = fixture()
    val ex        = the[TexishException] thrownBy proc.process("$\\fence{abc}$")
    ex.getMessage should include("one delimiter")
  }

  "a delimiter option reads a bare character or a command name, with or without its backslash" in {
    delimiterNamed("(") shouldBe Some(0x28)
    delimiterNamed("|") shouldBe Some(0x7C)
    delimiterNamed("\\langle") shouldBe Some(0x27E8)
    delimiterNamed("langle") shouldBe Some(0x27E8)
    delimiterNamed(" rfloor ") shouldBe Some(0x230B)
    delimiterNamed("nonsense") shouldBe None
  }

  "\\frac's fences wrap the stack, which is what \\binom is" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\frac left:( right:) rule:0 {n}{k}$")

    // the fenced form is wider than the bare stack by the two fences
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)

    def part(style: MathStyle, c: Char): Box =
      val s = new MathMode(t, bf, style)
      s.addChar(c)
      s.result.asInstanceOf[Box]

    val frac   = m.makeFractionOf(part(MathStyle.Text.num, 'n'), part(MathStyle.Text.denom, 'k'), MathStyle.Text, None)
    val fenced = m.makeDelimited(Some(0x28), frac, Some(0x29))

    fenced.width should be > frac.width
  }
