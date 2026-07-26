package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Glue, GlyphBox, HBox, HeadlessTypesetter, MathAtom, MathClass, MathFont,
  MathMode, MathSymbols, TexishException}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The operator names, modular forms and implication arrows LaTeX defines alongside `\sin` and `\log`. They were
  * once a `math` package a document had to `\use`, which put an arbitrary line through a single standard set —
  * `\tan` built in, `\arctan` not. They are all built in now, so `$\arcsin x$` needs no preamble.
  *
  * On the fixed-metric [[HeadlessTypesetter]] every glyph is 6 wide and the math font is pinned to 14pt, so the
  * spaces below are exact: 18mu to the em, and `\,` `\;` `\quad` are 3mu, 5mu and 18mu of it.
  */
class MathOperatorNameTests extends AnyFreeSpec with Matchers:

  private def mathFont(t: HeadlessTypesetter): MathFont =
    t.selectFont("lmroman", 14, Set("regular"))
    new MathFont(t, t.currentFont, None)

  private val em    = 14.0
  private val thin  = 3.0 / 18 * em
  private val thick = 5.0 / 18 * em
  private val quad  = em

  /** Run a formula through the processor in math mode and hand back the horizontal list it translated to —
    * the boxes and glue that actually get set, inter-atom spacing and all. */
  private def boxesOf(formula: String): Vector[Box] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)

    registerTypesettingPrimitives(proc, handler)

    val m = new MathMode(t, mathFont(t))

    t.push(m)
    proc.process(formula)
    m.result.asInstanceOf[HBox].boxes.toVector

  /** The upright text a row of glyphs sets — how an operator name reaches the page. */
  private def text(b: Box): String = b.asInstanceOf[HBox].boxes.map(_.asInstanceOf[GlyphBox].glyph.toChar).mkString

  private def glyph(b: Box): Char  = b.asInstanceOf[GlyphBox].glyph.toChar
  private def glue(b: Box): Double = b.asInstanceOf[Glue].naturalSize

  // --- operator names --------------------------------------------------------

  "the inverse trigonometric operators are built in, upright, with scripts to the side" in {
    val mf = mathFont(new HeadlessTypesetter)

    for name <- Seq("arcsin", "arccos", "arctan") do
      withClue(s"\\$name: ") {
        MathSymbols.commandNode(mf, name) match
          case Some(a: MathAtom) =>
            a.cls shouldBe MathClass.Op
            text(a.nucleus) shouldBe name // upright letters, not a run of italic variables
            a.limits shouldBe Some(false) // log-like: \arcsin^2 x keeps the 2 beside, not above
          case other => fail(s"expected an Op atom, got $other")
      }
  }

  // \Pr is the odd one of the four: LaTeX sets it with limits, like \max and \det, so a display stacks its
  // subscript underneath rather than setting it to the side.
  "the probability operator is built in and takes limits, unlike the trigonometric ones" in {
    val mf = mathFont(new HeadlessTypesetter)

    MathSymbols.commandNode(mf, "Pr") match
      case Some(a: MathAtom) =>
        a.cls shouldBe MathClass.Op
        text(a.nucleus) shouldBe "Pr"
        a.limits shouldBe None // limit-like: the default, which a display stacks
      case other => fail(s"expected an Op atom, got $other")
  }

  "an operator name sets as one upright row taking an operator's spacing" in {
    // Were the letters ordinary variables there would be six italic glyphs with no space before the x.
    val bs = boxesOf("\\arcsin x")

    bs should have length 3
    text(bs(0)) shouldBe "arcsin"
    glue(bs(1)) shouldBe (thin +- 0.001) // Op followed by Ord
    glyph(bs(2)) should not be 'x'       // the variable is set from the mathematical-italic block
  }

  // --- modular forms ---------------------------------------------------------

  // \bmod is the binary operator of "a mod b", so it earns the medium space of a Bin atom on either side. An
  // Op-class "mod" would take an operator's spacing instead, which is wrong between two operands.
  "\\bmod is an upright binary operator" in {
    val mf = mathFont(new HeadlessTypesetter)

    MathSymbols.commandNode(mf, "bmod") match
      case Some(a: MathAtom) =>
        a.cls shouldBe MathClass.Bin
        text(a.nucleus) shouldBe "mod"
      case other => fail(s"expected a Bin atom, got $other")
  }

  "\\pmod sets a quad of space, then a parenthesised upright mod and its modulus" in {
    val bs = boxesOf("\\pmod{m}")

    glue(bs(0)) shouldBe (quad +- 0.001) // sets the modulus off from the formula
    glyph(bs(1)) shouldBe '('
    text(bs(2)) shouldBe "mod"
    glyph(bs.last) shouldBe ')'
  }

  "\\mod drops the parentheses and \\pod drops the word" in {
    val md = boxesOf("\\mod{m}")

    glue(md(0)) shouldBe (quad +- 0.001)
    text(md(1)) shouldBe "mod"
    md.collect { case g: GlyphBox => g.glyph.toChar } should not contain ')'

    val pd = boxesOf("\\pod{m}")

    glue(pd(0)) shouldBe (quad +- 0.001)
    glyph(pd(1)) shouldBe '('
    glyph(pd(3)) shouldBe ')'
    pd.count(_.isInstanceOf[HBox]) shouldBe 1 // the modulus alone — no "mod" row
  }

  "the modulus is a sub-formula, not a literal, so it may be any expression" in {
    val plain = boxesOf("\\pmod{m}")
    val expr  = boxesOf("\\pmod{2^n}")

    // same shape, but the modulus box is wider because it carries a superscripted expression
    expr should have length plain.length
    expr(5).width should be > plain(5).width
  }

  // --- implication arrows ----------------------------------------------------

  "an implication arrow is a heavy arrow with a thick space on either side" in {
    for (name, cp) <- Seq("implies" -> '⇒', "impliedby" -> '⇐', "iff" -> '⇔') do
      withClue(s"\\$name: ") {
        val bs = boxesOf(s"\\$name")

        bs should have length 3
        glue(bs(0)) shouldBe (thick +- 0.001)
        glyph(bs(1)) shouldBe cp
        glue(bs(2)) shouldBe (thick +- 0.001)
      }
  }

  // The extra space is the whole point: an arrow is already a relation and earns thickspace from the inter-atom
  // spacing, so a connective between statements comes out wider than an ordinary relation in the same formula.
  "a connective is set wider than a plain relation" in {
    def spacing(formula: String) = boxesOf(formula).collect { case g: Glue => g.naturalSize }.sum

    spacing("a \\implies b") shouldBe (4 * thick +- 0.001)
    spacing("a \\Rightarrow b") shouldBe (2 * thick +- 0.001)
    spacing("a \\implies b") should be > spacing("a \\Rightarrow b")
  }

  // --- the package is gone ---------------------------------------------------

  "none of it needs a package" in {
    // The formula that used to require \use{math}, with nothing loaded at all.
    noException should be thrownBy boxesOf("\\arcsin x \\implies \\Pr(x) \\bmod 2 \\pmod{n}")
  }

  "the ones that take an argument are math-mode commands, and say so outside math mode" in {
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)

    registerTypesettingPrimitives(proc, handler)

    for name <- Seq("pmod{m}", "mod{m}", "pod{m}", "implies", "impliedby", "iff") do
      withClue(s"\\$name: ") {
        val e = the[TexishException] thrownBy proc.process(s"\\$name")

        e.getMessage should include("math mode")
      }
  }
