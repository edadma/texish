package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 2 of math support: atom classification and inter-atom spacing. These run on the fixed-metric
  * [[StubTypesetter]] (every glyph 6 wide), so the geometry is exact and font-independent: a math list is
  * built by hand, translated, and the resulting horizontal list of boxes and glue inspected. The default
  * font is 14pt, so one em is 14 and the math spaces are 3mu/4mu/5mu of that (thin/medium/thick).
  */
class MathModeTests extends AnyFreeSpec with Matchers:

  // a math font over the stub's current (text) font: no MATH table, fixed 6-wide glyphs
  def mathFont(t: StubTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  def mathBox(build: MathMode => Unit): HBox =
    val t = new StubTypesetter
    val m = new MathMode(t, mathFont(t))
    build(m)
    m.result.asInstanceOf[HBox]

  val em   = 14.0
  val thin = 3.0 / 18 * em // 2.333…
  val med  = 4.0 / 18 * em // 3.111…
  val thick = 5.0 / 18 * em // 3.888…

  "a binary operator gets medium space on both sides: a+b" in {
    val box = mathBox { m => m.addChar('a'); m.addChar('+'); m.addChar('b') }

    box.boxes should have length 5 // a, glue, +, glue, b
    box.boxes(0) shouldBe a[GlyphBox]
    box.boxes(2) shouldBe a[GlyphBox]
    box.boxes(4) shouldBe a[GlyphBox]
    box.boxes(1).asInstanceOf[Glue].naturalSize shouldBe (med +- 0.001)
    box.boxes(3).asInstanceOf[Glue].naturalSize shouldBe (med +- 0.001)
    box.width shouldBe (6 + med + 6 + med + 6 +- 0.001)
  }

  "a relation gets thick space on both sides: a=b" in {
    val box = mathBox { m => m.addChar('a'); m.addChar('='); m.addChar('b') }

    box.boxes should have length 5
    box.boxes(1).asInstanceOf[Glue].naturalSize shouldBe (thick +- 0.001)
    box.boxes(3).asInstanceOf[Glue].naturalSize shouldBe (thick +- 0.001)
  }

  "two ordinary atoms touch with no space: variables ab" in {
    val box = mathBox { m => m.addChar('a'); m.addChar('b') }

    box.boxes should have length 2 // no glue between two Ord atoms
    box.width shouldBe (12.0 +- 0.001)
  }

  "a leading minus is unary — no surrounding space: -a" in {
    // the Bin '-' begins the list, so normalization demotes it to Ord; Ord-Ord spacing is zero
    val box = mathBox { m => m.addChar('-'); m.addChar('a') }

    box.boxes should have length 2
    box.width shouldBe (12.0 +- 0.001)
  }

  "a minus after a relation is unary — no binary space after it: a=-b" in {
    val box = mathBox { m => m.addChar('a'); m.addChar('='); m.addChar('-'); m.addChar('b') }

    // a, thick, =, thick, -, b  →  the '-' is demoted to Ord by the preceding relation, so the minus and b
    // sit flush (Ord-Ord = 0); the thick space before the minus is the relation's, not the operator's. Had
    // the minus stayed Bin, a medium space would separate it from b.
    box.boxes should have length 6
    box.boxes(1).asInstanceOf[Glue].naturalSize shouldBe (thick +- 0.001)
    box.boxes(3).asInstanceOf[Glue].naturalSize shouldBe (thick +- 0.001)
    box.boxes(4) shouldBe a[GlyphBox] // the minus
    box.boxes(5) shouldBe a[GlyphBox] // b, with no glue before it
  }

  "a binary operator before a closing fence is demoted: a+) " in {
    val box = mathBox { m => m.addChar('a'); m.addChar('+'); m.addChar(')') }

    // '+' followed by Close is reclassified Ord; Ord-Ord then Ord-Close are both zero
    box.boxes should have length 3
    box.width shouldBe (18.0 +- 0.001)
  }

  "a trailing binary operator is demoted: a+" in {
    val box = mathBox { m => m.addChar('a'); m.addChar('+') }

    box.boxes should have length 2
    box.width shouldBe (12.0 +- 0.001)
  }

  "an explicit thin space is emitted and suppresses no automatic space: a\\,b" in {
    val box = mathBox { m => m.addChar('a'); m.addCommand(",") shouldBe true; m.addChar('b') }

    // two Ord atoms (auto space 0) with the author's thin space between them
    box.boxes should have length 3
    box.boxes(1).asInstanceOf[Glue].naturalSize shouldBe (thin +- 0.001)
  }

  "operator names set upright as a single Op atom: \\sin" in {
    val box = mathBox { m => m.addCommand("sin") shouldBe true; m.addChar('x') }

    // \sin (Op) then x (Ord): Op-Ord spacing is thin
    box.boxes should have length 3
    box.boxes(0) shouldBe a[HBox]                 // the three-glyph "sin"
    box.boxes(0).width shouldBe (18.0 +- 0.001)   // s, i, n at 6 each
    box.boxes(1).asInstanceOf[Glue].naturalSize shouldBe (thin +- 0.001)
  }

  "the symbol table classifies control sequences correctly" in {
    val t  = new StubTypesetter
    val mf = mathFont(t)

    def cls(name: String) = MathSymbols.commandNode(mf, name).map { case a: MathAtom => a.cls; case _ => null }

    cls("leq")   shouldBe Some(MathClass.Rel)
    cls("sum")   shouldBe Some(MathClass.Op)
    cls("alpha") shouldBe Some(MathClass.Ord)
    cls("times") shouldBe Some(MathClass.Bin)
    MathSymbols.commandNode(mf, "notarealsymbol") shouldBe None
  }

  "single characters are classified by the character table" in {
    val t  = new StubTypesetter
    val mf = mathFont(t)

    def cls(c: Char) = MathSymbols.charNode(mf, c.toInt).map { case a: MathAtom => a.cls; case _ => null }

    cls('+') shouldBe Some(MathClass.Bin)
    cls('=') shouldBe Some(MathClass.Rel)
    cls('(') shouldBe Some(MathClass.Open)
    cls(')') shouldBe Some(MathClass.Close)
    cls(',') shouldBe Some(MathClass.Punct)
    cls('a') shouldBe Some(MathClass.Ord)
    cls('7') shouldBe Some(MathClass.Ord)
  }

  "translating draws each atom through the glyph seam at the spaced positions" in {
    val rec = new RecordingGlyphTypesetter
    val m   = new MathMode(rec, new MathFont(rec, rec.currentFont, None))

    m.addChar('a'); m.addChar('+'); m.addChar('b')
    rec.draw(m.result.asInstanceOf[HBox])

    rec.drawn.map(_._1) should have length 3 // one glyph per atom — the glue does not draw
    val xs = rec.drawn.map(_._2)
    xs(0) shouldBe (0.0 +- 0.001)
    xs(1) shouldBe (6 + med +- 0.001)
    xs(2) shouldBe (6 + med + 6 + med +- 0.001)
  }
