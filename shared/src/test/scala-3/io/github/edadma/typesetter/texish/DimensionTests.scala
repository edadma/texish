package io.github.edadma.typesetter.texish

import io.github.edadma.typesetter.StubTypesetter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Unit-suffixed dimensions parse to Dimen, carried internally in big points (1/72 inch) — the engine's one canonical
  * unit on every backend.
  */
class DimensionTests extends AnyFreeSpec with Matchers:

  def fixture(): (StubTypesetter, Processor) =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  "points pass through unchanged" in {
    val (t, proc) = fixture()
    proc.process("\\set x {12pt}")
    t.getNumber("x") shouldBe 12.0
    t.getVar("x") shouldBe Value.Dimen(12)
  }

  "inches convert to points" in {
    val (t, proc) = fixture()
    proc.process("\\set x {0.5in}")
    t.getNumber("x") shouldBe 36.0
  }

  "picas convert to points" in {
    val (t, proc) = fixture()
    proc.process("\\set x {2pc}")
    t.getNumber("x") shouldBe 24.0
  }

  "millimetres convert to points" in {
    val (t, proc) = fixture()
    proc.process("\\set x {3mm}")
    t.getNumber("x") shouldBe 8.503937007874017 +- 1e-9
  }

  "centimetres convert to points" in {
    val (t, proc) = fixture()
    proc.process("\\set x {2.54cm}")
    t.getNumber("x") shouldBe 72.0 +- 1e-9
  }

  "negative dimensions parse" in {
    val (t, proc) = fixture()
    proc.process("\\set x {-3pt}")
    t.getNumber("x") shouldBe -3.0
  }

  "bare numbers stay Num (meaning points)" in {
    val (t, proc) = fixture()
    proc.process("\\set x {10}")
    t.getVar("x") shouldBe Value.Num(10)
    t.getNumber("x") shouldBe 10.0
  }

  "non-dimension text stays Text" in {
    val (t, proc) = fixture()
    proc.process("\\set x {pointer}")
    t.getVar("x") shouldBe Value.Text("pointer")
  }

  "whole dimensions display without a decimal point" in {
    Value.display(Value.Dimen(36.0)) shouldBe "36pt"
    Value.display(Value.Dimen(1.5)) shouldBe "1.5pt"
  }

  "\\vskip and \\hskip accept unit-suffixed dimensions" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("\\vskip {12pt}")
    noException should be thrownBy proc.process("\\vskip {0.25in}")
    noException should be thrownBy proc.process("text \\hskip {3mm} more")
  }
