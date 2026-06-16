package io.github.edadma.texish.parser

import io.github.edadma.texish.{Font, HeadlessTypesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Unit-suffixed dimensions parse to Dimen, carried internally in big points (1/72 inch) — the engine's one canonical
  * unit on every backend.
  */
class DimensionTests extends AnyFreeSpec with Matchers:

  def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
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

  "em resolves against the current font size" in {
    val (t, proc) = fixture()
    t.currentFont = new Font("stub", 10, 6, 4.5, Set.empty, "stub", None, Set.empty)
    proc.process("\\set x {1.5em}")
    t.getVar("x") shouldBe Value.Dimen(15)
  }

  "ex resolves against the current font x-height" in {
    val (t, proc) = fixture()
    t.currentFont = new Font("stub", 10, 6, 4.5, Set.empty, "stub", None, Set.empty)
    proc.process("\\set x {2ex}")
    t.getVar("x") shouldBe Value.Dimen(9)
  }

  "em defaults to the host's startup font" in {
    // every Typesetter starts with a 14pt default font, so em works before any \font command
    val (t, proc) = fixture()
    proc.process("\\set x {1em}")
    t.getVar("x") shouldBe Value.Dimen(14)
  }

  "em without a font-aware host stays text" in {
    val handler = new StringHandler
    val proc    = new Processor(handler)
    proc.process("\\set x {1em}")
    handler.get("x") shouldBe Value.Text("1em")
  }

  "\\vskip and \\hskip accept unit-suffixed dimensions" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("\\vskip {12pt}")
    noException should be thrownBy proc.process("\\vskip {0.25in}")
    noException should be thrownBy proc.process("text \\hskip {3mm} more")
  }
