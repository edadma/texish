package io.github.edadma.typesetter.parser

import io.github.edadma.typesetter.{MathMode, StubTypesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 2 language wiring: the `$` active character toggling math mode, and the routing of text and
  * control sequences into the math list while it is open. The geometry is covered by the engine-level
  * MathModeTests; here we exercise the parser path end to end.
  */
class MathParsingTests extends AnyFreeSpec with Matchers:

  def fixture(): (StubTypesetter, Processor) =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  "$ opens and closes inline math without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("Let $a + b = c$ hold.")
  }

  "math mode accepts control-sequence symbols" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\alpha + \\beta \\leq \\gamma$")
  }

  "an unknown control sequence in math is reported as an unknown math symbol" in {
    val (_, proc) = fixture()
    val ex = the[ParserException] thrownBy proc.process("$\\notarealsymbol$")
    ex.getMessage should include("Unknown math symbol")
  }

  "the same control sequence is a math symbol in math and an error in text" in {
    val (_, proc) = fixture()
    // \leq is only meaningful inside math; outside, it is an unknown command
    the[ParserException] thrownBy proc.process("\\leq") // text mode: unknown command
    noException should be thrownBy {
      val (_, p2) = fixture()
      p2.process("$\\leq$")
    }
  }

  "math is left when the closing $ is seen — following text routes normally" in {
    val (t, proc) = fixture()
    proc.process("$x$ and then ordinary text")
    // after the closing $, we are back in a horizontal (paragraph) text mode, not math
    t.mode shouldBe a[io.github.edadma.typesetter.HorizontalMode]
    t.mode should not be a[MathMode]
  }
