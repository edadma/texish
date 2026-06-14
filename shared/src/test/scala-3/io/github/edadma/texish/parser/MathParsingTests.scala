package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.{MathMode, StubTypesetter}
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
    t.mode shouldBe a[io.github.edadma.texish.HorizontalMode]
    t.mode should not be a[MathMode]
  }

  "^ and _ build scripts inside math without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$x^2 + y_i - z^a_b$")
  }

  "a script grabs only one character: x^2y leaves y as a following atom" in {
    val (_, proc) = fixture()
    proc.pushTokenizer(Tokenizer("2y", proc.activeChars))
    val field = proc.readScriptField(CharReader.fromString(""))

    field.collect { case Token.Text(s, _) => s } shouldBe Vector("2")
    proc.nextToken() match
      case Token.Text(s, _) => s shouldBe "y" // the remainder was pushed back to be read normally
      case other            => fail(s"expected the leftover 'y', got $other")
  }

  "a braced script field is the whole group; a control sequence is one token" in {
    val (_, proc) = fixture()

    proc.pushTokenizer(Tokenizer("{a+b}c", proc.activeChars))
    val braced = proc.readScriptField(CharReader.fromString(""))
    braced.head shouldBe a[Token.BeginGroup]
    braced.last shouldBe a[Token.EndGroup]

    proc.pushTokenizer(Tokenizer("\\alpha x", proc.activeChars))
    proc.readScriptField(CharReader.fromString("")) match
      case Vector(Token.ControlSeq(name, _)) => name shouldBe "alpha"
      case other                             => fail(s"expected a single \\alpha token, got $other")
  }

  "a double superscript on one atom is reported as an error" in {
    val (_, proc) = fixture()
    val ex = the[ParserException] thrownBy proc.process("$x^2^3$")
    ex.getMessage should include("double superscript")
  }

  "^ in ordinary text is the literal character, not a script marker" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("the caret x^2 and underscore y_i in prose")
  }

  "\\frac builds a fraction inside math without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\frac{a + b}{c} + \\frac12$")
  }

  "\\frac outside math is reported as a math-only command" in {
    val (_, proc) = fixture()
    val ex = the[ParserException] thrownBy proc.process("\\frac{a}{b}")
    ex.getMessage should include("math mode")
  }

  "\\sqrt builds a radical inside math without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\sqrt{x + 1} + \\sqrt{\\frac{a}{b}}$")
  }

  "\\sqrt[3]{x} builds a cube root (an optional degree) without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\sqrt[3]{x} + \\sqrt[n+1]{x} + \\sqrt{y}$")
  }

  "\\sqrt outside math is reported as a math-only command" in {
    val (_, proc) = fixture()
    val ex = the[ParserException] thrownBy proc.process("\\sqrt{x}")
    ex.getMessage should include("math mode")
  }

  "\\left … \\right builds a delimited sub-formula inside math without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\left( \\frac{a}{b} \\right) + \\left[ x \\right]$")
  }

  "a null delimiter (\\left.) is accepted" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\left. \\frac{a}{b} \\right|$")
  }

  "control-sequence delimiters (\\langle … \\rangle) stretch too" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\left\\langle x \\right\\rangle$")
  }

  "nested \\left … \\right pairs balance" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\left( \\left[ x \\right] \\right)$")
  }

  "\\left without a matching \\right is reported" in {
    val (_, proc) = fixture()
    val ex = the[ParserException] thrownBy proc.process("$\\left( x $")
    ex.getMessage should include("\\right")
  }

  "\\right standing alone is reported" in {
    val (_, proc) = fixture()
    val ex = the[ParserException] thrownBy proc.process("$ x \\right)$")
    ex.getMessage should include("\\left")
  }

  "\\sum\\limits with bounds, and \\lim\\limits, lay out without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\sum\\limits_{i=1}^{n} i + \\lim\\limits_{x} f + \\int_0^1 g$")
  }

  "\\limits after something that is not an operator is reported" in {
    val (_, proc) = fixture()
    val ex = the[ParserException] thrownBy proc.process("$x\\limits_2$")
    ex.getMessage should include("operator")
  }

  "math accents (\\hat, \\widehat) lay out without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$\\hat{x} + \\vec{v} + \\widehat{ABC} + \\bar{y}$")
  }

  "an accent outside math is reported as a math-only command" in {
    val (_, proc) = fixture()
    val ex = the[ParserException] thrownBy proc.process("\\hat{x}")
    ex.getMessage should include("math mode")
  }

  "$$ … $$ opens and closes a display without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("Before. $$x + y = z$$ After.")
  }

  "after a display the math mode is left and we are back in vertical (paragraph-breaking) context" in {
    val (t, proc) = fixture()
    proc.process("$$x + y$$")
    // a display breaks the paragraph and rides the vertical list, so we are not in a math or horizontal mode
    t.mode should not be a[MathMode]
    t.mode should not be a[io.github.edadma.texish.HorizontalMode]
  }

  "a display with big operators and fractions lays out without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$$\\sum_{i=1}^{n} \\frac{1}{i} + \\int_0^1 f$$")
  }

  "\\eqno numbers a display without error" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$$a^2 + b^2 = c^2 \\eqno{(3.1)}$$")
  }

  "\\eqno outside display math is reported" in {
    val (_, proc) = fixture()
    val ex = the[ParserException] thrownBy proc.process("$x \\eqno{(1)}$") // inline, not a display
    ex.getMessage should include("display math")
  }
