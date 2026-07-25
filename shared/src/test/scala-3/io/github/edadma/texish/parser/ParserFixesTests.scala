package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.{HeadlessTypesetter, MathFont, MathMode, TexishException}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Regression tests for the parser/processor defects found in the July 2026 whole-codebase review: silent
  * token loss in expression evaluation, greedy symbolic control-sequence lexing, brace-blind `\seq`/`\map`
  * item splitting, CRLF line endings, `\=`/`\!=` coercion, `\ifx` space handling, and the smaller findings
  * (computed `\range` bounds, `\the` as an expression, map iteration order, `\counterwithin` cycles,
  * surrogate-pair splitting, stray `}` reporting, and suppression restore after an error). */
class ParserFixesTests extends AnyFreeSpec with Matchers:

  private def process(input: String): String =
    val handler = new StringHandler
    val proc    = new Processor(handler)
    proc.process(input)
    handler.result

  private def valueOf(input: String, name: String): Value =
    val handler = new StringHandler
    val proc    = new Processor(handler)
    proc.process(input)
    handler.get(name)

  "expression evaluation keeps every token" - {
    "a variable followed by literal text interpolates instead of dropping the tail" in {
      valueOf("\\set x {12}\\set y {\\x tail}", "y") shouldBe Value.Text("12 tail")
    }

    "a variable referenced mid-value interpolates its display" in {
      valueOf("\\set name {World}\\set greeting {Hello \\name}", "greeting") shouldBe Value.Text("Hello World")
    }

    "a tail of nothing but whitespace does not turn a number into text" in {
      valueOf("\\set n {5}\\set m {\\n }", "m") shouldBe Value.Num(5)
    }
  }

  "\\seq and \\map items split only at top-level spaces" - {
    "a braced \\seq item keeps its interior space" in {
      valueOf("\\set s {\\seq{{a b} c}}", "s") shouldBe Value.Seq(Vector(Value.Text("a b"), Value.Text("c")))
    }

    "a \\map literal can hold a space-containing value" in {
      valueOf("\\set m {\\map{name {Ada Lovelace} age 36}}", "m") match
        case Value.Map(entries) =>
          entries("name") shouldBe Value.Text("Ada Lovelace")
          entries("age") shouldBe Value.Num(36)
        case other => fail(s"expected a map, got $other")
    }
  }

  "CRLF sources tokenize like LF sources" - {
    "a CRLF line end is one newline, not a space and a newline" in {
      process("one\r\ntwo") shouldBe process("one\ntwo")
    }

    "a CRLF blank line still reads as two consecutive newlines" in {
      process("a\r\n\r\nb") shouldBe process("a\n\nb")
    }
  }

  "\\= and \\!= coerce numeric text like the ordering comparisons" - {
    "a text-typed digit equals the same number" in {
      // \for over a string yields Text items ({ab5} stays text; a bare {5} would already evaluate to a
      // number); before the fix Text("5") vs Num(5) was silently unequal
      process("\\for \\c {ab5} {\\if{\\={\\c}{5}}{yes}\\fi}") shouldBe "yes"
    }

    "\\!= stays the exact negation under coercion" in {
      process("\\for \\c {ab5} {\\if{\\!={\\c}{5}}{x}\\fi}") shouldBe "xx"
    }
  }

  "\\ifx skips the interword spaces around its operands" in {
    process("\\ifx \\a \\a{T}\\fi") shouldBe "T"
    process("\\ifx \\a \\b{T}\\fi") shouldBe ""
  }

  "\\range accepts computed bounds" in {
    process("\\for \\i {\\range{1}{\\+{1}{2}}} {\\i}") shouldBe "123"
  }

  "\\the sets a result usable in an expression" in {
    valueOf("\\set p {42}\\set x {\\the\\p}", "x") shouldBe Value.Num(42)
  }

  "\\for over a \\map visits entries in declaration order" in {
    // six entries force a real hash map past the small-map cases that happen to preserve order
    process("\\for \\e {\\map{a 1 b 2 c 3 d 4 e 5 f 6}} {\\e.key}") shouldBe "abcdef"
  }

  "a \\counterwithin cycle terminates instead of hanging" in {
    noException should be thrownBy process(
      "\\newcounter{a}\\newcounter{b}\\counterwithin{a}{b}\\counterwithin{b}{a}\\stepcounter{a}",
    )
  }

  "surrogate pairs survive the character-splitting primitives" - {
    "\\head, \\tail and \\last work in code points" in {
      valueOf("\\set h {\\head{𝜋x}}", "h") shouldBe Value.Text("𝜋")
      valueOf("\\set t {\\tail{𝜋x}}", "t") shouldBe Value.Text("x")
      valueOf("\\set l {\\last{x𝜋}}", "l") shouldBe Value.Text("𝜋")
    }

    "\\for over a string iterates whole symbols" in {
      process("\\for \\c {a𝜋b} {[\\c]}") shouldBe "[a][𝜋][b]"
    }

    "a math script field takes a whole astral symbol" in {
      val t       = new HeadlessTypesetter
      val handler = new TypesetterHandler(t)
      val proc    = new Processor(handler)
      registerTypesettingPrimitives(proc, handler)
      proc.pushTokenizer(Tokenizer("𝜋x", proc.activeChars))
      val field = proc.readScriptField(CharReader.fromString(""))
      field.collect { case Token.Text(s, _) => s } shouldBe Vector("𝜋")
      proc.nextToken() match
        case Token.Text(s, _) => s shouldBe "x"
        case other            => fail(s"expected the leftover 'x', got $other")
    }

    "math text routes whole code points into the math list" in {
      val t       = new HeadlessTypesetter
      val handler = new TypesetterHandler(t)
      val proc    = new Processor(handler)
      registerTypesettingPrimitives(proc, handler)
      t.enterMath(false)
      val base = t.mode.asInstanceOf[MathMode]

      class RecordingMathMode extends MathMode(t, base.baseMathFont):
        val seen = ArrayBuffer[Int]()
        override def addChar(codepoint: Int): Unit = seen += codepoint

      val rec = new RecordingMathMode
      t.push(rec)
      handler.text("𝜋x")
      rec.seen.toVector shouldBe Vector(0x1d70b, 'x'.toInt)
    }
  }

  "a stray } is reported at its own position" in {
    val ex = the[TexishException] thrownBy process("abc}")
    ex.getMessage should include("Unmatched '}'")
  }

  "output suppression is restored when an expression raises" in {
    val handler = new StringHandler
    val proc    = new Processor(handler)
    an[Exception] should be thrownBy proc.process("\\set x {\\+{1}{\\seq{a}}}")
    proc.process("after")
    handler.result should include("after")
  }

