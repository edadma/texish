package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** A variable that holds a dimension *string* — most often an element pulled from a `\words` sequence, which is
  * text-typed — must coerce to its size in points wherever a number is expected, exactly as the dimension literal
  * does. `\calc{1in}` is 72, so a variable holding `"1in"` must read as 72 in `\calc` and in a picture coordinate,
  * not be rejected as an unknown name. This is what lets a package read a coordinate like `at 1in 3in` apart with
  * `\words` and compute with the pieces. Font-relative `em`/`ex` are not resolved here, where no font is in scope.
  */
class DimensionStringTests extends AnyFreeSpec with Matchers:

  private def run(src: String): StringHandler =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h

  "Value.number coerces a text value" - {
    "an absolute-unit dimension string gives its size in points" in {
      Value.number(Value.Text("1in")) shouldBe Some(72.0)
      Value.number(Value.Text("72pt")) shouldBe Some(72.0)
      Value.number(Value.Text("2.54cm")) shouldBe Some(72.0)
      Value.number(Value.Text("25.4mm")) shouldBe Some(72.0)
      Value.number(Value.Text("1pc")) shouldBe Some(12.0)
    }

    "a plain numeric string still coerces" in {
      Value.number(Value.Text("5")) shouldBe Some(5.0)
      Value.number(Value.Text("-2.5")) shouldBe Some(-2.5)
    }

    "a non-numeric string is not a number" in {
      Value.number(Value.Text("hello")) shouldBe None
      Value.number(Value.Text("#808080")) shouldBe None
    }

    "a font-relative unit is left for a font-aware context" in {
      Value.number(Value.Text("1em")) shouldBe None
    }
  }

  "in a document" - {
    "a dimension string pulled from \\words is usable in \\calc" in {
      // the element is text-typed; before the coercion this threw "unknown name 'x'"
      val h = run("\\set ws {\\words{at 1in 3in}}\\set x {\\head{\\tail{\\ws}}}\\set r {\\calc{\\x * 2}}")
      Value.display(h.get("r")) shouldBe "144"
    }

    "the variable and the literal agree" in {
      run("\\calc{1in + 1in}").result shouldBe "144"
      val h = run("\\set a {\\head{\\words{1in}}}\\set b {\\head{\\words{1in}}}\\set r {\\calc{\\a + \\b}}")
      Value.display(h.get("r")) shouldBe "144"
    }
  }
