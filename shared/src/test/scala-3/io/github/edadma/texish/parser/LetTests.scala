package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `\let newname oldname` copies the *current* meaning of `oldname` to `newname`: a macro, a variable, or a
  * built-in primitive. The copy is a snapshot — redefining the source afterwards leaves the alias alone — and a
  * macro/variable alias is scope-aware (local by default, global under `\global`).
  */
class LetTests extends AnyFreeSpec with Matchers:

  private def run(src: String): StringHandler =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h

  "\\let aliases a macro" in {
    run("\\def foo {bar}\\let baz foo\\baz").result shouldBe "bar"
  }

  "the alias is a snapshot — redefining the source does not change it" in {
    run("\\def foo {one}\\let baz foo\\def foo {two}\\baz-\\foo").result shouldBe "one-two"
  }

  "\\let aliases a variable's value" in {
    val h = run("\\set x {5}\\let y x")
    Value.display(h.get("y")) shouldBe "5"
  }

  "\\let aliases a built-in primitive (source named as a control sequence)" in {
    run("\\let plus \\+\\plus 2 3").result shouldBe "5"
  }

  "a macro alias is local to its group by default" in {
    val h = run("\\def foo {x}{\\let baz foo}")
    h.get("baz") shouldBe Value.Undefined
  }

  "\\global\\let escapes the group" in {
    run("\\def foo {x}{\\global\\let baz foo}\\baz").result shouldBe "x"
  }

  "\\let of a name with no meaning is an error" in {
    a[ParserException] should be thrownBy run("\\let a nothingHere")
  }
