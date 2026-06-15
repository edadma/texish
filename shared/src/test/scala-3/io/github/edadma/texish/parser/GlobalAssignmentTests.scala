package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `\global` makes the following assignment escape its group: a plain `\set`/`\def` is local and vanishes when the
  * group closes, while a `\global` one (or `\gdef`) is still set afterwards. Tested with a StringHandler, whose
  * groups drop locals on exit, so the local/global difference is directly observable.
  */
class GlobalAssignmentTests extends AnyFreeSpec with Matchers:

  private def run(src: String): StringHandler =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h

  "\\global\\set survives the group it is made in; a plain \\set does not" in {
    val h = run("{\\set a {x}\\global\\set b {y}}")
    h.get("a") shouldBe Value.Undefined        // local: gone once the group closes
    Value.display(h.get("b")) shouldBe "y"     // global: still set
  }

  "\\global affects only the next assignment, then reverts to local" in {
    val h = run("{\\global\\set a {x}\\set b {y}}")
    Value.display(h.get("a")) shouldBe "x"     // made global
    h.get("b") shouldBe Value.Undefined        // the flag did not leak: b stayed local
  }

  "\\gdef defines a macro globally" in {
    val h = run("{\\gdef foo {bar}}")
    h.get("foo") shouldBe a[Value.Macro]
  }

  "a plain \\def does not survive its group" in {
    val h = run("{\\def foo {bar}}")
    h.get("foo") shouldBe Value.Undefined
  }

  "\\global\\def survives its group and expands afterwards" in {
    val h = run("{\\global\\def foo {bar}}\\foo")
    h.result shouldBe "bar"
  }
