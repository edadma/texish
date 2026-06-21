package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** A `<name>` parameter of a `\def` macro is read verbatim at the call site, exactly as `\url`'s argument is: the
  * literal characters of the braced group, with no comment, escape or macro processing, bound to the parameter as a
  * single text value. This lets a macro receive a mini-language fragment whose specials (`|`, `::=`, `//`) would
  * otherwise be eaten, then parse it itself with the string primitives.
  */
class RawParamTests extends AnyFreeSpec with Matchers:

  private def run(src: String): StringHandler =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h

  "a <name> parameter is bound to its argument's literal text" in {
    run("\\def g <e> {[\\e]}\\g{a ::= b | c}").result shouldBe "[a ::= b | c]"
  }

  "verbatim specials that are normally markup survive a raw parameter" in {
    run("\\def g <e> {\\size{\\e}}\\g{//}").result shouldBe "2"
  }

  "a raw parameter preserves internal runs of spaces" in {
    run("\\def g <e> {\\size{\\e}}\\g{a  b}").result shouldBe "4"
  }

  "a raw parameter can follow a mandatory one" in {
    run("\\def g n <e> {\\n:\\e}\\g{1}{x|y}").result shouldBe "1:x|y"
  }

  "\\words splits a raw parameter on whitespace" in {
    run("\\def g <e> {\\for\\w{\\words{\\e}}{<\\w>}}\\g{ a ::= b | c }").result shouldBe "<a><::=><b><|><c>"
  }

  "\\words treats tabs and newlines as separators, collapsing runs" in {
    run("\\def g <e> {\\for\\w{\\words{\\e}}{<\\w>}}\\g{a\n\nb\tc}").result shouldBe "<a><b><c>"
  }

  "\\words of all-whitespace is the empty sequence" in {
    run("\\def g <e> {[\\for\\w{\\words{\\e}}{<\\w>}]}\\g{   }").result shouldBe "[]"
  }

  "\\message produces no typeset output" in {
    run("a\\message{tracing here}b").result shouldBe "ab"
  }
