package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The string functions as *values*, which is how a package uses them: `\set x {\upcase{ab}}`, `\words{\moves}`,
  * `\if {\= {\trim{\s}} {…}}`. Two things have to hold for that, and neither is visible from the text a function
  * writes where it stands.
  *
  * The first is that the function sets a result at all. Without one, expression evaluation falls back to the
  * argument's own tokens — so `\upcase` in a `\set` yields the text it was given, unchanged. That reads as the
  * operation having run and done nothing, which is the worst way for it to fail.
  *
  * The second is that a literal argument stays the characters it was written as. Value evaluation reads a lone
  * text token as the number it parses as, and `Double.parseDouble` accepts far more than a document author has in
  * mind: "1.e4" is 1.0 × 10⁴. A string function handed those characters must split, case or measure *them*, not
  * the number — that is the whole point of a verbatim `<name>` argument, whose documentation names \words as the
  * way to consume it.
  */
class StringFunctionValueTests extends AnyFreeSpec with Matchers:

  private def run(src: String): StringHandler =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h

  private def value(src: String): String = Value.display(run(src).get("x"))

  "the case and whitespace functions yield a value, not only text" - {
    "\\upcase in expression position" in { value("\\set x {\\upcase{ab}}") shouldBe "AB" }
    "\\downcase in expression position" in { value("\\set x {\\downcase{AB}}") shouldBe "ab" }
    "\\trim in expression position" in { value("\\set x {\\trim{  ab  }}") shouldBe "ab" }

    "a comparison sees the converted string" in {
      value("\\set x {\\= {\\downcase{Q}} {q}}") shouldBe "true"
    }

    "the text they write where they stand is unchanged" in {
      run("\\upcase{ab}\\downcase{AB}\\trim{ c }").result shouldBe "ABabc"
    }
  }

  "a literal argument to a string function is its characters, not the number they parse as" - {
    // The case that found this: a move list is split into words before anything looks at it, and a line of one
    // move is a single text token. Read as a value, "1.e4" is 10000.
    "\\words of a lone move" in {
      run("\\for\\w{\\words{1.e4}}{[\\w]}").result shouldBe "[1.e4]"
    }
    "\\words through a verbatim parameter" in {
      run("\\def g <m> {\\for\\w{\\words{\\m}}{[\\w]}}\\g{1.e4}").result shouldBe "[1.e4]"
    }
    "\\size counts the characters written" in {
      value("\\set x {\\size{1.e4}}") shouldBe "4"
      value("\\set x {\\size{007}}") shouldBe "3"
    }
    "\\cat joins what was written" in {
      value("\\set x {\\cat{1.e4}{!}}") shouldBe "1.e4!"
    }
    "\\upcase keeps a leading zero" in {
      value("\\set x {\\upcase{007a}}") shouldBe "007A"
    }

    // Only a run of literal characters is protected. A variable, a nested call and a sequence are values and
    // still arrive as values, so \size of a sequence is its length rather than the length of its display.
    "a variable's value still arrives as a value" in {
      value("\\set n {\\calc{2 + 3}}\\set x {\\size{\\seq{a b c}}}") shouldBe "3"
      value("\\set n {12.50}\\set x {\\cat{\\n}{}}") shouldBe "12.5"
    }
  }
