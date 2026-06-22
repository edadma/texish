package io.github.edadma.texish.parser

import io.github.edadma.texish.HeadlessTypesetter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The `math` package (packages/math.texish), exercised through the full parser. The package's job is the
  * amsmath name macros the engine does not define on its own — the remaining inverse and probability operators,
  * the modular forms, and the implication arrows — so these check that each expands and typesets inside math
  * without error after `\use{math}`. The thin-space-before-a-paren case doubles as a regression for the
  * tokenizer's comma rule that the package's `\pmod` depends on.
  */
class MathPackageTests extends AnyFreeSpec with Matchers:

  private def render(body: String): Unit =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.process(s"\\use{math}$body")

  "the package's operator macros expand inside math" in {
    noException should be thrownBy render("$\\arcsin x + \\arccos x + \\arctan x$")
    noException should be thrownBy render("$\\Pr[X = x]$")
  }

  "the modular forms expand inside math" in {
    noException should be thrownBy render("$a \\bmod n$")
    noException should be thrownBy render("$x \\equiv y \\pmod{n}$")
    noException should be thrownBy render("$x \\equiv y \\mod{n} \\pod{n}$")
  }

  "the implication arrows expand inside math" in {
    noException should be thrownBy render("$p \\implies q \\iff r \\impliedby s$")
  }

  "\\dots is an alias for the low ellipsis" in {
    noException should be thrownBy render("$a_1, a_2, \\dots, a_n$")
  }

  "a thin space sits directly before a parenthesis (the tokenizer comma rule)" in {
    // \pmod relies on \, immediately preceding `(`; this is the construct that fails if `\,(` merges into one
    // control sequence named `,(`.
    noException should be thrownBy render("$\\dfrac{n!}{k!\\,(n - k)!}$")
  }
