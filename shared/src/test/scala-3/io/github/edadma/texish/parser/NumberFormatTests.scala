package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The number-to-string formatters that label numbered things — `\arabic`/`\roman`/`\Roman`/`\alph`/`\Alph`/
  * `\fnsymbol`. The pure functions are tested directly across edge cases (4=iv, 9=ix, 1990=mcmxc; 27=aa), and the
  * primitives are tested end-to-end through a StringHandler, including composing inside `\set` (they set a result)
  * and reading a computed number argument.
  */
class NumberFormatTests extends AnyFreeSpec with Matchers:

  private def run(src: String): StringHandler =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h

  "the pure formatters" - {
    "arabic is the decimal string" in {
      NumberFormat.arabic(0) shouldBe "0"
      NumberFormat.arabic(42) shouldBe "42"
      NumberFormat.arabic(-3) shouldBe "-3"
    }

    "roman numerals, lowercase" in {
      NumberFormat.roman(1) shouldBe "i"
      NumberFormat.roman(4) shouldBe "iv"
      NumberFormat.roman(9) shouldBe "ix"
      NumberFormat.roman(14) shouldBe "xiv"
      NumberFormat.roman(40) shouldBe "xl"
      NumberFormat.roman(90) shouldBe "xc"
      NumberFormat.roman(2024) shouldBe "mmxxiv"
      NumberFormat.roman(1990) shouldBe "mcmxc"
      NumberFormat.roman(3888) shouldBe "mmmdccclxxxviii"
    }

    "roman of zero or negative falls back to decimal" in {
      NumberFormat.roman(0) shouldBe "0"
      NumberFormat.roman(-5) shouldBe "-5"
    }

    "Roman is uppercase" in {
      NumberFormat.Roman(4) shouldBe "IV"
      NumberFormat.Roman(1990) shouldBe "MCMXC"
    }

    "alph is bijective base-26, 1-based" in {
      NumberFormat.alph(1) shouldBe "a"
      NumberFormat.alph(26) shouldBe "z"
      NumberFormat.alph(27) shouldBe "aa"
      NumberFormat.alph(28) shouldBe "ab"
      NumberFormat.alph(52) shouldBe "az"
      NumberFormat.alph(53) shouldBe "ba"
      NumberFormat.alph(702) shouldBe "zz"
      NumberFormat.alph(703) shouldBe "aaa"
    }

    "alph of zero or negative falls back to decimal" in {
      NumberFormat.alph(0) shouldBe "0"
    }

    "Alph is uppercase" in {
      NumberFormat.Alph(1) shouldBe "A"
      NumberFormat.Alph(27) shouldBe "AA"
    }

    "fnsymbol cycles and doubles" in {
      NumberFormat.fnsymbol(1) shouldBe "*"
      NumberFormat.fnsymbol(2) shouldBe "†"
      NumberFormat.fnsymbol(6) shouldBe "‖"
      NumberFormat.fnsymbol(7) shouldBe "**"
      NumberFormat.fnsymbol(8) shouldBe "††"
      NumberFormat.fnsymbol(13) shouldBe "***"
    }
  }

  "the primitives" - {
    "output their formatted text directly" in {
      run("\\roman 4").result shouldBe "iv"
      run("\\Roman 9").result shouldBe "IX"
      run("\\alph 27").result shouldBe "aa"
      run("\\Alph 2").result shouldBe "B"
      run("\\arabic 42").result shouldBe "42"
      run("\\fnsymbol 3").result shouldBe "‡"
    }

    "compose inside \\set (they set a result)" in {
      val h = run("\\set s {\\roman 14}")
      Value.display(h.get("s")) shouldBe "xiv"
    }

    "read a computed number argument" in {
      val h = run("\\set n {7}\\set s {\\Alph {\\calc{n*2}}}")
      Value.display(h.get("s")) shouldBe "N" // 14th letter
    }
  }
