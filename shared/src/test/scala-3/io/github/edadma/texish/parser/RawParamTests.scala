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

  "a raw parameter can follow an optional one, taken and omitted" in {
    // This is what lets a package present the interface an engine primitive presents: an option bracket read as
    // tokens, then a verbatim body — the shape \qrcode[ecc:h]{https://…} needs, where the options are ordinary
    // text but the data is a URL whose // must survive
    run("\\def g [o:none] <e> {\\o|\\e}\\g{a//b}").result shouldBe "none|a//b"
    run("\\def g [o:none] <e> {\\o|\\e}\\g[ecc:h]{a//b}").result shouldBe "ecc:h|a//b"
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

  "\\oklch builds a hex colour from L C h" in {
    run("\\oklch{0.7}{0.1}{260}").result should fullyMatch regex "#[0-9a-f]{6}"
  }

  "\\oklchof then \\oklch round-trips a colour" in {
    val s = "\\set lch {\\oklchof{#336699}}" +
      "\\oklch{\\head{\\lch}}{\\head{\\tail{\\lch}}}{\\head{\\tail{\\tail{\\lch}}}}"
    run(s).result shouldBe "#336699"
  }

  "a darker shade keeps the hue: lower the Oklch lightness" in {
    val s = "\\set lch {\\oklchof{#6699cc}}\\set l {\\head{\\lch}}" +
      "\\oklch{\\calc{\\l - 0.15}}{\\head{\\tail{\\lch}}}{\\head{\\tail{\\tail{\\lch}}}}"
    run(s).result should fullyMatch regex "#[0-9a-f]{6}"
  }
