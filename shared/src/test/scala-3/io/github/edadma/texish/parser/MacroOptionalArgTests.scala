package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `\def` (and `\newenvironment`) parameters follow an xparse-style named model: a bare name is a mandatory
  * parameter, and `[name:default]` declares an optional one supplied at the call site as a bracketed `[…]` argument,
  * falling back to `default` when omitted. A missing optional never consumes input, and a braced mandatory argument
  * is never mistaken for an optional one.
  */
class MacroOptionalArgTests extends AnyFreeSpec with Matchers:

  private def run(src: String): StringHandler =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h

  "an omitted optional argument uses its declared default" in {
    run("\\def label [k:fig] {\\k}\\label").result shouldBe "fig"
  }

  "a supplied optional argument overrides the default" in {
    run("\\def label [k:fig] {\\k}\\label[tab]").result shouldBe "tab"
  }

  "an optional parameter can precede a mandatory one" in {
    val src = "\\def kv [k:def] v {\\k=\\v}"
    run(s"$src\\kv{X}").result shouldBe "def=X"
    run(s"$src\\kv[set]{X}").result shouldBe "set=X"
  }

  "an optional parameter with no default expands to nothing when omitted" in {
    run("\\def opt [k] {<\\k>}\\opt").result shouldBe "<>"
    run("\\def opt [k] {<\\k>}\\opt[Y]").result shouldBe "<Y>"
  }

  "a braced mandatory argument starting with [ is not read as the optional" in {
    run("\\def kv [k:def] v {\\k=\\v}\\kv{[Z]}").result shouldBe "def=[Z]"
  }

  "a ] inside braces does not close the optional argument" in {
    run("\\def show [k:x] {\\k}\\show[{a]b}]").result shouldBe "a]b"
  }

  "environments accept optional parameters too" in {
    val src = "\\newenvironment{w}[n:5]{n=\\n;}{;z}"
    run(s"$src\\begin{w}q\\end{w}").result shouldBe "n=5;q;z"
    run(s"$src\\begin{w}[8]q\\end{w}").result shouldBe "n=8;q;z"
  }

  "a bare * declares a star flag: 1 when * follows, 0 when it does not" in {
    val src = "\\def s * t {\\if {\\star}S\\else N\\fi:\\t}"
    run(s"$src\\s*{x}").result shouldBe "S:x"
    run(s"$src\\s{x}").result shouldBe "N:x"
  }

  "a star flag may precede a mandatory argument given as bare text" in {
    val src = "\\def s * t {\\if {\\star}+\\fi\\t}"
    run(s"$src\\s*x").result shouldBe "+x"
    run(s"$src\\s y").result shouldBe "y"
  }
