package io.github.edadma.texish.parser

import io.github.edadma.texish.TexishException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The rules that say what a piece of the document language *means*, as against what any one primitive does:
  * what a macro is worth when it is used as a value, where a value goes when a primitive produces one, what
  * counts as equal, what `\ifx` compares, what a name may be spelled with, and what `\calc` can reach.
  *
  * Each of these answered wrongly and silently before — a macro evaluated to its own source text, a value
  * primitive typeset nothing, a comparison against "no value" was false, `\ifx` compared spellings rather than
  * meanings, a name with a digit could be bound and never read back, and arithmetic could not call anything.
  * They are gathered here because they are one contract, not six unrelated primitives.
  */
class LanguageSemanticsTests extends AnyFreeSpec with Matchers:

  private def run(src: String): StringHandler =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h

  private def out(src: String): String = run(src).result

  private def valueOf(src: String, name: String = "v"): Value = run(src).get(name)

  private def display(src: String, name: String = "v"): String = Value.display(valueOf(src, name))

  // ---- A macro is worth what its whole body computes ------------------------------

  "a macro body may bind before it computes" in {
    // the body's first statement used to be the only one run, and the rest fell back to the body's source
    // text — so this evaluated to the string "a b a * b"
    display("\\def area {\\set a {3}\\set b {4}\\calc{a * b}}\\set v {\\area}") shouldBe "12"
  }

  "a macro returning a value keeps its type" in {
    valueOf("\\def twice n {\\calc{\\n * 2}}\\set v {\\twice{21}}") shouldBe Value.Num(42)
    valueOf("\\def pair {\\seq{a b}}\\set v {\\pair}") shouldBe Value.Seq(Vector(Value.Text("a"), Value.Text("b")))
  }

  "a macro that is a bare variable reference keeps that variable's type" in {
    // the value is read back through the result, not re-parsed from the text it printed
    valueOf("\\set s {\\seq{a b}}\\def alias {\\s}\\set v {\\alias}") shouldBe
      Value.Seq(Vector(Value.Text("a"), Value.Text("b")))
  }

  "a macro that writes a sentence is worth the sentence, not the number inside it" in {
    display("\\set n {3}\\def label {Item \\the\\n}\\set v {\\label}") shouldBe "Item 3"
  }

  "a macro may choose its value in a conditional, and the branch keeps its type" in {
    val src = "\\def pick n {\\if {\\> {\\n} {0}}\\seq{up}\\else\\seq{down}\\fi}"
    valueOf(src + "\\set v {\\pick{1}}") shouldBe Value.Seq(Vector(Value.Text("up")))
    valueOf(src + "\\set v {\\pick{-1}}") shouldBe Value.Seq(Vector(Value.Text("down")))
  }

  "a macro's value composes inside another expression" in {
    display("\\def half n {\\calc{\\n / 2}}\\set v {\\calc{\\half{10} + 1}}") shouldBe "6"
  }

  "a macro used in document position still writes what its body writes" in {
    out("\\def area {\\set a {3}\\set b {4}\\calc{a * b}}\\area") shouldBe "12"
  }

  "a macro is worth the same thing while a module is loading, where output is suppressed" in {
    // \use suppresses output for the whole file, and a macro's value is read from what its body writes — so a
    // package that says \def usfmfamily {lmroman} and then \font \usfmfamily 10 regular read the family as
    // nothing at all, and the font selection failed on the package's own line
    val h    = new StringHandler
    val proc = new Processor(h)
    h.suppressOutput(true)
    proc.process("\\def fam {lmroman}\\set v {\\fam}")
    h.suppressOutput(false)
    Value.display(h.get("v")) shouldBe "lmroman"
  }

  // ---- "produced nothing" is not the same fact as "produced Nil" -------------------

  "a stored empty value reads back as empty rather than as the source text" in {
    // the evaluator used to read a Nil result as "this produced no value" and fall back to the text of the
    // call itself, so an empty entry came back as the characters "mapget m k"
    valueOf("\\mapset m {k} {}\\set v {\\mapget m {k}}") shouldBe Value.Nil
  }

  // ---- Where a value goes when a primitive produces one ---------------------------

  "a primitive that produces a scalar typesets it" in {
    out("\\cat{ab}{cd}") shouldBe "abcd"
    out("\\setcounter{page}{7}\\value{page}") shouldBe "7"
    out("\\mapset m {k} {v}\\mapget m {k}") shouldBe "v"
    out("\\mapset m {k} {v}\\maphas m {k}") shouldBe "true"
    out("\\contains{\\seq{a b}}{b}") shouldBe "true"
  }

  "a primitive that produces a container typesets nothing" in {
    // [a, b] is how a sequence is shown for debugging, not something a document ever wants set
    out("\\seq{a b}") shouldBe ""
    out("\\sort{\\seq{b a}}") shouldBe ""
    out("\\reverse{\\seq{a b}}") shouldBe ""
    out("\\keys{\\map{k v}}") shouldBe ""
    out("\\oklchof{#ff0000}") shouldBe ""
  }

  "a container is still shown on request" in {
    out("\\set s {\\reverse{\\seq{a b}}}\\the\\s") shouldBe "[b, a]"
  }

  // ---- Equality --------------------------------------------------------------------

  "absence equals absence" in {
    // \x is unset (Undefined) and {} is Nil; no document can tell them apart, and this is the test every
    // package wants to write
    out("\\if {\\= {\\x} {}}absent\\fi") shouldBe "absent"
    out("\\set x {}\\if {\\= {\\x} {}}absent\\fi") shouldBe "absent"
  }

  "a value is not absent" in {
    out("\\set x {0}\\if {\\= {\\x} {}}absent\\else present\\fi").trim shouldBe "present"
    out("\\if {\\!= {\\x} {}}set\\else unset\\fi") shouldBe "unset"
  }

  "sequences and maps compare by their contents" in {
    out("\\if {\\= {\\seq{a b}} {\\seq{a b}}}same\\fi") shouldBe "same"
    out("\\if {\\= {\\seq{a b}} {\\seq{a c}}}same\\fi") shouldBe ""
    out("\\if {\\= {\\map{k v}} {\\map{k v}}}same\\fi") shouldBe "same"
  }

  // ---- \ifx compares meanings ------------------------------------------------------

  "\\ifx is true for two names defined the same way" in {
    out("\\def a {x}\\def b {x}\\ifx \\a \\b{same}\\fi") shouldBe "same"
  }

  "\\ifx is false for two names defined differently" in {
    out("\\def a {x}\\def b {y}\\ifx \\a \\b{same}\\fi") shouldBe ""
    out("\\def a {x}\\def b n {x}\\ifx \\a \\b{same}\\fi") shouldBe ""
  }

  "\\ifx sees through \\let" in {
    out("\\def a {x}\\let b \\a\\ifx \\a \\b{same}\\fi") shouldBe "same"
  }

  "\\ifx is true for two names that mean nothing, which is how a package asks whether one was defined" in {
    out("\\ifx \\nowhere \\alsonowhere{both undefined}\\fi") shouldBe "both undefined"
    out("\\def a {x}\\ifx \\a \\nowhere{both undefined}\\fi") shouldBe ""
  }

  "\\ifx compares two variables by their values" in {
    out("\\set a {1}\\set b {1}\\ifx \\a \\b{same}\\fi") shouldBe "same"
    out("\\set a {1}\\set b {2}\\ifx \\a \\b{same}\\fi") shouldBe ""
  }

  // ---- A name is letters only ------------------------------------------------------

  "a name with a digit is refused rather than bound out of reach" in {
    // \set count2 {5} used to succeed, and \count2 then read as \count followed by the text 2 — the value
    // was set and could never be read back, and what came out was a wrong value rather than an error
    val e = intercept[TexishException](run("\\set count2 {5}"))
    e.getMessage should include("count2")
    e.getMessage should include("letters only")

    intercept[TexishException](run("\\def twice2 n {\\n}"))
    intercept[TexishException](run("\\set x {1}\\let x2 \\x"))
  }

  "a name spelled out rather than written as a control sequence may still carry a digit" in {
    // a counter, a map key and a bare \calc identifier are all read back by the spelling that made them
    out("\\setcounter{level2}{4}\\arabic{\\value{level2}}") shouldBe "4"
    out("\\mapset m {k2} {v}\\mapget m {k2}") shouldBe "v"
  }

  // ---- \calc can call ---------------------------------------------------------------

  "\\calc evaluates a call rather than flattening it to an identifier" in {
    // \nth{\p}{1} used to flatten to the identifier nthp1 and fail with "unknown name"
    display("\\set p {\\seq{3 4}}\\set v {\\calc{\\nth{\\p}{1} * 2}}") shouldBe "6"
    display("\\set p {\\seq{1 2 3}}\\set v {\\calc{\\total{\\p} + \\size{\\p}}}") shouldBe "9"
  }

  "\\calc still reads a bare control sequence as the variable it names" in {
    display("\\set n {4}\\set v {\\calc{\\n * 2}}") shouldBe "8"
    display("\\set n {4}\\set v {\\calc{n * 2}}") shouldBe "8"
  }

  "\\calc reads a dotted field, which is not an identifier" in {
    // \forloop.index is a field of a map; flattening gave the text "forloop.index" and the expression failed on
    // the name forloop, so a loop could not do arithmetic on its own position without binding it first
    display("\\set m {\\map{w 4 h 3}}\\set v {\\calc{\\m.w * \\m.h}}") shouldBe "12"
    out("\\for\\i{\\seq{a b c}}{\\calc{\\forloop.index * 10} }").trim shouldBe "10 20 30"
  }

  "\\calc rejects a call that is not a number" in {
    val e = intercept[TexishException](run("\\set v {\\calc{\\upcase{ab} + 1}}"))
    e.getMessage should include("not a number")
  }

  // ---- The guide's examples, run rather than asserted ------------------------------

  "the guide's multi-statement macro computes what the page says" in {
    display("\\set margin {5}\\def area w h {\\set inner {\\calc{\\w - 2 * margin}}\\calc{\\inner * \\h}}" +
      "\\set v {\\area{60}{40}}") shouldBe "2000"
  }

  "the guide's value-placement examples put values where the page says" in {
    out("\\cat{Chapter }{Nine}") shouldBe "Chapter Nine"
    out("\\sort{\\seq{b a}}") shouldBe ""
    out("\\set sorted {\\sort{\\seq{b a}}}\\the\\sorted") shouldBe "[a, b]"
  }

  "the guide's \\calc call example averages a sequence" in {
    display("\\set xs {\\seq{1 2 3 4}}\\set v {\\calc{\\total{\\xs} / \\size{\\xs}}}") shouldBe "2.5"
  }

  "the guide's label example is worth its whole sentence" in {
    display("\\set n {3}\\def label {Item \\the\\n}\\set v {\\label}") shouldBe "Item 3"
  }

