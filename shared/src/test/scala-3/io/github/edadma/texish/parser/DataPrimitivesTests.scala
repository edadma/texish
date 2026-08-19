package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The data-handling primitives (`PrimitivesData.scala`): indexing, slicing, ordering, searching and folding over
  * sequences, strings and maps, plus `\while`. Tested through a StringHandler, which is the language on its own
  * with no typesetter under it.
  *
  * Two conventions are asserted throughout rather than assumed, because a document depends on them: positions
  * count from 1, and a string behaves as the sequence of its characters — in code points, so an astral symbol is
  * one item.
  */
class DataPrimitivesTests extends AnyFreeSpec with Matchers:

  private def run(src: String): StringHandler =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h

  private def valueOf(src: String, name: String = "v"): String =
    Value.display(run(src).get(name))

  // ---- Indexing and slicing --------------------------------------------------------

  "\\nth counts from 1" in {
    valueOf("\\set v {\\nth{\\seq{a b c}}{2}}") shouldBe "b"
    valueOf("\\set v {\\nth{\\seq{a b c}}{1}}") shouldBe "a"
  }

  "\\nth off either end is Undefined, not an error" in {
    // Undefined, not Nil: a Nil result reads to the expression evaluator as "produced no value", and it then
    // falls back to the argument's source text — so a \\nth past the end would evaluate to "a b5"
    run("\\set v {\\nth{\\seq{a b}}{5}}").get("v") shouldBe Value.Undefined
    run("\\set v {\\nth{\\seq{a b}}{0}}").get("v") shouldBe Value.Undefined
  }

  "\\nth indexes a string by character" in {
    valueOf("\\set v {\\nth{hello}{2}}") shouldBe "e"
  }

  "a string is indexed in code points, so an astral symbol is one character" in {
    // the emoji is a surrogate pair in UTF-16; counting units would make \nth 2 half a character
    valueOf("\\set v {\\nth{a🎲b}{3}}") shouldBe "b"
  }

  "\\slice takes a count from a 1-based start, clamped at both ends" in {
    valueOf("\\set v {\\join{\\slice{\\seq{a b c d e}}{2}{3}}{-}}") shouldBe "b-c-d"
    valueOf("\\set v {\\join{\\slice{\\seq{a b c}}{2}{99}}{-}}") shouldBe "b-c"
    valueOf("\\set v {\\join{\\slice{\\seq{a b c}}{1}{0}}{-}}") shouldBe ""
  }

  "\\slice of a string gives a string" in {
    valueOf("\\set v {\\slice{typesetting}{5}{3}}") shouldBe "set"
  }

  "\\reverse turns a sequence and a string around" in {
    valueOf("\\set v {\\join{\\reverse{\\seq{a b c}}}{}}") shouldBe "cba"
    valueOf("\\set v {\\reverse{abc}}") shouldBe "cba"
  }

  // ---- Building sequences ----------------------------------------------------------

  "\\append and \\prepend add one item to either end" in {
    valueOf("\\set v {\\join{\\append{\\seq{a b}}{c}}{}}") shouldBe "abc"
    valueOf("\\set v {\\join{\\prepend{\\seq{b c}}{a}}{}}") shouldBe "abc"
  }

  "a sequence grows across a loop when the assignment is global" in {
    // the accumulation \for could not do before: the body's \global\set survives the iteration's scope
    val src = "\\global\\set acc {\\seq{}}\\for\\i{\\range{1}{4}}{\\global\\set acc {\\append{\\acc}{\\i}}}" +
      "\\set v {\\join{\\acc}{,}}"
    valueOf(src) shouldBe "1,2,3,4"
  }

  "\\concat joins two sequences where \\cat joins two values as text" in {
    valueOf("\\set v {\\size{\\concat{\\seq{a b}}{\\seq{c d}}}}") shouldBe "4"
    // \cat is unchanged and still textual
    valueOf("\\set v {\\cat{ab}{cd}}") shouldBe "abcd"
  }

  "\\join puts a separator between the items" in {
    valueOf("\\set v {\\join{\\seq{1 2 3}}{, }}") shouldBe "1, 2, 3"
  }

  "\\chunk groups a flat list into records, the last one short" in {
    valueOf("\\set v {\\size{\\chunk{\\seq{1 2 3 4 5 6}}{2}}}") shouldBe "3"
    valueOf("\\set v {\\join{\\nth{\\chunk{\\seq{1 2 3 4 5}}{2}}{3}}{,}}") shouldBe "5"
  }

  // ---- Searching -------------------------------------------------------------------

  "\\contains tests membership in a sequence and a substring in a string" in {
    run("\\set v {\\contains{\\seq{a b c}}{b}}").get("v") shouldBe Value.Bool(true)
    run("\\set v {\\contains{\\seq{a b c}}{z}}").get("v") shouldBe Value.Bool(false)
    run("\\set v {\\contains{typesetting}{set}}").get("v") shouldBe Value.Bool(true)
  }

  "\\indexof gives a 1-based position, or 0 when absent" in {
    valueOf("\\set v {\\indexof{\\seq{a b c}}{c}}") shouldBe "3"
    valueOf("\\set v {\\indexof{\\seq{a b c}}{z}}") shouldBe "0"
    valueOf("\\set v {\\indexof{typesetting}{set}}") shouldBe "5"
  }

  "a 0 from \\indexof is falsy, so the same call tests presence" in {
    valueOf("\\if {\\indexof{\\seq{a b}}{b}}\\set v {yes}\\else\\set v {no}\\fi") shouldBe "yes"
    valueOf("\\if {\\indexof{\\seq{a b}}{z}}\\set v {yes}\\else\\set v {no}\\fi") shouldBe "no"
  }

  // ---- Aggregates ------------------------------------------------------------------

  "\\total totals a sequence of numbers" in {
    valueOf("\\set v {\\total{\\seq{1 2 3.5}}}") shouldBe "6.5"
    valueOf("\\set v {\\total{\\seq{}}}") shouldBe "0"
  }

  "\\minimum and \\maximum use the same ordering as \\sort" in {
    valueOf("\\set v {\\minimum{\\seq{10 2 33}}}") shouldBe "2"
    valueOf("\\set v {\\maximum{\\seq{10 2 33}}}") shouldBe "33"
    valueOf("\\set v {\\minimum{\\seq{pear apple fig}}}") shouldBe "apple"
  }

  "an aggregate of an empty sequence is Undefined rather than a wrong number" in {
    run("\\set v {\\maximum{\\seq{}}}").get("v") shouldBe Value.Undefined
  }

  // ---- Ordering --------------------------------------------------------------------

  "\\sort orders numbers numerically, not as text" in {
    valueOf("\\set v {\\join{\\sort{\\seq{10 2 33 4}}}{,}}") shouldBe "2,4,10,33"
  }

  "\\sort orders words alphabetically" in {
    valueOf("\\set v {\\join{\\sort{\\seq{pear apple fig}}}{,}}") shouldBe "apple,fig,pear"
  }

  "\\sortby orders by a computed key" in {
    // the index's case-folded order: a capital must not sort ahead of everything
    val src = "\\set v {\\join{\\sortby\\w{\\seq{Zebra apple Mongoose}}{\\downcase{\\w}}}{,}}"
    valueOf(src) shouldBe "apple,Mongoose,Zebra"
  }

  "\\sortby is stable, so equal keys keep their original order" in {
    val src = "\\set v {\\join{\\sortby\\w{\\seq{bb aa cc dd}}{\\size{\\w}}}{,}}"
    valueOf(src) shouldBe "bb,aa,cc,dd"
  }

  // ---- Folding with a body ---------------------------------------------------------

  "\\filter keeps the items whose condition holds" in {
    valueOf("\\set v {\\join{\\filter\\n{\\range{1}{6}}{\\> {\\n} {3}}}{,}}") shouldBe "4,5,6"
  }

  "\\transform replaces each item by what the expression computes" in {
    valueOf("\\set v {\\join{\\transform\\n{\\range{1}{4}}{\\calc{\\n * \\n}}}{,}}") shouldBe "1,4,9,16"
  }

  "\\transform and \\total together are a fold, which is what a plot's sums need" in {
    // the least-squares Sxy that plot.texish accumulates through five \global variables
    // \\calc reads its argument as an expression STRING, so a primitive call inside it flattens to nonsense
    // (\\nth{\\p}{1} becomes the identifier "nthp1"); a bare or backslashed variable name is what it takes. So the
    // elements are bound first, and the body is a short statement sequence whose last value is the result.
    val src = "\\set pairs {\\chunk{\\seq{1 2 3 4}}{2}}" +
      "\\set v {\\total{\\transform\\p{\\pairs}{\\set a {\\nth{\\p}{1}}\\set b {\\nth{\\p}{2}}\\calc{a * b}}}}"
    valueOf(src) shouldBe "14"
  }

  "the loop variable does not leak out of the body" in {
    run("\\set n {outer}\\set v {\\size{\\filter\\n{\\seq{a b}}{1}}}").get("n") shouldBe Value.Text("outer")
  }

  // ---- Strings ---------------------------------------------------------------------

  "\\split cuts on a literal separator" in {
    valueOf("\\set v {\\join{\\split{a,b,c}{,}}{|}}") shouldBe "a|b|c"
  }

  "\\split treats the separator literally, not as a pattern" in {
    // a regex split on "." would cut between every character
    valueOf("\\set v {\\join{\\split{a.b.c}{.}}{|}}") shouldBe "a|b|c"
  }

  "\\split on an empty separator gives the characters" in {
    valueOf("\\set v {\\join{\\split{abc}{}}{-}}") shouldBe "a-b-c"
  }

  "\\split and \\join are inverses" in {
    valueOf("\\set v {\\join{\\split{one two three}{ }}{ }}") shouldBe "one two three"
  }

  "\\replace changes every occurrence, matched literally" in {
    valueOf("\\set v {\\replace{a-b-c}{-}{+}}") shouldBe "a+b+c"
    valueOf("\\set v {\\replace{a.b}{.}{!}}") shouldBe "a!b"
  }

  "\\replace of an empty pattern leaves the text alone rather than looping" in {
    valueOf("\\set v {\\replace{abc}{}{x}}") shouldBe "abc"
  }

  "\\repeat repeats a string, and a count of zero gives nothing" in {
    valueOf("\\set v {\\repeat{ab}{3}}") shouldBe "ababab"
    valueOf("\\set v {\\repeat{ab}{0}}") shouldBe ""
  }

  "\\startswith and \\endswith test either end" in {
    run("\\set v {\\startswith{chapter one}{chapter}}").get("v") shouldBe Value.Bool(true)
    run("\\set v {\\endswith{chapter one}{one}}").get("v") shouldBe Value.Bool(true)
    run("\\set v {\\startswith{chapter one}{one}}").get("v") shouldBe Value.Bool(false)
  }

  "\\fixed keeps the trailing zeros \\round drops" in {
    valueOf("\\set v {\\fixed{0.3}{2}}") shouldBe "0.30"
    valueOf("\\set v {\\round{0.3}{2}}") shouldBe "0.3"
    valueOf("\\set v {\\fixed{2}{2}}") shouldBe "2.00"
  }

  "\\fixed rounds, carries and keeps a negative sign" in {
    valueOf("\\set v {\\fixed{1.006}{2}}") shouldBe "1.01"
    valueOf("\\set v {\\fixed{1.999}{2}}") shouldBe "2.00"
    valueOf("\\set v {\\fixed{-1.5}{1}}") shouldBe "-1.5"
    valueOf("\\set v {\\fixed{3.7}{0}}") shouldBe "4"
  }

  // ---- Maps ------------------------------------------------------------------------

  "\\keys and \\values read a map as sequences, in its own order" in {
    valueOf("\\set m {\\map{b 2 a 1 c 3}}\\set v {\\join{\\keys{\\m}}{,}}") shouldBe "b,a,c"
    valueOf("\\set m {\\map{b 2 a 1 c 3}}\\set v {\\join{\\values{\\m}}{,}}") shouldBe "2,1,3"
  }

  "a map's keys can then be sorted like any other sequence" in {
    valueOf("\\set m {\\map{b 2 a 1 c 3}}\\set v {\\join{\\sort{\\keys{\\m}}}{,}}") shouldBe "a,b,c"
  }

  "\\mapdel removes a key and leaves the rest" in {
    val src = "\\mapset m {a} {1}\\mapset m {b} {2}\\mapdel m {a}" +
      "\\set p {\\maphas m {a}}\\set v {\\mapget m {b}}"
    val h   = run(src)
    h.get("p") shouldBe Value.Bool(false)
    Value.display(h.get("v")) shouldBe "2"
  }

  "\\mapdel of a key that is not there leaves the map alone" in {
    valueOf("\\mapset m {a} {1}\\mapdel m {z}\\set v {\\mapget m {a}}") shouldBe "1"
  }

  // ---- \while ----------------------------------------------------------------------

  "\\while repeats until its condition goes false, re-reading it each time" in {
    val src = "\\global\\set n {0}\\while {\\< {\\n} {5}} {\\global\\set n {\\calc{n + 1}}}\\set v {\\n}"
    valueOf(src) shouldBe "5"
  }

  "a \\while whose condition starts false runs no iterations" in {
    val src = "\\global\\set n {9}\\while {\\< {\\n} {5}} {\\global\\set n {\\calc{n + 1}}}\\set v {\\n}"
    valueOf(src) shouldBe "9"
  }

  "a \\while can iterate until a computation converges" in {
    // Newton's method for a square root — a loop whose length is not known before it starts, which is the case
    // \for cannot express at all
    val src = "\\global\\set x {2}\\global\\set d {1}" +
      "\\while {\\> {\\d} {0.0000001}} {\\global\\set p {\\x}\\global\\set x {\\calc{(x + 2/x)/2}}" +
      "\\global\\set d {\\calc{abs(x - p)}}}\\set v {\\fixed{\\x}{6}}"
    valueOf(src) shouldBe "1.414214"
  }
