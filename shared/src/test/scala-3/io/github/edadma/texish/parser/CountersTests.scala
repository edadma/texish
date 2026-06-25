package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The LaTeX-style named-counter primitives: \newcounter declares one at zero, \stepcounter and \addtocounter
  * advance it, \setcounter assigns it, \value reads it as a number (so \arabic{\value{c}} formats it), and
  * \counterwithin makes a child counter reset whenever its parent steps. Counters are global by design — they are
  * not restored at group exit — which the last case checks. Tested with a StringHandler so the formatted output is
  * exactly the text a document would print.
  */
class CountersTests extends AnyFreeSpec with Matchers:

  private def run(src: String): String =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h.result

  "a new counter starts at zero" in {
    run("\\newcounter{c}\\arabic{\\value{c}}") shouldBe "0"
  }

  "\\stepcounter advances by one" in {
    run("\\newcounter{c}\\stepcounter{c}\\stepcounter{c}\\arabic{\\value{c}}") shouldBe "2"
  }

  "\\setcounter and \\addtocounter assign and add" in {
    run("\\newcounter{c}\\setcounter{c}{5}\\addtocounter{c}{3}\\arabic{\\value{c}}") shouldBe "8"
  }

  "\\value reads an undeclared counter as zero" in {
    run("\\arabic{\\value{never}}") shouldBe "0"
  }

  "a counter declared \\counterwithin resets when its parent steps" in {
    val out = run(
      "\\newcounter{section}\\newcounter{subsection}\\counterwithin{subsection}{section}" +
        "\\stepcounter{section}\\stepcounter{subsection}\\stepcounter{subsection}" +
        "\\arabic{\\value{section}}.\\arabic{\\value{subsection}} " +
        "\\stepcounter{section}" +
        "\\arabic{\\value{section}}.\\arabic{\\value{subsection}}")
    out shouldBe "1.2 2.0"
  }

  "the reset cascades down a chain of \\counterwithin" in {
    val out = run(
      "\\newcounter{section}\\newcounter{subsection}\\newcounter{subsubsection}" +
        "\\counterwithin{subsection}{section}\\counterwithin{subsubsection}{subsection}" +
        "\\stepcounter{section}\\stepcounter{subsection}\\stepcounter{subsubsection}" +
        "\\stepcounter{section}" +
        "\\arabic{\\value{subsection}}\\arabic{\\value{subsubsection}}")
    out shouldBe "00"
  }

  "counters survive a group — they are global, never restored at group exit" in {
    run("\\newcounter{c}{\\stepcounter{c}}\\arabic{\\value{c}}") shouldBe "1"
  }
