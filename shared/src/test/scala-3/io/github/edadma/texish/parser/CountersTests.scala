package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The `counters` package (`\use{counters}`): LaTeX-style document counters built in the document language over
  * the global keyed store. Covers create/set/add/step, formatting the value, group survival (counters are global),
  * and reset-lists — stepping a parent zeroes the counters declared within it, recursively.
  */
class CountersTests extends AnyFreeSpec with Matchers:

  private def run(src: String): String =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process("\\use{counters}" + src)
    h.result

  "\\newcounter starts a counter at 0" in {
    run("\\newcounter{c}\\arabic{\\value c}") shouldBe "0"
  }

  "\\setcounter and \\arabic display it" in {
    run("\\newcounter{c}\\setcounter{c}{7}\\arabic{\\value c}") shouldBe "7"
  }

  "\\stepcounter increments by one" in {
    run("\\newcounter{c}\\stepcounter{c}\\stepcounter{c}\\arabic{\\value c}") shouldBe "2"
  }

  "\\addtocounter adds (and subtracts)" in {
    run("\\newcounter{c}\\setcounter{c}{5}\\addtocounter{c}{3}\\addtocounter{c}{-2}\\arabic{\\value c}") shouldBe "6"
  }

  "the value formats with any formatter" in {
    run("\\newcounter{c}\\setcounter{c}{4}\\roman{\\value c}") shouldBe "iv"
    run("\\newcounter{d}\\setcounter{d}{3}\\Alph{\\value d}") shouldBe "C"
  }

  "counters are global — they survive a group" in {
    run("\\newcounter{c}{\\stepcounter{c}}\\arabic{\\value c}") shouldBe "1"
  }

  "stepping a parent resets a counter declared within it" in {
    val out = run(
      "\\newcounter{section}\\newcounter{subsection}\\counterwithin{subsection}{section}" +
        "\\setcounter{section}{1}\\setcounter{subsection}{4}" +
        "\\stepcounter{section}" +
        "\\arabic{\\value section}.\\arabic{\\value subsection}",
    )
    out shouldBe "2.0" // section 1->2, subsection reset to 0
  }

  "stepping a child does not disturb its parent" in {
    val out = run(
      "\\newcounter{section}\\newcounter{subsection}\\counterwithin{subsection}{section}" +
        "\\setcounter{section}{2}\\stepcounter{subsection}" +
        "\\arabic{\\value section}.\\arabic{\\value subsection}",
    )
    out shouldBe "2.1"
  }

  "reset cascades to grandchildren" in {
    val out = run(
      "\\newcounter{chapter}\\newcounter{section}\\newcounter{subsection}" +
        "\\counterwithin{section}{chapter}\\counterwithin{subsection}{section}" +
        "\\setcounter{section}{3}\\setcounter{subsection}{9}" +
        "\\stepcounter{chapter}" +
        "\\arabic{\\value chapter}.\\arabic{\\value section}.\\arabic{\\value subsection}",
    )
    out shouldBe "1.0.0"
  }
