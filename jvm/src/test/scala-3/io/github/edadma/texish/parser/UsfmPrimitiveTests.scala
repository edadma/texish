package io.github.edadma.texish.parser

import java.nio.file.Files

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The `\usfm` primitive end to end: it reads a USFM file, translates it, and processes the result against the
  * `\usfm…` macros in effect. A real filesystem is required, so this lives in the JVM suite. The macros here are
  * plain-text stand-ins for the styled ones in the `usfm` package, so the test observes the translation the
  * primitive drove without needing the full typesetter.
  */
class UsfmPrimitiveTests extends AnyFreeSpec with Matchers:

  private val stubs =
    """\def usfmc n {[c\n]}
      |\def usfmv n {[v\n]}
      |\def usfmp n {(p)}
      |\def usfmq n {(q\n)}
      |\def usfms n t {<s:\t>}
      |\def usfmwj t {<wj>\t</wj>}
      |\def usfmref d t {ref(\d)}
      |\def usfmfootnote b {[fn:\b]}
      |\def usfmfr t {\t}
      |\def usfmft t {\t}
      |\def usfmfqa t {\t}
      |""".stripMargin

  "\\usfm reads a file beside the document and typesets its markers through the \\usfm macros" in {
    val dir = Files.createTempDirectory("texish-usfm")
    Files.writeString(
      dir.resolve("mark.usfm"),
      """\id MRK Test
        |\usfm 3.1
        |\c 1
        |\s1 The Beginning
        |\p \v 1 In the beginning\f + \fr 1:1 \ft Or \fqa start\f* was the Word.
        |""".stripMargin,
    )

    val handler = new StringHandler
    val proc    = new Processor(handler)
    proc.setBaseDir(dir.toString)
    proc.process(stubs + "\\usfm{mark.usfm}")

    val out = handler.result
    out should include("[c1]")
    out should include("<s:The Beginning")
    out should include("[v1]")
    out should include("In the beginning")
    out should include("[fn:")
    out should include("start")
    out should include("was the Word.")
    // metadata lines leave no trace
    out should not include "MRK"
    out should not include "3.1"
  }

  "\\usfm still accepts an absolute path" in {
    val dir  = Files.createTempDirectory("texish-usfm-abs")
    val file = dir.resolve("john.usfm")
    Files.writeString(file, "\\p \\wj Follow Me.\\wj* he said.\n")

    val handler = new StringHandler
    val proc    = new Processor(handler)
    proc.process(stubs + s"\\usfm{$file}")

    handler.result should include("<wj>Follow Me.</wj> he said.")
  }
