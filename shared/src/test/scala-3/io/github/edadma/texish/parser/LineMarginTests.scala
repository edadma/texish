package io.github.edadma.texish.parser

import io.github.edadma.texish.{Builder, HBox, StubTypesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream

// \leftskip / \rightskip narrow every line of a paragraph and \hangindent / \hangafter narrow the
// lines they select — TeX's paragraph-shape parameters, now honoured by the line breaker. Each line
// is still set to the full \hsize, with the margins folded in as leading/trailing glue, so the
// inset shows up as the width of a line's leading box. The StubTypesetter sets every character 6
// wide, so the geometry is exact.
class LineMarginTests extends AnyFreeSpec with Matchers:

  // The paragraph's set lines (each HBox is one line), after applying the given numeric variables.
  private def lines(src: String, vars: (String, Double)*): List[HBox] =
    val t = new StubTypesetter
    t.set("vsize", 1.0e9) // one page, no page breaking during the probe
    vars.foreach((k, v) => t.set(k, v))
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new ByteArrayOutputStream) {
      proc.process(src)
      t.paragraph() // close the last paragraph so its lines reach the page list
    }
    t.mode.asInstanceOf[Builder].list.collect { case h: HBox => h }

  // long enough to wrap to several lines at the test's hsize
  private val para = (1 to 40).map(_ => "word").mkString(" ") + "\n"

  "\\leftskip insets the left of every line, and each line still fills hsize" in {
    val ls = lines(para, "hsize" -> 200.0, "leftskip" -> 40.0)
    ls.length should be > 1
    ls.foreach { line =>
      line.boxes.head.width shouldBe 40.0 // the leading margin
      line.width shouldBe 200.0 +- 0.1    // the whole line still sets to hsize
    }
  }

  "\\rightskip narrows the measure, so the paragraph needs more lines" in {
    val plain  = lines(para, "hsize" -> 200.0)
    val narrow = lines(para, "hsize" -> 200.0, "rightskip" -> 80.0)
    narrow.length should be > plain.length
  }

  "\\hangindent with \\hangafter 1 indents the continuation lines only" in {
    val ls = lines(para, "hsize" -> 200.0, "hangindent" -> 30.0, "hangafter" -> 1.0)
    ls.length should be > 1
    ls.head.boxes.head.width shouldBe 0.0 // first line at the margin
    ls.tail.foreach(_.boxes.head.width shouldBe 30.0) // every later line hangs
  }

  "a negative \\hangafter hangs the first lines instead" in {
    val ls = lines(para, "hsize" -> 200.0, "hangindent" -> 30.0, "hangafter" -> -1.0)
    ls.length should be > 1
    ls.head.boxes.head.width shouldBe 30.0       // first line hangs
    ls.tail.foreach(_.boxes.head.width shouldBe 0.0) // the rest are at the margin
  }

  "\\hangindent and \\hangafter revert to their defaults after the paragraph" in {
    val t = new StubTypesetter
    t.set("vsize", 1.0e9)
    t.set("hangindent", 30.0)
    t.set("hangafter", 2.0)
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new ByteArrayOutputStream) {
      proc.process(para)
      t.paragraph()
    }
    t.getNumber("hangindent") shouldBe 0.0
    t.getNumber("hangafter") shouldBe 1.0
  }
