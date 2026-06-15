package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, CharBox, DocumentMode, FloatBox, HBox, StubTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** \topinsert typesets its body into a block that floats to the top of whatever page it lands on: the block rides
  * the vertical list as a zero-size FloatBox that migrates out of its line, the page builder counts its height
  * against the page, and shipout lifts it above the body — the top-placement mirror of \footnote.
  */
class FloatTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: StubTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def fixture(): (StubTypesetter, CapturingDocument, Processor) =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    (t, doc, proc)

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  private def texts(box: Box): List[String] = box match
    case v: VBox    => v.boxes.toList.flatMap(texts)
    case h: HBox    => h.boxes.toList.flatMap(texts)
    case c: CharBox => List(c.text)
    case _          => Nil

  "\\topinsert migrates the float out of the line to the vertical list" in {
    val (t, _, proc) = fixture()

    proc.process("alpha \\topinsert{boxed chart} beta gamma")
    t.paragraph()

    val items = t.mode.asInstanceOf[Builder].list

    // the float sits at the top level of the vertical list, not inside any line
    items.count(_.isInstanceOf[FloatBox]) shouldBe 1
    for line <- items.collect { case h: HBox => h } do
      line.boxes.exists(_.isInstanceOf[FloatBox]) shouldBe false

    // and it carries the typeset body
    val float = items.collectFirst { case f: FloatBox => f }.get.content
    texts(float) should (contain("boxed") and contain("chart"))
  }

  "a float heads the page its reference is on, ahead of the body text" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)

    proc.process("one \\topinsert{TOPFIG} two\n\n\\vfill\\eject after\n\n")
    t.end()

    doc.shipped.length shouldBe 2

    val page0 = texts(doc.shipped(0))
    page0.head shouldBe "TOPFIG"      // the float is the first thing on the page
    page0 should contain("one")       // the body it floated above is still there
    texts(doc.shipped(1)) should not contain "TOPFIG"
  }

  "several floats stack at the top in contribution order" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)

    proc.process("body \\topinsert{FIRST} more \\topinsert{SECOND} text\n\n")
    t.end()

    val page0 = texts(doc.shipped(0))
    page0.indexOf("FIRST") should be < page0.indexOf("SECOND")
    page0.indexOf("SECOND") should be < page0.indexOf("body")
  }

  "floated pages still build to exactly vsize, so float height counts against the page" in {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)
    t.set("vsize", 200.0)
    t.set("hsize", 200.0)

    val source =
      (1 to 8).map(i => s"\\topinsert{plot$i} word$i tail$i tag$i more$i stuff$i extra$i padding$i").mkString(" ")
    val out    = new java.io.ByteArrayOutputStream

    Console.withOut(out) {
      proc.process(source)
      t.end()
    }

    out.toString shouldBe empty
    doc.shipped.length should be > 1
    for page <- doc.shipped do page.height shouldBe 200.0 +- 1e-9

    // every float landed on some page exactly once
    for i <- 1 to 8 do doc.shipped.count(p => texts(p) contains s"plot$i") shouldBe 1
  }

  "a float that cannot share its reference page is carried to head a later one" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)
    t.set("vsize", 80.0)
    t.set("hsize", 200.0)

    // the page fills with paragraph lines, then a float is asked for near the bottom; counting its height
    // overflows the page, so the break lands before it and the float heads the next page
    proc.process("l1\n\nl2\n\nl3\n\n\\topinsert{LATE} after\n\n")
    t.end()

    doc.shipped.length should be > 1
    val onLate = doc.shipped.indexWhere(p => texts(p) contains "LATE")
    onLate should be > 0
    texts(doc.shipped(onLate)).head shouldBe "LATE"
  }
