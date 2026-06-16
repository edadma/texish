package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, DocumentMode, HeadlessTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** A document of many paragraphs flows across many pages with no glue-setting complaints: with raggedbottom set,
  * every page — including one holding only a mid-paragraph fragment, which has no stretchable glue of its own —
  * is set to exactly vsize, and pageno tracks the count.
  */
class MultiPageStressTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box) // stub drawing is a no-op; this keeps page and pageno maintenance live

  "a long document paginates cleanly" in {
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)

    val doc = new CapturingDocument(t)
    t.document = doc
    t.set("vsize", 200.0)
    t.set("raggedbottom", 1.0)

    val source = (1 to 40)
      .map(i => s"Paragraph $i begins here. " + "filler words follow it onward " * 5)
      .mkString("\n\n")

    val out = new java.io.ByteArrayOutputStream

    Console.withOut(out) {
      proc.process(source)
      t.end()
    }

    val pages = doc.shipped.length

    pages should be > 5
    // every page is glue-set to exactly vsize
    for page <- doc.shipped do page.height shouldBe 200.0 +- 1e-6
    // pageno has advanced past the last shipped page
    t.getNumber("pageno") shouldBe (pages + 1).toDouble
    // and no page was reported underfull or overfull
    out.toString shouldBe empty
  }
