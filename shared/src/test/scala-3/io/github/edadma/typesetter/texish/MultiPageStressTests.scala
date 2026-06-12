package io.github.edadma.typesetter.texish

import io.github.edadma.typesetter.{Box, DocumentMode, StubTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** A document of many paragraphs flows across many pages with no glue-setting complaints: parskip's fil stretch
  * absorbs the slack on every page, every shipped page is set to exactly vsize, and pageno tracks the count.
  */
class MultiPageStressTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: StubTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box) // stub drawing is a no-op; this keeps page and pageno maintenance live

  "a long document paginates cleanly" in {
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)

    val doc = new CapturingDocument(t)
    t.document = doc
    t.set("vsize", 200.0)

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
