package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, DocumentMode, Glue, Penalty, HeadlessTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Page-break control from the language: \penalty (numeric break desirability), \nobreak (forbid), \eject (end the
  * paragraph and force a break), and the pageno variable maintained by the document as pages ship.
  */
class PageBreakPrimitivesTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      page += 1

  private def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  private def capturing(): (HeadlessTypesetter, CapturingDocument, Processor) =
    val (t, proc) = fixture()
    val doc       = new CapturingDocument(t)
    t.document = doc
    (t, doc, proc)

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  "\\penalty adds a penalty box with the given value" in {
    val (t, proc) = fixture()
    proc.process("\\penalty {-200}")
    t.mode.asInstanceOf[Builder].last shouldBe a[Penalty]
    t.mode.asInstanceOf[Builder].last.asInstanceOf[Penalty].penalty shouldBe -200
  }

  "\\nobreak adds an inhibiting penalty" in {
    val (t, proc) = fixture()
    proc.process("\\nobreak")
    t.mode.asInstanceOf[Builder].last.asInstanceOf[Penalty].penalty shouldBe Penalty.Inhibit
  }

  // The document package keeps a section heading with the text it introduces by following the heading box with an
  // inhibiting penalty: that makes the glue beneath the heading a non-break, so a page break that would orphan the
  // heading at the foot of a page falls before it instead. Regression for a heading stranded at the page bottom.
  "a section heading is followed by an inhibiting penalty so it is not orphaned" in quietly {
    val (t, proc) = fixture()
    // A paragraph after the heading closes back to the page builder, so its list holds the section's vertical
    // items: the space above, the heading box, the \nobreak, then the space below.
    proc.process("\\use{document}\\section{Heading}\n\nbody text here\n\n")
    val items  = t.mode.asInstanceOf[Builder].list
    val penIdx = items.indexWhere {
      case p: Penalty => p.penalty == Penalty.Inhibit
      case _          => false
    }
    penIdx should be > 0
    items(penIdx - 1) should not be a[Glue]    // the heading line box, not glue
    items(penIdx - 1) should not be a[Penalty]
    items(penIdx + 1) shouldBe a[Glue]         // the glue under the heading, now non-breakable
  }

  "\\eject ends the paragraph and ships the page" in quietly {
    val (t, doc, proc) = capturing()
    proc.process("one two three\n\n\\eject")
    doc.shipped.length shouldBe 1
  }

  "\\eject on an empty page does not produce a blank page" in quietly {
    val (t, doc, proc) = capturing()
    proc.process("one\n\n\\eject\\eject two")
    doc.shipped.length shouldBe 1
  }

  "\\vfill\\eject fills the rest of the page" in {
    val (t, doc, proc) = capturing()
    t.set("vsize", 100.0)

    val out = new java.io.ByteArrayOutputStream
    Console.withOut(out)(proc.process("one\n\n\\vfill\\eject"))

    doc.shipped.length shouldBe 1
    doc.shipped(0).height shouldBe 100.0 +- 1e-9
    out.toString shouldBe empty
  }

  "\\vfill mid-paragraph ends the paragraph and fills the page bottom" in {
    val (t, doc, proc) = capturing()
    t.set("vsize", 100.0)
    t.set("topskip", Glue(0))

    // a single newline leaves the paragraph open: the fill must still land in the vertical list and absorb
    // the page's slack at the bottom, not set as horizontal space inside the paragraph's last line
    val out = new java.io.ByteArrayOutputStream
    Console.withOut(out)(proc.process("one two\n\\vfill\\eject"))

    out.toString shouldBe empty
    doc.shipped.length shouldBe 1
    doc.shipped(0).height shouldBe 100.0 +- 1e-9
    // the line sits at the top; the set fill below it holds everything else
    doc.shipped(0).boxes.last.height shouldBe 90.0 +- 1e-9
  }

  "\\vskip mid-paragraph ends the paragraph and skips vertically" in {
    val (t, proc) = fixture()

    proc.process("one two\n\\vskip 20pt more")
    t.paragraph()

    // the skip sits between the two paragraphs' lines at the top level of the vertical list
    val items = t.mode.asInstanceOf[Builder].list
    items.count(b => b.isInstanceOf[Glue] && b.asInstanceOf[Glue].naturalSize == 20.0) shouldBe 1
  }

  "a document ending in \\eject has no trailing blank page" in quietly {
    val (t, doc, proc) = capturing()

    proc.process("one\n\n\\vfill\\eject")
    t.end()

    doc.shipped.length shouldBe 1
  }

  "pageno starts at 1 and advances as pages ship" in quietly {
    val (t, proc) = fixture()
    t.getNumber("pageno") shouldBe 1.0

    proc.process("one\n\n\\eject two\n\n\\eject")
    t.getNumber("pageno") shouldBe 3.0
  }
