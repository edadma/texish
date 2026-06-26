package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, DocumentMode, Glue, HBox, HeadlessTypesetter, Penalty, VBox}
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

  "a list item carried across a page break is not given doubled interline glue" in quietly {
    val (t, doc, proc) = capturing()
    t.set("hsize", 200.0) // a narrow column so each item wraps to two lines
    t.set("vsize", 90.0)  // a short page so the list overflows and an item continues at the next page's top

    // each \item is a two-line paragraph; the page builder carries an item's lines onto the next page and used
    // to re-synthesise the interline glue they already carried, leaving two spacing boxes back to back between
    // the item's first and second line — visible as ~2.9pt of extra leading on exactly that one line.
    val items = (1 to 10).map(i => s"\\item word one two three four five six seven eight nine ten eleven twelve item $i")
    proc.process("\\use{document}\n" + items.mkString("\\begin{itemize}\n", "\n", "\n\\end{itemize}\n"))
    t.end()

    doc.shipped.length should be >= 2

    // The top-to-top advance between an item's first two lines is one baselineskip. Page 1 opens with a freshly
    // set item; page 2 opens with an item the page builder carried over the break. The carried item's first two
    // lines must advance by the same baselineskip — the bug re-synthesised the interline glue they already
    // carried, so they sat one extra glue apart.
    def firstItemAdvance(page: VBox): Double =
      val tops     = page.boxes.toSeq.scanLeft(0.0)(_ + _.height)
      val lineTops = page.boxes.toSeq.zip(tops).collect { case (_: HBox, top) => top }
      lineTops(1) - lineTops(0)

    firstItemAdvance(doc.shipped(1)) shouldBe firstItemAdvance(doc.shipped(0)) +- 0.01
  }
