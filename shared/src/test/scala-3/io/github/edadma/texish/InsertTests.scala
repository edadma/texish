package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Footnotes ride the vertical list as zero-size inserts (see InsertBox): the page builder counts the insert's
  * content against the page from the moment it is contributed, breaks pages so reference and footnote stay
  * together, and at shipout moves the content to the foot of the page below the separator rule.
  */
class InsertTests extends AnyFreeSpec with Matchers:

  private class Line(val ascent: Double, val descent: Double = 0) extends ContentBox:
    val width: Double    = 10
    val xAdvance: Double = 10
    def draw(t: Typesetter, x: Double, y: Double): Unit = ()

  private class Note(val ascent: Double) extends ContentBox:
    val descent: Double  = 0
    val width: Double    = 10
    val xAdvance: Double = 10
    def draw(t: Typesetter, x: Double, y: Double): Unit = ()

  private class CapturingDocument(t: Typesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      page += 1

  // footnotesep 10, the 0.4 rule, and the 2.6 gap below it: the separator above the footnote block costs 13
  private def setup(vsize: Double): (CapturingDocument, PageMode) =
    val t = new HeadlessTypesetter
    t.set("vsize", vsize)
    t.set("footnotesep", Glue(10))
    t.set("raggedbottom", 1.0)
    val doc = new CapturingDocument(t)
    t.document = doc
    (doc, new PageMode(t))

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  "footnote content moves to the foot of the page below the rule" in quietly {
    val (doc, pm) = setup(100)
    val note      = Note(20)

    pm add Line(40)
    pm add InsertBox(note, 12)
    pm.newpage()

    doc.shipped.length shouldBe 1

    val page = doc.shipped(0)

    // the page is still exactly vsize: body built to vsize minus the footnote block
    page.height shouldBe 100.0 +- 1e-9
    // the insert itself is gone; its content sits last, under the rule and the gap below it
    page.boxes.count(_.isInstanceOf[InsertBox]) shouldBe 0
    page.boxes.last should be theSameInstanceAs note
    page.boxes(page.boxes.length - 2).height shouldBe 2.6 +- 1e-9
    page.boxes(page.boxes.length - 3) shouldBe a[RuleBox]
    // everything above the separator occupies what the footnote block left over
    page.boxes.dropRight(4).map(_.height).sum shouldBe (100.0 - 20 - 13) +- 1e-9
  }

  "a footnote shrinks the room available to the body" in quietly {
    val (doc, pm) = setup(100)

    pm add Line(40)
    pm add InsertBox(Note(20), 12)
    pm add Glue(10, 5, 5)
    pm add Line(40) // 90 alone would fit, but the footnote block's 33 pushes it over

    doc.shipped.length shouldBe 1
    doc.shipped(0).boxes.count(_.isInstanceOf[Line]) shouldBe 1
    // the footnote stayed on the page of its reference
    doc.shipped(0).boxes.exists(_.isInstanceOf[RuleBox]) shouldBe true
    pm.list.count(_.isInstanceOf[Line]) shouldBe 1
    pm.list.exists(_.isInstanceOf[InsertBox]) shouldBe false
  }

  "an insert carried over a break takes its footnote to the next page" in quietly {
    val (doc, pm) = setup(100)
    val note      = Note(40)

    pm add Line(40)
    pm add Glue(10, 5, 5)
    pm add Line(40)
    pm add InsertBox(note, 12) // 90 + 53 overflows: the insert's line moves over, and the insert with it

    doc.shipped.length shouldBe 1
    doc.shipped(0).boxes.exists(_.isInstanceOf[RuleBox]) shouldBe false
    pm.list.exists(_.isInstanceOf[InsertBox]) shouldBe true

    pm.newpage()

    doc.shipped.length shouldBe 2
    doc.shipped(1).height shouldBe 100.0 +- 1e-9
    doc.shipped(1).boxes.last should be theSameInstanceAs note
  }

  "two footnotes on a page share one separator" in quietly {
    val (doc, pm) = setup(200)
    val n1        = Note(20)
    val n2        = Note(30)

    pm add Line(40)
    pm add InsertBox(n1, 40)
    pm add Glue(10, 5, 5)
    pm add Line(40)
    pm add InsertBox(n2, 40)
    pm.newpage()

    val page = doc.shipped(0)

    page.height shouldBe 200.0 +- 1e-9
    page.boxes.count(_.isInstanceOf[RuleBox]) shouldBe 1
    // contents stack at the foot in document order, the second led off the first by interline glue: with leading 40
    // and n1's depth 0 and n2's ascent 30, a 10-high spacer sits between them, so their baselines are 40 apart
    val i1 = page.boxes.indexWhere(_ eq n1)
    page.boxes(i1) should be theSameInstanceAs n1
    page.boxes(i1 + 1).height shouldBe 10.0 +- 1e-9
    page.boxes(i1 + 2) should be theSameInstanceAs n2
  }

  "pages with footnotes never overrun vsize" in quietly {
    val (doc, pm) = setup(100)

    for i <- 1 to 6 do
      if i > 1 then pm add Glue(10, 5, 5)
      pm add Line(40)
      pm add InsertBox(Note(10), 12)

    pm.newpage()

    doc.shipped.length should be > 1
    for page <- doc.shipped do
      page.height shouldBe 100.0 +- 1e-9
      page.boxes.count(_.isInstanceOf[RuleBox]) shouldBe 1
  }
