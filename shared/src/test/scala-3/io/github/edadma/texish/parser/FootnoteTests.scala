package io.github.edadma.texish.parser

import io.github.edadma.texish.{
  Box,
  Builder,
  CharBox,
  DocumentMode,
  HBox,
  InsertBox,
  RuleBox,
  ShiftBox,
  HeadlessTypesetter,
  VBox,
}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** \footnote sets a raised marker number in the running text and typesets its body at the foot of whatever page
  * the marker lands on: the body rides the vertical list as a zero-size insert that migrates out of its line, the
  * page builder counts its height against the page, and shipout moves it below the separator rule.
  */
class FootnoteTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def fixture(): (HeadlessTypesetter, CapturingDocument, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    (t, doc, proc)

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  private def texts(box: Box): List[String] = box match
    case v: VBox     => v.boxes.toList.flatMap(texts)
    case h: HBox     => h.boxes.toList.flatMap(texts)
    case s: ShiftBox => texts(s.box)
    case c: CharBox  => List(c.text)
    case _           => Nil

  "\\footnote sets a raised marker and migrates the note out of the line" in {
    val (t, _, proc) = fixture()

    proc.process("alpha \\footnote{down low} beta gamma")
    t.paragraph()

    val items = t.mode.asInstanceOf[Builder].list

    // the insert sits at the top level of the vertical list, not inside any line
    items.count(_.isInstanceOf[InsertBox]) shouldBe 1
    for line <- items.collect { case h: HBox => h } do
      line.boxes.exists(_.isInstanceOf[InsertBox]) shouldBe false

    // the marker stayed in the line, as a raised "1"
    val markers = items.collect { case h: HBox => h }.flatMap(_.boxes).collect { case s: ShiftBox => s }
    markers.length shouldBe 1
    markers.head.shift should be < 0.0
    texts(markers.head) shouldBe List("1")

    // the note block carries the "N." prefix and the body text
    val note = items.collectFirst { case i: InsertBox => i }.get.content
    texts(note).take(3) shouldBe List("1.", "down", "low")
  }

  "the footnote body is set flush even when the marker falls in an indented paragraph" in {
    val (t, _, proc) = fixture()
    t.set("leftskip", 40.0)

    proc.process("indented \\footnote{note body} text")
    t.paragraph()

    val items = t.mode.asInstanceOf[Builder].list
    val note  = items.collectFirst { case i: InsertBox => i }.get.content.asInstanceOf[VBox]

    // the note's first line opens with the "1." marker, with no leading indent glue carried in from the
    // surrounding paragraph's leftskip
    val firstLine   = note.boxes.collectFirst { case h: HBox => h }.get
    val leadingWide = firstLine.boxes.takeWhile(b => !b.isInstanceOf[CharBox]).map(_.width).sum
    leadingWide shouldBe 0.0 +- 1e-9
    firstLine.boxes.collectFirst { case c: CharBox => c.text }.get shouldBe "1."
  }

  "the footnote counter numbers markers in order" in {
    val (t, _, proc) = fixture()

    proc.process("alpha \\footnote{one} beta \\footnote{two} gamma")
    t.paragraph()

    t.getNumber("footnoteno") shouldBe 2.0

    val items   = t.mode.asInstanceOf[Builder].list
    val markers = items.collect { case h: HBox => h }.flatMap(_.boxes).collect { case s: ShiftBox => s }

    markers.flatMap(texts) shouldBe List("1", "2")
  }

  "the footnote body is set smaller and the surrounding text is unaffected" in {
    val (t, _, proc) = fixture()

    proc.process("alpha \\footnote{small} beta")
    t.paragraph()

    val items = t.mode.asInstanceOf[Builder].list
    val note  = items.collectFirst { case i: InsertBox => i }.get.content

    val noteChars = texts(note)
    noteChars should contain("small")
    for
      line <- note.asInstanceOf[VBox].boxes.collect { case h: HBox => h }
      c    <- line.boxes.collect { case c: CharBox => c }
    do c.font.size shouldBe (10.0 * 0.8) +- 1e-9

    // the text after the footnote is back at full size
    val bodyChars = items.collect { case h: HBox => h }.flatMap(_.boxes).collect { case c: CharBox => c }
    bodyChars.find(_.text == "beta").get.font.size shouldBe 10.0 +- 1e-9
  }

  "a footnote lands at the foot of the page its reference is on" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)

    proc.process("one \\footnote{down here} two\n\n\\vfill\\eject after\n\n")
    t.end()

    doc.shipped.length shouldBe 2
    doc.shipped(0).boxes.exists(_.isInstanceOf[RuleBox]) shouldBe true
    texts(doc.shipped(0)) should contain("down")
    doc.shipped(1).boxes.exists(_.isInstanceOf[RuleBox]) shouldBe false
  }

  "footnoted pages still build to exactly vsize" in {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)
    t.set("vsize", 200.0)
    t.set("hsize", 200.0)

    val source =
      (1 to 8).map(i => s"word$i \\footnote{fn$i} tail$i tag$i more$i stuff$i extra$i padding$i").mkString(" ")
    val out    = new java.io.ByteArrayOutputStream

    Console.withOut(out) {
      proc.process(source)
      t.end()
    }

    out.toString shouldBe empty
    doc.shipped.length should be > 1

    for page <- doc.shipped do
      page.height shouldBe 200.0 +- 1e-9
      page.boxes.count(_.isInstanceOf[RuleBox]) should be <= 1

    // every footnote landed on some page exactly once
    for i <- 1 to 8 do doc.shipped.count(p => texts(p) contains s"fn$i") shouldBe 1
  }
