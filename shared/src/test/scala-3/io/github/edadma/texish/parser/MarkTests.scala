package io.github.edadma.texish.parser

import io.github.edadma.texish.{
  Box,
  Builder,
  DocumentMode,
  Glue,
  HBox,
  MarkBox,
  Penalty,
  StubTypesetter,
  VBox,
  VBoxBuilder,
  VSpaceBox,
}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Marks label points in the document; the page builder records which marks land on each page (topmark = the
  * previous page's last mark, firstmark/botmark = this page's first and last, a markless page inheriting topmark
  * for all three), and marks issued mid-paragraph migrate out of their line to the vertical list.
  */
class MarkTests extends AnyFreeSpec with Matchers:

  private class MarkRecorder(t: StubTypesetter) extends DocumentMode(t):
    val records = new ArrayBuffer[(String, String, String)]
    override infix def add(box: Box): Unit =
      records += ((text("topmark"), text("firstmark"), text("botmark")))
      super.add(box)
    private def text(k: String): String = t.get(k) match
      case Some(Value.Text(s)) => s
      case v                   => s"?$v"

  private def fixture(): (StubTypesetter, MarkRecorder, Processor) =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new MarkRecorder(t)
    t.document = doc
    (t, doc, proc)

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  "\\mark in vertical mode adds a mark box" in {
    val (t, _, proc) = fixture()

    proc.process("\\mark{alpha}")

    val last = t.mode.asInstanceOf[Builder].last
    last shouldBe a[MarkBox]
    last.asInstanceOf[MarkBox].text shouldBe "alpha"
  }

  "a mark inside a paragraph migrates out of its line" in {
    val (t, _, proc) = fixture()
    t.set("hsize", 100.0) // force several lines

    // the mark sits in the paragraph's first line, whose gap always carries the club penalty
    proc.process("alpha \\mark{here} beta gamma delta epsilon zeta eta theta iota kappa")
    t.paragraph()

    val items = t.mode.asInstanceOf[Builder].list

    // the mark sits at the top level of the vertical list, not inside any line
    items.count(_.isInstanceOf[MarkBox]) shouldBe 1
    for line <- items.collect { case h: HBox => h } do
      line.boxes.exists(_.isInstanceOf[MarkBox]) shouldBe false

    // and it comes right after its line, before the penalty guarding the following glue
    val markIdx = items.indexWhere(_.isInstanceOf[MarkBox])
    items(markIdx - 1) shouldBe a[HBox]
    items(markIdx + 1) shouldBe a[Penalty]
  }

  "page marks track what lands on each page" in quietly {
    val (t, doc, proc) = fixture()

    proc.process("\\mark{a} one \\mark{b} more\n\n\\vfill\\eject \\mark{c} two\n\n\\vfill\\eject three\n\n")
    t.end()

    doc.records.toList shouldBe List(
      ("", "a", "b"),  // page 1: marks a and b
      ("b", "c", "c"), // page 2: one mark
      ("c", "c", "c"), // page 3: markless, inherits topmark
    )
  }

  "marks stay continuous when a paragraph breaks across pages" in quietly {
    val (t, doc, proc) = fixture()
    t.set("vsize", 80.0)
    t.set("hsize", 100.0)
    t.set("clubpenalty", 0.0)
    t.set("widowpenalty", 0.0)

    val source = (1 to 12).map(i => s"\\mark{$i} word$i$i word$i$i word$i$i").mkString(" ")

    proc.process(source)
    t.end()

    doc.records.length should be > 1
    doc.records.head._1 shouldBe ""
    // each page's topmark is the previous page's botmark, and marks advance in order
    for i <- 1 until doc.records.length do doc.records(i)._1 shouldBe doc.records(i - 1)._3
    val bots = doc.records.map(_._3.toInt)
    bots shouldBe bots.sorted
    bots.last shouldBe 12
  }

  "interline glue is inserted across a mark, measured from the last box" in {
    val t = new StubTypesetter
    t.set("baselineskip", Glue(20))

    class Line(val ascent: Double, val descent: Double) extends io.github.edadma.texish.ContentBox:
      val width: Double    = 10
      val xAdvance: Double = 10
      def draw(t: io.github.edadma.texish.Typesetter, x: Double, y: Double): Unit = ()

    val vb = new VBoxBuilder(t)
    vb add Line(5, 5)
    vb add MarkBox("here")
    vb add Line(5, 5)

    val box = vb.result.asInstanceOf[VBox]
    box.boxes.map(_.height) shouldBe Seq(10.0, 0.0, 10.0, 10.0)
    box.boxes(1) shouldBe a[MarkBox]
    box.boxes(2) shouldBe a[VSpaceBox]
  }
