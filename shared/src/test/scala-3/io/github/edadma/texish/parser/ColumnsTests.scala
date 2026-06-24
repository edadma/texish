package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, DocumentMode, HBox, HSpaceBox, HeadlessTypesetter, VerticalBox, VTop}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues

import scala.collection.mutable.ArrayBuffer

/** \columns sets its body as several balanced side-by-side columns: the body is typeset once at the column
  * width, divided into equal-height pieces by \vsplit's breaking, and the pieces stood next to each other with a
  * gutter between. These tests check the resulting horizontal box has the right number of column vboxes and
  * gutters, that the columns come out roughly level, and that the gutter width is configurable.
  */
class ColumnsTests extends AnyFreeSpec with Matchers with OptionValues:

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[Box]
    override infix def add(box: Box): Unit =
      shipped += box
      super.add(box)

  private def render(src: String): Seq[Box] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\set raggedbottom {1}\n" + src + "\n"))
    t.end()
    doc.shipped.toSeq

  // the column block is the one horizontal box whose children are themselves vertical boxes (each a \vtop, so
  // the columns top-align rather than aligning on their unequal last baselines)
  private def columnsHBox(boxes: Seq[Box]): Option[HBox] =
    def find(b: Box): Option[HBox] = b match
      case h: HBox if h.boxes.exists(_.isInstanceOf[VTop]) => Some(h)
      case h: HBox                                         => h.boxes.iterator.flatMap(find).nextOption()
      case v: VerticalBox                                  => v.boxes.iterator.flatMap(find).nextOption()
      case _                                               => None
    boxes.iterator.flatMap(find).nextOption()

  private val body =
    "{\\noindent The quick brown fox jumps over the lazy dog, and then, gathering its breath, it turns and " +
      "jumps back again, because a single leap is never quite enough to settle an old argument between a fox " +
      "and a dog, who had after all only been keeping watch over the sheep in the meadow by the river.}"

  "\\columns{3} produces three column vboxes with two gutters between them" in {
    val hb = columnsHBox(render("\\set hsize {320}\\columns{3}" + body)).value
    hb.boxes.count(_.isInstanceOf[VTop]) shouldBe 3
    hb.boxes.count(_.isInstanceOf[HSpaceBox]) shouldBe 2
  }

  "the columns come out roughly level" in {
    val hb      = columnsHBox(render("\\set hsize {320}\\columns{3}" + body)).value
    val heights = hb.boxes.collect { case v: VTop => v.height }
    heights.foreach(_ should be > 0.0)            // every column carries content
    (heights.max - heights.min) should be < 18.0  // a surplus line goes to the earlier columns, so the last
    //                                                runs at most a line short — far tighter than naive first-fit
    // The last column is never the tallest: an uneven division puts the extra line(s) in the earlier columns
    // (the newspaper convention), so the final column is the shortest. Balancing by height instead dumped the
    // remainder into the last column and left it the tallest.
    heights.last should be <= (heights.head + 0.01)
  }

  "\\columns{1} is a single column with no gutter" in {
    val hb = columnsHBox(render("\\set hsize {320}\\columns{1}" + body)).value
    hb.boxes.count(_.isInstanceOf[VTop]) shouldBe 1
    hb.boxes.count(_.isInstanceOf[HSpaceBox]) shouldBe 0
  }

  "the gutter width is set by the gap option" in {
    val hb = columnsHBox(render("\\set hsize {320}\\columns gap:40 {2}" + body)).value
    hb.boxes.collect { case s: HSpaceBox => s.width } shouldBe List(40.0)
  }

  "narrower columns than the page measure" in {
    // three columns of a 320pt measure are each well under a third of it once the gutters are taken out
    val hb       = columnsHBox(render("\\set hsize {320}\\columns{3}" + body)).value
    val colWidth = hb.boxes.collectFirst { case v: VTop => v.width }.value
    colWidth should be < 110.0
  }
