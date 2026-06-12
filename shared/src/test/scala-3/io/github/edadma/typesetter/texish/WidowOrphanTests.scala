package io.github.edadma.typesetter.texish

import io.github.edadma.typesetter.{Box, Builder, DocumentMode, HBox, Penalty, StubTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Widow/orphan protection: a paragraph emits clubpenalty after its first line and widowpenalty before its last
  * (interlinepenalty between the others), so with the 10000 defaults a page never ends right after a paragraph's
  * first line or begins with its last.
  */
class WidowOrphanTests extends AnyFreeSpec with Matchers:

  private val para = "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu"

  private class CapturingDocument(t: StubTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      page += 1

  private def fixture(): (StubTypesetter, Processor) =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    t.set("hsize", 100.0)
    (t, proc)

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  private def lines(items: Seq[Box]): Seq[Int] = // indices of the line boxes
    items.zipWithIndex.collect { case (b: HBox, i) => i }

  /** Typeset one paragraph and return the resulting vertical-list items. */
  private def listOf(source: String, set: (String, Double)*): List[Box] =
    val (t, proc) = fixture()
    for (k, v) <- set do t.set(k, v)
    proc.process(source)
    t.paragraph()
    t.mode.asInstanceOf[Builder].list

  "club and widow penalties bracket the paragraph's lines" in {
    val items = listOf(para)
    val ls    = lines(items)

    ls.length should be >= 3
    // a forbidding penalty immediately after the first line and after the second-to-last
    items(ls.head + 1).asInstanceOf[Penalty].penalty shouldBe 10000
    items(ls(ls.length - 2) + 1).asInstanceOf[Penalty].penalty shouldBe 10000
    // nothing but glue between the middle lines
    for k <- 1 until ls.length - 2 do items(ls(k) + 1) should not be a[Penalty]
  }

  "interlinepenalty applies between all other lines" in {
    val items = listOf(para, "interlinepenalty" -> 7.0)
    val ls    = lines(items)

    ls.length should be >= 4
    items(ls.head + 1).asInstanceOf[Penalty].penalty shouldBe 10007
    items(ls(1) + 1).asInstanceOf[Penalty].penalty shouldBe 7
    items(ls(ls.length - 2) + 1).asInstanceOf[Penalty].penalty shouldBe 10007
  }

  "zero penalties emit nothing" in {
    val items = listOf(para, "clubpenalty" -> 0.0, "widowpenalty" -> 0.0)

    items.exists(_.isInstanceOf[Penalty]) shouldBe false
  }

  /** Typeset two copies of the paragraph with the page sized by `vsizeOf` (given the line count of one paragraph
    * and the height of one line plus its interline gap) and return how many lines landed on the first page.
    */
  private def firstPageLines(vsizeOf: (Int, Double, Double) => Double, set: (String, Double)*): Int = quietly {
    val n = lines(listOf(para)).length // how many lines this paragraph makes at hsize 100

    val (t, proc) = fixture()
    val doc       = new CapturingDocument(t)
    t.document = doc
    for (k, v) <- set do t.set(k, v)

    val lineHeight = 10.0 // stub metrics: ascent 8, descent 2
    val gap        = t.getGlue("baselineskip").naturalSize - lineHeight // interline glue between such lines

    t.set("vsize", vsizeOf(n, lineHeight, gap))
    proc.process(s"$para\n\n$para")
    t.paragraph()

    doc.shipped.length shouldBe 1
    doc.shipped(0).boxes.count(_.isInstanceOf[HBox])
  }

  "orphan protection: a first line that would fit at the page bottom moves over with its paragraph" in {
    // room for the first paragraph plus exactly one more line
    def vsize(n: Int, line: Double, gap: Double) = n * line + (n - 1) * gap + (gap + line) + 1

    firstPageLines(vsize) shouldBe lines(listOf(para)).length // clubpenalty holds the line back
    firstPageLines(vsize, "clubpenalty" -> 0.0) shouldBe lines(listOf(para)).length + 1
  }

  "widow protection: a last line never starts a page alone" in {
    // room for everything except the second paragraph's last line
    def vsize(n: Int, line: Double, gap: Double) = (2 * n - 1) * line + (2 * n - 2) * gap + 1

    // with widow protection the break backs up one more line (clubpenalty off so it can)
    firstPageLines(vsize, "clubpenalty" -> 0.0) shouldBe 2 * lines(listOf(para)).length - 2
    firstPageLines(vsize, "clubpenalty" -> 0.0, "widowpenalty" -> 0.0) shouldBe 2 * lines(listOf(para)).length - 1
  }
