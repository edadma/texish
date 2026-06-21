package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, DocumentMode, Glue, HeadlessTypesetter, Penalty, Typesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The page builder chooses its break by cost (badness + penalty), as TeX does, not by taking the latest legal
  * break that happens to fit. These tests drive the page builder with hand-built boxes, glue and penalties so the
  * chosen break is exactly predictable: a mild penalty now steers the break away from an otherwise-fuller page,
  * and on a rigid page with no penalty the cost rule still lands on the latest break that fits — the old
  * first-fit behaviour, unchanged.
  */
class CostPageBreakTests extends AnyFreeSpec with Matchers:

  private class Block(h: Double) extends Box:
    val ascent: Double   = h
    val descent: Double  = 0.0
    val width: Double    = 10.0
    val xAdvance: Double = 10.0
    val isSpace: Boolean = false
    def draw(t: Typesetter, x: Double, y: Double): Unit = ()

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def fixture(vsize: Double): (HeadlessTypesetter, CapturingDocument) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    t.set("vsize", vsize)
    t.set("topskip", Glue(0)) // no space before the first box, so heights are exactly the box heights
    (t, doc)

  // count the Block boxes that landed on a shipped page
  private def blocks(v: VBox): Int = v.boxes.count(_.isInstanceOf[Block])

  "a mild penalty steers the break to an earlier, penalty-free page" in {
    // Breaks at the two glues and the penalty all fit a vsize-40 page. The later break sits at a \penalty 200;
    // the earlier glue break has a genuinely good badness (~12), so best-fit prefers it (cost 12 < 0.2 + 200),
    // while first-fit would simply take the latest break that fits and put one more block on the page.
    val (t, doc) = fixture(40)
    Console.withOut(new java.io.ByteArrayOutputStream) {
      t.add(new Block(10))
      t.add(Glue(5, 30)); t.add(new Block(10))
      t.add(Glue(5, 30)); t.add(new Block(2))
      t.add(Penalty(200))
      t.add(Glue(5, 30)); t.add(new Block(10))
      t.end()
    }
    blocks(doc.shipped(0)) shouldBe 2 // best-fit; first-fit would have placed 3 here
  }

  "with no penalty the cost rule lands on the latest break that fits, as first-fit did" in {
    // A rigid page (no stretch) where three blocks and their two gutters total 40, fitting vsize-45; a fourth
    // would overflow. Every fitting break is equally bad on a rigid page, so the tie goes to the later one — the
    // page carries as much as it can, exactly the old first-fit behaviour.
    val (t, doc) = fixture(45)
    Console.withOut(new java.io.ByteArrayOutputStream) {
      t.add(new Block(10))
      t.add(Glue(5)); t.add(new Block(10))
      t.add(Glue(5)); t.add(new Block(10))
      t.add(Glue(5)); t.add(new Block(10))
      t.end()
    }
    blocks(doc.shipped(0)) shouldBe 3 // 10 + 5 + 10 + 5 + 10 = 40 fits in 45; the fourth block would overflow
  }
