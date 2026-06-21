package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, DocumentMode, Glue, HeadlessTypesetter, Typesetter, VBox, VSpaceBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** \vsplit cuts a saved vbox at the latest legal breakpoint no taller than a target height — the same first-fit
  * rule the page builder uses — handing back the top piece and leaving the remainder in the register. The split
  * algorithm is checked directly on a hand-built list, and the primitive is checked end-to-end on registers set
  * from known boxes so the split is exactly predictable.
  */
class VsplitTests extends AnyFreeSpec with Matchers:

  // a content box of a known height (ascent only), so a list's heights are easy to reason about
  private class Block(h: Double) extends Box:
    val ascent: Double   = h
    val descent: Double  = 0.0
    val width: Double    = 10.0
    val xAdvance: Double = 10.0
    val isSpace: Boolean = false
    def draw(t: Typesetter, x: Double, y: Double): Unit = ()

  // Block(10), Glue(5), Block(10), Glue(5), Block(10) — cumulative heights 0,10,15,25,30,40; the legal breaks are
  // the two glues, at prefix heights 10 and 25
  private def sample: Seq[Box] =
    Seq(new Block(10), Glue(5), new Block(10), Glue(5), new Block(10))

  "splits at the latest glue whose prefix still fits, dropping the break glue" in {
    val (top, rest) = splitVList(sample, 30)
    top.length shouldBe 3                 // Block, Glue, Block
    rest.head.isSpace shouldBe false      // remainder never begins with the dropped break glue
    rest.collect { case b: Block => b }.length shouldBe 1
  }

  "a smaller target breaks earlier" in {
    val (top, rest) = splitVList(sample, 20)
    top.length shouldBe 1                 // just the first Block
    rest.collect { case b: Block => b }.length shouldBe 2
  }

  "with no legal breakpoint the whole list is the top and nothing remains" in {
    val (top, rest) = splitVList(Seq(new Block(10), new Block(10)), 5)
    top.length shouldBe 2
    rest shouldBe empty
  }

  "a nobreak glue is not a legal breakpoint" in {
    val list        = Seq(new Block(10), Glue(5).noBreak, new Block(10))
    val (top, rest) = splitVList(list, 12)
    top.length shouldBe 3 // no legal break, so it cannot cut at the nobreak glue
    rest shouldBe empty
  }

  "the resolved interline space of a built vbox (a VSpaceBox) is a legal breakpoint" in {
    // a finalized vbox holds VSpaceBox where the live list had flexible Glue; \vsplit must break there too
    val list        = Seq(new Block(10), new VSpaceBox(5), new Block(10), new VSpaceBox(5), new Block(10))
    val (top, rest) = splitVList(list, 20)
    top.length shouldBe 1
    rest.collect { case b: Block => b }.length shouldBe 2
  }

  // ---- the primitive end-to-end ----------------------------------------------

  private def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    t.document = new DocumentMode(t)
    (t, proc)

  private def register(t: HeadlessTypesetter, name: String): Option[VBox] = t.get(name) match
    case Some(Value.Native(b: VBox)) => Some(b)
    case _                           => None

  "\\setbox top \\vsplit src captures the top and leaves the remainder in the source register" in {
    val (t, proc) = fixture()
    t.set("col", Value.Native(new VBox(Seq(new Block(10), Glue(5), new Block(10), Glue(5), new Block(10)))))

    proc.process("\\setbox top \\vsplit col to:30")

    register(t, "top").map(_.boxes.length) shouldBe Some(3)
    register(t, "col").map(_.boxes.collect { case b: Block => b }.length) shouldBe Some(1)
  }

  "splitting a register down to nothing empties it" in {
    val (t, proc) = fixture()
    t.set("col", Value.Native(new VBox(Seq(new Block(10), Glue(5), new Block(10)))))

    proc.process("\\setbox top \\vsplit col to:30")

    register(t, "top").map(_.boxes.length) shouldBe Some(3) // the whole list fits the target, so it is taken intact
    t.get("col") shouldBe Some(Value.Undefined)
  }
