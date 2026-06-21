package io.github.edadma.texish.parser

import io.github.edadma.texish.{
  Box,
  Builder,
  CharBox,
  HBox,
  HBoxBuilder,
  HSpaceBox,
  HeadlessTypesetter,
  LeaderBox,
  LeaderGlue,
  LeaderKind,
}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Leaders fill flexible space with repeated copies of a unit box. The glue stretches like any other while the
  * line is set, then resolves to a LeaderBox of the chosen width that tiles the unit. Each test character is six
  * units wide (HeadlessTypesetter), so a dot-plus-gap unit below is exactly ten units.
  */
class LeaderTests extends AnyFreeSpec with Matchers:

  // a typesetter that records where each glyph string is drawn, so a tiled leader can be counted and located
  private class Recording extends HeadlessTypesetter:
    val drawn = ArrayBuffer[(String, Double)]()
    override def drawString(text: String, x: Double, y: Double): Unit = drawn += ((text, x))

  private val t = new HeadlessTypesetter
  // unit = "." (6) + a 4-unit gap = 10 units wide
  private def dotUnit(ts: HeadlessTypesetter): HBox = new HBox(Seq(new CharBox(ts, "."), HSpaceBox(4)))

  "centred leaders place a whole number of copies, centred in the span" in {
    val rec = new Recording
    new LeaderBox(35, dotUnit(rec), LeaderKind.Centered).draw(rec, 0, 0)
    // floor(35/10) = 3 copies; leftover 5 split as 2.5 on each side
    rec.drawn.map(_._1) shouldBe Seq(".", ".", ".")
    rec.drawn.map(_._2) shouldBe Seq(2.5, 12.5, 22.5)
  }

  "aligned leaders tile on a grid anchored at the origin" in {
    val rec = new Recording
    new LeaderBox(35, dotUnit(rec), LeaderKind.Aligned).draw(rec, 0, 0)
    // grid at 0,10,20,30,... ; copies whose full width fits in [0,35]: 0,10,20
    rec.drawn.map(_._2) shouldBe Seq(0.0, 10.0, 20.0)
  }

  "a fil-stretch leader absorbs all the slack when the box is set" in {
    val hb = new HBoxBuilder(t, 50) // \hbox to 50
    hb add new CharBox(t, "ab")     // 12 units of content
    hb add new LeaderGlue(dotUnit(t), LeaderKind.Centered, 0, 1, 0, 1)

    val result = hb.result.asInstanceOf[HBox]
    val leader = result.boxes.collect { case l: LeaderBox => l }
    leader.length shouldBe 1
    leader.head.width shouldBe 38.0 +- 1e-9 // 50 - 12
  }

  // ---- primitive wiring (full processor stack) ----

  private def fixture(): (HeadlessTypesetter, Processor) =
    val ht      = new HeadlessTypesetter
    val handler = new TypesetterHandler(ht)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    ht.document = new io.github.edadma.texish.DocumentMode(ht)
    (ht, proc)

  private def leadersIn(ht: HeadlessTypesetter): List[LeaderGlue] =
    ht.mode.asInstanceOf[Builder].list.collect { case l: LeaderGlue => l }

  "\\dotfill adds centred dot leaders" in {
    val (ht, proc) = fixture()
    proc.process("entry\\dotfill 5")

    val ls = leadersIn(ht)
    ls.length shouldBe 1
    ls.head.kind shouldBe LeaderKind.Centered
    ls.head.stretchOrder shouldBe 1 // fills the line
  }

  "\\leaders reads a unit box and a glue" in {
    val (ht, proc) = fixture()
    proc.process("a\\leaders\\hbox{.}{0pt plus 1fil}b")

    val ls = leadersIn(ht)
    ls.length shouldBe 1
    ls.head.kind shouldBe LeaderKind.Aligned
    ls.head.stretchOrder shouldBe 1
  }

  "\\hrulefill adds a rule leader that stretches" in {
    val (ht, proc) = fixture()
    proc.process("a\\hrulefill b")

    val ls = leadersIn(ht)
    ls.length shouldBe 1
    ls.head.stretchOrder shouldBe 1
  }
