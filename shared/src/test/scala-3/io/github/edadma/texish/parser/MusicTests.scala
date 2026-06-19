package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, HeadlessTypesetter, PictureBox, PictureOp, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The `music` package (packages/music.texish) drawn through the full parser. A score lowers to a five-line staff,
  * a clef, and per-note geometry on the picture layer, so these check the parts that carry the music: the staff is
  * five lines, a pitch's height follows its diatonic step, stems flip direction at the middle line, the duration
  * decides whether there is a stem at all, and a note off the staff gets ledger lines. (Default config: gap 8,
  * pad 30, so the bottom line is y=30 and a diatonic step is 4 units.)
  */
class MusicTests extends AnyFreeSpec with Matchers:

  private class Capture extends HeadlessTypesetter:
    val pictures = ArrayBuffer[PictureBox]()
    override infix def add(box: Box): Typesetter =
      box match
        case pb: PictureBox => pictures += pb
        case _              =>
      super.add(box)

  // Process arbitrary music source (so a test can \set config before the score) and return the first picture.
  private def opsRaw(src: String): Vector[PictureOp] =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(s"\\use{music}$src"))
    t.pictures should not be empty
    t.pictures.head.displayList

  private def ops(score: String): Vector[PictureOp] =
    opsRaw(s"\\score{$score}")

  // straight segments: a MoveTo immediately followed by a LineTo (note heads are curves, so they don't appear)
  private def hlines(o: Vector[PictureOp]): Vector[Double] =
    o.sliding(2).collect { case Vector(PictureOp.MoveTo(_, y0), PictureOp.LineTo(_, y1)) if y0 == y1 => y0 }.toVector
  private def vlines(o: Vector[PictureOp]): Vector[(Double, Double)] =
    o.sliding(2).collect { case Vector(PictureOp.MoveTo(x0, y0), PictureOp.LineTo(x1, y1)) if x0 == x1 => (y0, y1) }.toVector
  // note-head centres: the Translate of each head group, minus the clef's own translate at x=20
  private def heads(o: Vector[PictureOp]): Vector[(Double, Double)] =
    o.collect { case PictureOp.Translate(x, y) if x >= 40 => (x, y) }

  "the staff is five horizontal lines" in {
    // g sits on the staff (no ledger lines), so the only horizontal segments are the five staff lines
    hlines(ops("g")).distinct.sorted shouldBe Vector(30.0, 38.0, 46.0, 54.0, 62.0)
  }

  "a pitch's height follows its diatonic step" in {
    heads(ops("e")).head shouldBe (48.0, 30.0)        // E is the bottom line (at the first-note x, musicleft)
    heads(ops("f")).head._2 shouldBe 34.0             // one step up is half a line gap
    heads(ops("g")).head._2 shouldBe 38.0             // the second line
  }

  "stems point up below the middle line and down above it" in {
    val (cy0, cy1) = vlines(ops("c")).head  // C is below the staff → stem up
    cy1 should be > cy0
    val (gy0, gy1) = vlines(ops("g'")).head // the G above the staff → stem down
    gy1 should be < gy0
  }

  "a whole note has no stem; shorter notes do" in {
    vlines(ops("c1")) shouldBe empty
    vlines(ops("c2")) should have size 1
    vlines(ops("c4")) should have size 1
  }

  "a note off the staff gets ledger lines" in {
    // middle C is one step below the bottom line, so it gets one ledger line at y=22, below the staff at y=30
    hlines(ops("c")).filter(_ < 30.0) shouldBe Vector(22.0)
  }

  "an accidental adds strokes to the left of the head" in {
    // a plain low C has only its stem as a straight vertical segment; the sharp adds its two uprights
    vlines(ops("c")) should have size 1
    vlines(ops("+c")).size should be > 1
  }

  "a beamed run draws a flat beam above the staff" in {
    // the beam is one horizontal segment at the common beam height (msTop + gap + 6 = 62 + 8 + 6 = 76),
    // well above the top staff line, and the run carries no eighth-note flags of its own
    hlines(ops("[ c d e ]")) should contain(76.0)
  }

  "a time signature places two numerals after the clef" in {
    // \at lowers each numeral to a placed box; nothing else in the score uses Place
    val placed = opsRaw("\\set musictimenum {3}\\set musictimeden {4}\\score{c d}")
      .collect { case p: PictureOp.Place => p }
    placed should have size 2
  }
