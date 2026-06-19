package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, GlyphBox, HeadlessTypesetter, PictureBox, PictureOp, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The `music` package (packages/music.texish) drawn through the full parser. A score lowers to a five-line staff
  * and per-note geometry on the picture layer: the heads, clef, accidentals, flags, rests and time-signature figures
  * are Bravura glyphs stamped with \fontglyph (each a placed [[GlyphBox]]), while stems, beams, staff lines and
  * ledger lines are ruled. The headless glyph seam uses a codepoint as its own glyph index, so a placed glyph's
  * `glyph` field is the SMuFL codepoint — which lets these tests check which symbol landed where. They cover the
  * parts that carry the music: the staff is five lines, a pitch's height follows its diatonic step, stems flip at
  * the middle line, the duration decides head shape and whether there is a stem, off-staff notes get ledger lines,
  * an accidental sits to the left of its head, a beamed run shares a flat beam, and a time signature stacks figures.
  * (Default config: gap 8, pad 30, left 44, so the bottom line is y=30 and a diatonic step is 4 units.)
  */
class MusicTests extends AnyFreeSpec with Matchers:

  // SMuFL codepoints, matching packages/music.texish.
  private val NoteWhole = 0xe0a2
  private val NoteHalf  = 0xe0a3
  private val NoteBlack = 0xe0a4
  private val GClef     = 0xe050
  private val AccSharp  = 0xe262
  private val TimeSig0  = 0xe080

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

  // straight segments: a MoveTo immediately followed by a LineTo (note heads are glyphs, so they don't appear)
  private def hlines(o: Vector[PictureOp]): Vector[Double] =
    o.sliding(2).collect { case Vector(PictureOp.MoveTo(_, y0), PictureOp.LineTo(_, y1)) if y0 == y1 => y0 }.toVector
  private def vlines(o: Vector[PictureOp]): Vector[(Double, Double)] =
    o.sliding(2).collect { case Vector(PictureOp.MoveTo(x0, y0), PictureOp.LineTo(x1, y1)) if x0 == x1 => (y0, y1) }.toVector
  // placed glyphs as (codepoint, x, y) — the headless seam makes the glyph index equal the codepoint
  private def glyphs(o: Vector[PictureOp]): Vector[(Int, Double, Double)] =
    o.collect { case PictureOp.Place(g: GlyphBox, _, x, y) => (g.glyph, x, y) }
  // note heads: placed glyphs whose codepoint is one of the three notehead shapes
  private def heads(o: Vector[PictureOp]): Vector[(Int, Double, Double)] =
    glyphs(o).filter((cp, _, _) => cp == NoteWhole || cp == NoteHalf || cp == NoteBlack)

  "the staff is five horizontal lines" in {
    // g sits on the staff (no ledger lines), so the only horizontal segments are the five staff lines
    hlines(ops("g")).distinct.sorted shouldBe Vector(30.0, 38.0, 46.0, 54.0, 62.0)
  }

  "a pitch's height follows its diatonic step" in {
    heads(ops("e")).head shouldBe (NoteBlack, 44.0, 30.0) // E is the bottom line, at the first-note x (musicleft)
    heads(ops("f")).head._3 shouldBe 34.0                 // one step up is half a staff space
    heads(ops("g")).head._3 shouldBe 38.0                 // the second line
  }

  "the clef is stamped before the notes" in {
    val gs = glyphs(ops("g"))
    gs.head._1 shouldBe GClef            // the clef is the first glyph placed
    gs.head._2 should be < 44.0          // and it sits left of the first note
  }

  "the duration chooses the note-head shape" in {
    heads(ops("c1")).head._1 shouldBe NoteWhole
    heads(ops("c2")).head._1 shouldBe NoteHalf
    heads(ops("c4")).head._1 shouldBe NoteBlack
    heads(ops("c8")).head._1 shouldBe NoteBlack
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

  "an accidental places its sign to the left of the head" in {
    val sharp = glyphs(ops("+c")).filter((cp, _, _) => cp == AccSharp)
    sharp should have size 1
    sharp.head._2 should be < heads(ops("+c")).head._2 // the sign sits left of the head
    glyphs(ops("c")).filter((cp, _, _) => cp == AccSharp) shouldBe empty
  }

  "a beamed run draws a flat beam above the staff" in {
    // the beam is a horizontal segment at the common beam height (msTop + 1.6*gap), above the top staff line at 62,
    // and the run carries no eighth-note flags of its own
    hlines(ops("[ c d e ]")).filter(_ > 62.5) should not be empty
  }

  "a time signature stacks two figures after the clef" in {
    val figures = glyphs(opsRaw("\\set musictimenum {3}\\set musictimeden {4}\\score{c d}"))
      .filter((cp, _, _) => cp >= TimeSig0 && cp <= TimeSig0 + 9)
    figures.map(_._1) should contain theSameElementsAs Vector(TimeSig0 + 3, TimeSig0 + 4)
    // the numerator sits above the denominator (smaller y is higher on the page in picture space)
    val num = figures.find(_._1 == TimeSig0 + 3).get
    val den = figures.find(_._1 == TimeSig0 + 4).get
    num._3 should be > den._3
    num._2 shouldBe den._2 // centred on a common x
  }
