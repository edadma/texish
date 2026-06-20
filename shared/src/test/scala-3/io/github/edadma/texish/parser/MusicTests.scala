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
  * an accidental sits to the left of its head, a beamed run shares a beam tilted to the melody, and a time signature stacks figures.
  * (Default config: gap 8, pad 30, left 44, so the bottom line is y=30 and a diatonic step is 4 units.)
  */
class MusicTests extends AnyFreeSpec with Matchers:

  // SMuFL codepoints, matching packages/music.texish.
  private val NoteWhole = 0xe0a2
  private val NoteHalf  = 0xe0a3
  private val NoteBlack = 0xe0a4
  private val GClef     = 0xe050
  private val CClef     = 0xe05c
  private val AccFlat   = 0xe260
  private val AccSharp  = 0xe262
  private val TimeSig0  = 0xe080
  private val Flag16Up  = 0xe242
  private val AugDot    = 0xe1e7

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
  // beam segments as ((x0,y0),(x1,y1)). Beams are the only rules drawn at 0.5*gap (=4.0 at the default gap),
  // so tracking the current line width isolates them from stems (0.12*gap) and staff/ledger lines, whatever
  // their slope or height — a contour beam need not be horizontal or sit above the staff.
  private def beams(o: Vector[PictureOp]): Vector[((Double, Double), (Double, Double))] =
    val out = ArrayBuffer[((Double, Double), (Double, Double))]()
    var w   = 0.0
    for i <- o.indices do
      o(i) match
        case PictureOp.SetLineWidth(x) => w = x
        case PictureOp.MoveTo(x0, y0) if w == 4.0 && i + 1 < o.length =>
          o(i + 1) match
            case PictureOp.LineTo(x1, y1) => out += (((x0, y0), (x1, y1)))
            case _                        =>
        case _ =>
    out.toVector
  // note heads: placed glyphs whose codepoint is one of the three notehead shapes
  private def heads(o: Vector[PictureOp]): Vector[(Int, Double, Double)] =
    glyphs(o).filter((cp, _, _) => cp == NoteWhole || cp == NoteHalf || cp == NoteBlack)

  "the staff is five horizontal lines" in {
    // g sits on the staff (no ledger lines), so the only horizontal segments are the five staff lines
    hlines(ops("g")).distinct.sorted shouldBe Vector(30.0, 38.0, 46.0, 54.0, 62.0)
  }

  "a pitch's height follows its diatonic step" in {
    heads(ops("e")).head._3 shouldBe 30.0 // E is the bottom line
    heads(ops("f")).head._3 shouldBe 34.0 // one step up is half a staff space
    heads(ops("g")).head._3 shouldBe 38.0 // the second line
    // successive notes advance by musicnote (22) along a common baseline
    val xs = heads(ops("e e e")).map(_._2)
    (xs(1) - xs(0)) shouldBe 22.0 +- 0.001
    (xs(2) - xs(1)) shouldBe 22.0 +- 0.001
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

  "a beamed run draws a beam that tilts with the melody" in {
    // c d e rises, so the beam over the run rises too: its segments are sloped (the two ends differ in height),
    // not the flat bar a fixed beam height would give. The run carries no eighth-note flags of its own.
    val bs = beams(ops("[ c d e ]"))
    bs should not be empty
    bs.foreach { case ((_, y0), (_, y1)) => y1 should be > y0 } // each segment climbs to the right
    // and a level run (one repeated pitch) draws a flat beam, confirming the slope tracks the pitches
    beams(ops("[ c c c ]")).foreach { case ((_, y0), (_, y1)) => y1 shouldBe y0 }
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

  "a positive key signature writes sharps in order, before the notes" in {
    val sharps = glyphs(opsRaw("\\set musickey {2}\\score{c d}")).filter((cp, _, _) => cp == AccSharp)
    sharps should have size 2
    sharps.map(_._3) shouldBe Vector(62.0, 50.0) // F# on the top line, C# in the third space (treble)
    sharps(0)._2 should be < sharps(1)._2        // written left to right
    sharps(1)._2 should be < heads(opsRaw("\\set musickey {2}\\score{c d}")).head._2 // ahead of the first note
  }

  "a negative key signature writes that many flats" in {
    val flats = glyphs(opsRaw("\\set musickey {-3}\\score{c}")).filter((cp, _, _) => cp == AccFlat)
    flats should have size 3
    flats.map(_._3) shouldBe Vector(46.0, 58.0, 42.0) // Bb middle line, Eb top space, Ab second space (treble)
  }

  "the music font is configurable" in {
    // every stamped glyph is set in the chosen SMuFL face, not hard-wired to Bravura
    val faces = opsRaw("\\set musicfont {petaluma}\\score{c d}")
      .collect { case PictureOp.Place(g: GlyphBox, _, _, _) => g.font.typeface }.distinct
    faces shouldBe Vector("petaluma")
  }

  "a dotted note places an augmentation dot to the right of the head" in {
    val dot = glyphs(ops("c4.")).filter((cp, _, _) => cp == AugDot)
    dot should have size 1
    dot.head._2 should be > heads(ops("c4.")).head._2 // the dot sits right of the head
    glyphs(ops("c4")).filter((cp, _, _) => cp == AugDot) shouldBe empty
  }

  "a multi-digit duration parses as one number, and a sixteenth gets its flag" in {
    // c16 must read as a sixteenth (flag16th), not a 1 then a 6; an eighth carries no 16th flag
    glyphs(ops("c16")).filter((cp, _, _) => cp == Flag16Up) should have size 1
    glyphs(ops("c8")).filter((cp, _, _) => cp == Flag16Up) shouldBe empty
  }

  "a beamed run of sixteenths draws a second beam below the first" in {
    // a two-note run draws one beam span between the stems; a sixteenth run adds a second, parallel span below it
    beams(ops("[ c16 d16 ]")) should have size 2
    beams(ops("[ c8 d8 ]")) should have size 1
    // the second beam runs parallel to and below the first
    val Vector(primary, secondary) = beams(ops("[ c16 d16 ]"))
    secondary._1._2 should be < primary._1._2
    (primary._1._2 - secondary._1._2) shouldBe (primary._2._2 - secondary._2._2) +- 0.001
  }

  "the repeat barlines draw their dots" in {
    // the repeat dots lower to arcs; a plain barline draws none, and noteheads are glyphs not arcs
    ops("c |: d").exists(_.isInstanceOf[PictureOp.Arc]) shouldBe true
    ops("c | d").exists(_.isInstanceOf[PictureOp.Arc]) shouldBe false
  }

  "the alto clef puts middle C on the middle line" in {
    val gs = glyphs(opsRaw("\\set musicclef {alto}\\score{c d}"))
    gs.head._1 shouldBe CClef                 // a C-clef, not a G-clef
    heads(opsRaw("\\set musicclef {alto}\\score{c}")).head._3 shouldBe 46.0 // middle C on the middle line
  }

  "a stray space in a score is harmless" in {
    // the trailing space must not introduce a phantom note (regression for the empty-token guard)
    heads(opsRaw("\\score{c d e }")) should have size 3
  }
