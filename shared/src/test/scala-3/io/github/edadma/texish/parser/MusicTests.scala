package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, CharBox, GlyphBox, HBox, HeadlessTypesetter, PictureBox, PictureOp, ShiftBox, TextExtents, Typesetter}
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
  private val AccentAbove   = 0xe4a0
  private val AccentBelow   = 0xe4a1
  private val StaccatoBelow = 0xe4a3
  private val Fermata       = 0xe4c0
  private val DynP          = 0xe520
  private val DynM          = 0xe521
  private val DynF          = 0xe522

  private class Capture extends HeadlessTypesetter:
    val pictures = ArrayBuffer[PictureBox]()
    val hboxes   = ArrayBuffer[HBox]()
    override infix def add(box: Box): Typesetter =
      box match
        case pb: PictureBox => pictures += pb
        case hb: HBox       => hboxes += hb
        case _              =>
      super.add(box)

  private def run(src: String, t: Capture = new Capture): Capture =
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(s"\\use{music}$src"))
    t

  // Process arbitrary music source (so a test can \set config before the score) and return the first picture.
  private def opsRaw(src: String): Vector[PictureOp] =
    val t = run(src)
    t.pictures should not be empty
    t.pictures.head.displayList

  // Every score's display list, in order — for a source with more than one \score.
  private def allOps(src: String): Vector[Vector[PictureOp]] =
    run(src).pictures.map(_.displayList).toVector

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
  // lyric text: the boxes \at places (syllables and hyphens), as (left x, baseline y, width) in drawing order
  private def texts(o: Vector[PictureOp]): Vector[(Double, Double, Double)] =
    o.collect { case PictureOp.Place(b: HBox, _, x, y) => (x, y, b.width) }
  // note heads: placed glyphs whose codepoint is one of the three notehead shapes
  private def heads(o: Vector[PictureOp]): Vector[(Int, Double, Double)] =
    glyphs(o).filter((cp, _, _) => cp == NoteWhole || cp == NoteHalf || cp == NoteBlack)
  // straight segments drawn at a given line width, as ((x0,y0),(x1,y1)); used to pick out hairpin rules (0.12*gap)
  private def rules(o: Vector[PictureOp], w: Double): Vector[((Double, Double), (Double, Double))] =
    val out = ArrayBuffer[((Double, Double), (Double, Double))]()
    var cur = 0.0
    for i <- o.indices do
      o(i) match
        case PictureOp.SetLineWidth(x) => cur = x
        case PictureOp.MoveTo(x0, y0) if math.abs(cur - w) < 1e-9 && i + 1 < o.length =>
          o(i + 1) match
            case PictureOp.LineTo(x1, y1) => out += (((x0, y0), (x1, y1)))
            case _                        =>
        case _ =>
    out.toVector

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

  "an articulation is drawn on the side away from the stem" in {
    // low C stems up, so its accent sits below the head (accentBelow); the high C' stems down, accent above
    glyphs(ops("c>")).filter((cp, _, _) => cp == AccentBelow) should have size 1
    glyphs(ops("c>")).filter((cp, _, _) => cp == AccentAbove) shouldBe empty
    glyphs(ops("c'>")).filter((cp, _, _) => cp == AccentAbove) should have size 1
    glyphs(ops("c")).filter((cp, _, _) => cp == AccentBelow || cp == AccentAbove) shouldBe empty
  }

  "several articulations stack outward from the head" in {
    // c>! carries both an accent and a staccato, and the second sits further from the head than the first
    val marks = glyphs(ops("c>!")).filter((cp, _, _) => cp == AccentBelow || cp == StaccatoBelow)
    marks.map(_._1) should contain allOf (AccentBelow, StaccatoBelow)
    val ys = marks.map(_._3)
    ys.distinct should have size 2 // stacked, not on top of each other
  }

  "a fermata arches above the note" in {
    val f = glyphs(ops("c2;")).filter((cp, _, _) => cp == Fermata)
    f should have size 1
    f.head._3 should be > 62.0 // above the top staff line
  }

  "a slur draws one curved arc over the run" in {
    ops("c d e").exists(_.isInstanceOf[PictureOp.CurveTo]) shouldBe false
    ops("( c d e )").count(_.isInstanceOf[PictureOp.CurveTo]) shouldBe 1
  }

  "a slur goes under the notes when every stem is up, and over them when one points down" in {
    // the arc's lowest/highest control point is its peak; compare it with the staff
    def peakY(o: Vector[PictureOp]): Double =
      o.collect { case PictureOp.CurveTo(_, y1, _, _, _, _) => y1 }.head
    peakY(ops("( c d e )")) should be < 30.0  // c d e stem up: the arc bows below the bottom line
    peakY(ops("( c' d' e' )")) should be > 62.0 // c' d' e' stem down: the arc bows above the top line
    peakY(ops("( b8 g8 )")) should be > 62.0  // mixed: one down-stem sends it above
  }

  "a flagged down-stem is long enough that its flag clears the head, by the flag's measured height" in {
    // a backend whose down flags are as tall as a real font's (26 and 30 points at this size), so the rule shows:
    // the stem reaches the flag's height plus three-quarters of a staff space (6), never less than 3.3 spaces
    class TallFlags extends Capture:
      override def glyphExtents(font: RenderFont, glyph: Int): TextExtents = glyph match
        case 0xe241 => TextExtents(0, -26, 6, 26, 6, 0) // flag8thDown
        case 0xe243 => TextExtents(0, -30, 6, 30, 6, 0) // flag16thDown
        case _      => super.glyphExtents(font, glyph)
    def stemLen(score: String): Double =
      val (y0, y1) = vlines(run(s"\\score{$score}", new TallFlags).pictures.head.displayList).head
      math.abs(y1 - y0)
    (stemLen("c'8") - stemLen("c'4")) shouldBe (32.0 - 3.3 * 8) +- 0.001
    (stemLen("c'16") - stemLen("c'4")) shouldBe (36.0 - 3.3 * 8) +- 0.001
    stemLen("c8") shouldBe stemLen("c4") // an up-stem flag curls away from the head; its stem is unchanged
  }

  "a tie joins two notes with a curve" in {
    ops("c c").count(_.isInstanceOf[PictureOp.CurveTo]) shouldBe 0
    ops("c- c").count(_.isInstanceOf[PictureOp.CurveTo]) shouldBe 1
  }

  "a dynamic lays its letters below the staff, left to right" in {
    val dyn = glyphs(ops("!mf c")).filter((cp, _, _) => cp == DynM || cp == DynF)
    dyn.map(_._1) shouldBe Vector(DynM, DynF) // mezzo then forte, in writing order
    dyn.foreach((_, _, y) => y should be < 30.0) // below the bottom staff line at y=30
    dyn(0)._2 should be < dyn(1)._2 // laid out left to right
  }

  "the dynamics lane drops to clear low notes" in {
    // the dynamic's baseline, measured from the bottom staff line (which moves up when the picture grows)
    def dynBelow(score: String): Double =
      val o = ops(score)
      // the staff lines are the horizontals that start at the staff's left end, half a staff space in
      val bottom = o.sliding(2).collect {
        case Vector(PictureOp.MoveTo(4.0, y0), PictureOp.LineTo(_, y1)) if y0 == y1 => y0
      }.min
      bottom - glyphs(o).filter((cp, _, _) => cp == DynP).head._3
    // a high note leaves the lane where it always sat, 2.6 staff spaces below the bottom line
    dynBelow("!p g'") shouldBe (2.6 * 8) +- 0.001
    // middle C (its head's bottom 1.5 spaces below the line) pushes it down: the letter's top — 8 points above
    // its baseline on this backend — keeps three-fifths of a space (4.8) clear of the head
    dynBelow("!p c") shouldBe (1.5 * 8 + 8 + 0.6 * 8) +- 0.001
    // and the picture grows to hold it: the lane stays above the picture's bottom edge
    glyphs(ops("!p c")).filter((cp, _, _) => cp == DynP).head._3 - 0.6 * 8 should be > 0.0
  }

  "lyrics move below a lowered dynamics lane" in {
    val o   = opsRaw("\\lyrics{one}\\score{!p c}")
    val dyn = glyphs(o).filter((cp, _, _) => cp == DynP).head._3
    val lyr = texts(o).head._2
    lyr should be < dyn - 0.6 * 8 // the syllable's baseline is below the p's descender
  }

  // every piece of text in a placed box, depth first, as (text, font, baseline shift from the box's own)
  private def runs(b: Box, shift: Double = 0): Vector[(String, io.github.edadma.texish.Font, Double)] = b match
    case c: CharBox  => Vector((c.text, c.font, shift))
    case s: ShiftBox => runs(s.box, shift + s.shift)
    case h: HBox     => h.boxes.toVector.flatMap(runs(_, shift))
    case _           => Vector()
  private def chordRuns(o: Vector[PictureOp]) =
    o.collect { case PictureOp.Place(b: HBox, _, _, y) if y > 62.0 => runs(b) }

  "a b or # in a chord name is a flat or sharp sign from the music font, as bold as the letters" in {
    // each sign is text set in the music font — the chord-symbol accidental — at the chord's weight, which a
    // one-weight music font provides as a synthetic bold
    val signs = chordRuns(opsRaw("\\score{\"Bbm7\" c \"F#\" d \"G7b9\" e}")).flatten
      .filter((t, _, _) => t == "\ued60" || t == "\ued62")
    signs.map(_._1) shouldBe Vector("\ued60", "\ued62", "\ued60")
    signs.foreach((_, f, _) => (f.typeface, f.syntheticBold) shouldBe ("bravura", true))
    chordRuns(opsRaw("\\score{\"Am7\" c}")).flatten.map(_._1).mkString shouldBe "Am7"
  }

  "a chord name is one box: the extension raised and smaller, the root and a slash bass on the baseline" in {
    val Vector(name) = chordRuns(opsRaw("\\score{\"G7/B\" c}"))
    name.map(_._1).mkString shouldBe "G7/B"
    def run(c: String) = name.find(_._1.contains(c)).get
    run("7")._3 should be < 0.0                       // raised (a negative shift is upward)
    run("7")._2.size should be < run("G")._2.size     // and smaller
    run("B")._3 shouldBe 0.0                          // the bass is back on the baseline, at full size
    run("B")._2.size shouldBe run("G")._2.size
  }

  "hairpins open the way they are written" in {
    // < opens a crescendo: the two rules meet at a point on the left and spread on the right
    val cr = rules(ops("< c d e ="), 0.96).filter { case ((x0, _), (x1, _)) => x0 != x1 }
    cr should have size 2
    cr.map(_._1._2).distinct should have size 1 // left ends coincide (the point)
    cr.map(_._2._2).distinct should have size 2 // right ends spread apart
    // > opens a diminuendo: spread on the left, meeting at a point on the right
    val dm = rules(ops("> c d e ="), 0.96).filter { case ((x0, _), (x1, _)) => x0 != x1 }
    dm should have size 2
    dm.map(_._1._2).distinct should have size 2 // left ends spread apart
    dm.map(_._2._2).distinct should have size 1 // right ends coincide (the point)
  }

  "lyrics set one syllable centred under each note, below the staff" in {
    val o  = opsRaw("\\lyrics{one two three}\\score{c d e}")
    val ts = texts(o)
    val hs = heads(o)
    ts should have size 3
    ts.foreach((_, y, _) => y should be < 30.0) // below the bottom staff line
    // every syllable's centre sits the same distance from its own head's left edge: each is centred on its note
    val offsets = ts.zip(hs).map { case ((x, _, w), (_, hx, _)) => x + w / 2 - hx }
    offsets.foreach(_ shouldBe offsets.head +- 0.001)
  }

  "a score without lyrics keeps the fixed advance, and \\lyrics arms only the next score" in {
    val Vector(withLyrics, plain) =
      allOps("\\lyrics{Wonderfully marvellously}\\score{c d}\\score{c d}")
    texts(plain) shouldBe empty
    val xs = heads(plain).map(_._2)
    (xs(1) - xs(0)) shouldBe 22.0 +- 0.001
    // and the same notes with no \lyrics at all lay out identically to that plain score
    heads(ops("c d")) shouldBe heads(plain)
    texts(withLyrics) should have size 2
  }

  "long syllables push their notes apart so the words do not collide" in {
    val o  = opsRaw("\\lyrics{Wonderfully marvellously}\\score{c d}")
    val hs = heads(o).map(_._2)
    (hs(1) - hs(0)) should be > 22.0 // wider than the fixed advance
    val Vector((x0, _, w0), (x1, _, _)) = texts(o)
    (x1 - (x0 + w0)) shouldBe 6.0 +- 0.001 // exactly musiclyricgap of white space between the two words
    // short syllables need no extra room, so the advance stays the fixed one
    val short = heads(opsRaw("\\lyrics{a b}\\score{c d}")).map(_._2)
    (short(1) - short(0)) shouldBe 22.0 +- 0.001
  }

  "the score widens by exactly the room the lyrics added" in {
    val plain = run("\\score{c d e}").pictures.head.width
    val o     = run("\\lyrics{Wonderfully marvellously a}\\score{c d e}").pictures.head
    val hs    = heads(o.displayList).map(_._2)
    val added = (hs(1) - hs(0) - 22.0) + (hs(2) - hs(1) - 22.0)
    o.width shouldBe (plain + added) +- 0.001
  }

  "a -- joins two syllables with a hyphen centred between them" in {
    val ts = texts(opsRaw("\\lyrics{de -- cid}\\score{c d}"))
    ts should have size 3 // de, the hyphen, cid
    val Vector(de, cid, hy) = ts // the hyphen is drawn once the syllable after it is placed
    val gapL = hy._1 - (de._1 + de._3)
    val gapR = cid._1 - (hy._1 + hy._3)
    gapL shouldBe gapR +- 0.001
    gapL should be >= 6.0 - 0.001 // a hyphenated pair keeps a full gap on each side of its hyphen
    texts(opsRaw("\\lyrics{de cid}\\score{c d}")) should have size 2 // no -- , no hyphen
  }

  "a rest takes no syllable and _ leaves a note without one" in {
    val o  = opsRaw("\\lyrics{one _ two}\\score{c r d e}")
    val ts = texts(o)
    val hs = heads(o) // the rest is not a head, so these are c, d, e
    ts should have size 2
    // one under c; d is skipped by _; two under e
    val centre = (t: (Double, Double, Double)) => t._1 + t._3 / 2
    (centre(ts(1)) - hs(2)._2) shouldBe (centre(ts(0)) - hs(0)._2) +- 0.001
  }

  "more syllables than notes are dropped, and notes past the last syllable keep the fixed advance" in {
    texts(opsRaw("\\lyrics{a b c d}\\score{c d}")) should have size 2
    val o  = opsRaw("\\lyrics{a}\\score{c d e}")
    texts(o) should have size 1
    val xs = heads(o).map(_._2)
    (xs(2) - xs(1)) shouldBe 22.0 +- 0.001
  }

  "a score inside a line adds nothing beside its picture, with or without lyrics or chords" in {
    // everything \score does before its picture runs in the surrounding mode, so a stray source space there would
    // sit beside the score in a \centerline and push it off centre; the box around it must be the picture exactly
    for src <- Seq(
        "\\hbox{\\score{c d}}",
        "\\hbox{\\lyrics{Wonderfully marvellously}\\score{c d}}",
        "\\hbox{\\score{\"Gsus4\" c \"Am7\" d}}",
      )
    do
      val t = run(src)
      t.hboxes.map(_.width) should contain(t.pictures.head.width)
  }

  "a chord name is set above the staff, starting at its note's head" in {
    val o  = opsRaw("\\score{\"G\" c d \"C\" e}")
    val ts = texts(o)
    val hs = heads(o)
    ts should have size 2
    ts.foreach((_, y, _) => y should be > 62.0) // above the top staff line
    ts(0)._1 shouldBe hs(0)._2 +- 0.001 // G over c
    ts(1)._1 shouldBe hs(2)._2 +- 0.001 // C over e, the note after its token
  }

  "a chord token takes no room on the staff" in {
    // short names: the notes keep the fixed advance and the score is as wide as without them
    val o  = run("\\score{\"G\" c \"C\" d}").pictures.head
    val xs = heads(o.displayList).map(_._2)
    (xs(1) - xs(0)) shouldBe 22.0 +- 0.001
    o.width shouldBe run("\\score{c d}").pictures.head.width +- 0.001
    heads(o.displayList) shouldBe heads(ops("c d"))
  }

  "a chord name wider than the advance pushes the next chord along" in {
    // a name is several pieces (G, then the raised sus4); the first name's right edge is that of its last piece
    val o      = opsRaw("\\score{\"Gsus4\" c \"Cmaj7\" d}")
    val second = heads(o)(1)._2 // the second name starts at its note's head
    val right  = texts(o).filter(_._1 < second).map((x, _, w) => x + w).max
    (second - right) shouldBe 8.0 +- 0.001 // exactly musicchordgap between the names
    // but a wide name followed by a plain note leaves that note at the fixed advance
    val xs = heads(opsRaw("\\score{\"Gsus4\" c d \"C\" e}")).map(_._2)
    (xs(1) - xs(0)) shouldBe 22.0 +- 0.001
  }

  "a chord name can sit over a rest, and one with no note after it is not drawn" in {
    val o = opsRaw("\\score{\"F\" r4 c}")
    texts(o) should have size 1
    texts(opsRaw("\\score{c \"G\"}")) shouldBe empty
  }

  "chords and lyrics share a score: names above, syllables below" in {
    val o  = opsRaw("\\lyrics{one two}\\score{\"G\" c \"C\" d}")
    val ts = texts(o)
    ts should have size 4
    ts.count(_._2 > 62.0) shouldBe 2
    ts.count(_._2 < 30.0) shouldBe 2
  }

  "a score with chords reserves room above the staff; one without keeps its height" in {
    val plain = run("\\score{c d}").pictures.head
    val chord = run("\\score{\"G\" c d}").pictures.head
    chord.ascent should be > plain.ascent // a picture's height is its ascent
    // \score{c d} before and after a chord score is the same height: the lane is reserved per score
    run("\\score{c d}\\score{\"G\" c d}\\score{c d}").pictures.map(_.ascent) shouldBe
      Seq(plain.ascent, chord.ascent, plain.ascent)
  }

  // eight bars of four quarters, with a 4/4 time signature, set in systems of musicwidth 300
  private val eightBars = (1 to 8).map(_ => "c d e f").mkString(" | ") + " |."
  private def systems(extra: String = "", score: String = eightBars): Vector[PictureBox] =
    run(s"\\set musicwidth {300}\\set musictimenum {4}\\set musictimeden {4}$extra\\score{$score}").pictures.toVector

  "a score too long for one line breaks at barlines into justified systems" in {
    val ps = systems()
    ps.size should be > 1
    ps.init.foreach(_.width shouldBe 300.0 +- 0.001) // every system but the last runs the full measure
    ps.last.width should be <= 300.0 + 0.001
    // every note is set exactly once across the systems
    ps.map(p => heads(p.displayList).size).sum shouldBe 32
  }

  "every system repeats the clef; only the first carries the time signature" in {
    val ps = systems()
    ps.foreach(p => glyphs(p.displayList).head._1 shouldBe GClef)
    def figures(p: PictureBox) = glyphs(p.displayList).count((cp, _, _) => cp >= TimeSig0 && cp <= TimeSig0 + 9)
    figures(ps.head) shouldBe 2
    ps.tail.foreach(figures(_) shouldBe 0)
  }

  "lyrics run on across the systems, one syllable to a note" in {
    val words = (1 to 32).map(i => s"w$i").mkString(" ")
    val ps    = systems(s"\\lyrics{$words}")
    ps.map(p => texts(p.displayList).size).sum shouldBe 32
  }

  "a tie across a line break is drawn to the end of one line and from the start of the next" in {
    // a measure only one bar wide, so the tied whole notes fall on two lines: one curve on each
    val ps = systems("\\set musicwidth {90}", "c1- | c1 |.")
    ps.size shouldBe 2
    val withTie = ps.map(p => p.displayList.count(_.isInstanceOf[PictureOp.CurveTo]))
    withTie.sum shouldBe 2
    withTie.count(_ == 1) shouldBe 2
  }

  "a bar too wide for any line gets a line of its own rather than looping or splitting it" in {
    val ps = systems(score = "c d e f g a b c' d' e' f' g' | c d |.")
    ps.size shouldBe 2
    ps.head.width should be > 300.0 // overfull, and left at its natural spacing
  }

  "a score that fits on one line is a single picture at its natural width" in {
    val ps = systems(score = "c d e f |.")
    ps.size shouldBe 1
    ps.head.width should be < 300.0
  }
