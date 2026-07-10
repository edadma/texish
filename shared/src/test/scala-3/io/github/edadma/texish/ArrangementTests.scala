package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The page arrangements — the output routine that places finished logical pages onto physical sheets. A simple
  * arrangement ships one page per sheet at the text origin, n-up tiles a grid in reading order, and the two-up
  * booklet imposes pages in saddle-stitch folding order, padding a short book to whole sheets with blanks. Metrics
  * come from the fixed [[HeadlessTypesetter]].
  */
class ArrangementTests extends AnyFreeSpec with Matchers:

  /** A stand-in for a page body that records where it is drawn and carries an identity. */
  private class Probe(val id: Int) extends ContentBox:
    var drawnAt: Option[(Double, Double)] = None
    val width: Double                     = 0
    val xAdvance: Double                  = 0
    val ascent: Double                    = 0
    val descent: Double                   = 0
    def draw(t: Typesetter, x: Double, y: Double): Unit = drawnAt = Some((x, y))

  /** A document that captures the sheets an arrangement ships rather than drawing them. */
  private class RecordingDoc(t0: Typesetter) extends DocumentMode(t0):
    val sheets = ArrayBuffer[SheetBox]()
    override def shipout(sheet: Box): Unit = sheet match
      case s: SheetBox => sheets += s
      case _           =>

  private def fixture(pw: Double = 100, ph: Double = 150): HeadlessTypesetter =
    val t = new HeadlessTypesetter
    t.set("paperwidth", pw)
    t.set("paperheight", ph)
    t.set("hoffset", 0.0)
    t.set("voffset", 0.0)
    t.set("pageno", 1.0)
    t

  /** The logical page id a composed page carries, or None for a blank pad page. */
  private def idOf(page: Box): Option[Int] = page match
    case s: SheetBox if s.placed.nonEmpty =>
      s.placed.head._1 match
        case p: Probe => Some(p.id)
        case _        => None
    case _ => None

  /** The (id, xoffset) of each page on a sheet, left to right. */
  private def layoutOf(sheet: SheetBox): Seq[(Option[Int], Double)] =
    sheet.placed.map { case (pg, dx, _) => (idOf(pg), dx) }

  "simple arrangement ships one sheet per page, body at the text origin" in {
    val t = new HeadlessTypesetter
    t.set("paperwidth", 200.0)
    t.set("paperheight", 300.0)
    t.set("hoffset", 10.0)
    t.set("voffset", 20.0)
    t.set("pageno", 1.0)

    val doc    = new DocumentMode(t)
    val probes = (0 until 3).map(new Probe(_))
    probes.foreach(doc.add)

    doc.printedPages.length shouldBe 3
    probes.foreach(_.drawnAt shouldBe Some((10.0, 20.0))) // each body lands at (hoffset, voffset)
  }

  "simple arrangement keeps the sheet the size of the page" in {
    SimpleArrangement.sheetSize(100, 150) shouldBe ((100.0, 150.0))
  }

  "two-up-booklet imposes pages two-up in saddle-stitch folding order" in {
    val t   = fixture(pw = 100)
    val doc = new RecordingDoc(t)
    doc.arrangement = new TwoUpBookletArrangement(None)

    val probes = (0 until 8).map(new Probe(_))
    probes.foreach(doc.add)
    doc.arrangement.flush(doc)

    // four sheets, front then back, outermost first: (8,1)(2,7)(6,3)(4,5) in one-based pages
    doc.sheets.length shouldBe 4
    doc.sheets.map(layoutOf) shouldBe Seq(
      Seq((Some(7), 0.0), (Some(0), 100.0)),
      Seq((Some(1), 0.0), (Some(6), 100.0)),
      Seq((Some(5), 0.0), (Some(2), 100.0)),
      Seq((Some(3), 0.0), (Some(4), 100.0)),
    )
  }

  "two-up-booklet pads before the last page so the last page stays on the outer sheet" in {
    val t   = fixture(pw = 100)
    val doc = new RecordingDoc(t)
    doc.arrangement = new TwoUpBookletArrangement(None)

    val probes = (0 until 3).map(new Probe(_)) // pads to 4: p0 p1 blank p2 — the blank goes before the last page
    probes.foreach(doc.add)
    doc.arrangement.flush(doc)

    doc.sheets.length shouldBe 2
    // front: (p2, p0); back: (p1, blank) — p0 first and p2 last both land on the outer sheet's outside faces
    doc.sheets.map(layoutOf) shouldBe Seq(
      Seq((Some(2), 0.0), (Some(0), 100.0)),
      Seq((Some(1), 0.0), (None, 100.0)),
    )
  }

  "two-up-booklet folds into fixed signatures when a size is given" in {
    val t   = fixture(pw = 100)
    val doc = new RecordingDoc(t)
    doc.arrangement = new TwoUpBookletArrangement(Some(4)) // each sheet its own fold group

    val probes = (0 until 8).map(new Probe(_))
    probes.foreach(doc.add)
    doc.arrangement.flush(doc)

    // two independent 4-page signatures: (p3,p0)(p1,p2) then (p7,p4)(p5,p6)
    doc.sheets.length shouldBe 4
    doc.sheets.map(layoutOf) shouldBe Seq(
      Seq((Some(3), 0.0), (Some(0), 100.0)),
      Seq((Some(1), 0.0), (Some(2), 100.0)),
      Seq((Some(7), 0.0), (Some(4), 100.0)),
      Seq((Some(5), 0.0), (Some(6), 100.0)),
    )
  }

  "two-up-booklet doubles the sheet width" in {
    new TwoUpBookletArrangement(None).sheetSize(105, 148) shouldBe ((210.0, 148.0))
  }

  "four-up-booklet gangs two folded sheets, top upright, in two-up order" in {
    val t   = fixture(pw = 100, ph = 150)
    val doc = new RecordingDoc(t)
    doc.arrangement = new FourUpBookletArrangement(None)

    val probes = (0 until 8).map(new Probe(_))
    probes.foreach(doc.add)
    doc.arrangement.flush(doc)

    // one A4: the two-up sheets (8,1)(2,7) and (6,3)(4,5) stacked, outer sheet on top, inner below — no rotation
    doc.sheets.length shouldBe 2
    doc.sheets.head.placed.map { case (pg, dx, dy) => (idOf(pg), dx, dy) } shouldBe Seq(
      (Some(7), 0.0, 0.0),   (Some(0), 100.0, 0.0),   // front, top half: p8 | p1
      (Some(5), 0.0, 150.0), (Some(2), 100.0, 150.0), // front, bottom half: p6 | p3
    )
    doc.sheets(1).placed.map { case (pg, dx, dy) => (idOf(pg), dx, dy) } shouldBe Seq(
      (Some(1), 0.0, 0.0),   (Some(6), 100.0, 0.0),   // back, top half: p2 | p7
      (Some(3), 0.0, 150.0), (Some(4), 100.0, 150.0), // back, bottom half: p4 | p5
    )
  }

  "four-up-booklet pads a lone folded sheet with a blank lower half" in {
    val t   = fixture(pw = 100, ph = 150)
    val doc = new RecordingDoc(t)
    doc.arrangement = new FourUpBookletArrangement(None)

    val probes = (0 until 4).map(new Probe(_)) // one folded sheet; the lower half of the A4 is blank
    probes.foreach(doc.add)
    doc.arrangement.flush(doc)

    doc.sheets.length shouldBe 2
    doc.sheets.head.placed.map { case (pg, dx, dy) => (idOf(pg), dx, dy) } shouldBe Seq(
      (Some(3), 0.0, 0.0),   (Some(0), 100.0, 0.0),   // front, top half: p4 | p1
      (None, 0.0, 150.0),    (None, 100.0, 150.0),    // front, bottom half: blank
    )
    doc.sheets(1).placed.map { case (pg, dx, dy) => (idOf(pg), dx, dy) } shouldBe Seq(
      (Some(1), 0.0, 0.0),   (Some(2), 100.0, 0.0),   // back, top half: p2 | p3
      (None, 0.0, 150.0),    (None, 100.0, 150.0),    // back, bottom half: blank
    )
  }

  "four-up-booklet pads before the last page so covers stay on the outer sheet" in {
    val t   = fixture(pw = 100, ph = 150)
    val doc = new RecordingDoc(t)
    doc.arrangement = new FourUpBookletArrangement(None)

    val probes = (0 until 6).map(new Probe(_)) // pads to 8: the two blanks land before the last page, not after it
    probes.foreach(doc.add)
    doc.arrangement.flush(doc)

    // one A4, two folded sheets: the first page (p1) and the last page (p6) both ride the outer sheet's outside
    doc.sheets.length shouldBe 2
    doc.sheets.head.placed.map { case (pg, dx, dy) => (idOf(pg), dx, dy) } shouldBe Seq(
      (Some(5), 0.0, 0.0),   (Some(0), 100.0, 0.0),   // front, top half (outer sheet): p6 | p1
      (None, 0.0, 150.0),    (Some(2), 100.0, 150.0), // front, bottom half (inner sheet): blank | p3
    )
    doc.sheets(1).placed.map { case (pg, dx, dy) => (idOf(pg), dx, dy) } shouldBe Seq(
      (Some(1), 0.0, 0.0),   (None, 100.0, 0.0),      // back, top half (outer sheet): p2 | blank
      (Some(3), 0.0, 150.0), (Some(4), 100.0, 150.0), // back, bottom half (inner sheet): p4 | p5
    )
  }

  "four-up-booklet doubles the sheet both ways" in {
    new FourUpBookletArrangement(None).sheetSize(105, 148) shouldBe ((210.0, 296.0))
  }

  "n-up tiles pages in reading order across a grid" in {
    val t   = fixture(pw = 100, ph = 100)
    val doc = new RecordingDoc(t)
    doc.arrangement = new NupArrangement(rows = 2, cols = 2)

    val probes = (0 until 5).map(new Probe(_))
    probes.foreach(doc.add)
    doc.arrangement.flush(doc)

    doc.sheets.length shouldBe 2 // four on the first sheet, one on a partial second
    doc.sheets.head.placed.map { case (pg, dx, dy) => (idOf(pg), dx, dy) } shouldBe Seq(
      (Some(0), 0.0, 0.0),
      (Some(1), 100.0, 0.0),
      (Some(2), 0.0, 100.0),
      (Some(3), 100.0, 100.0),
    )
    doc.sheets(1).placed.map { case (pg, dx, dy) => (idOf(pg), dx, dy) } shouldBe Seq((Some(4), 0.0, 0.0))
  }

  "n-up sizes the sheet to the grid" in {
    new NupArrangement(rows = 2, cols = 3).sheetSize(100, 100) shouldBe ((300.0, 200.0))
  }
