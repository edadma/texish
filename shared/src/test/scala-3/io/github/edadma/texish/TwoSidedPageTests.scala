package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Two-sided page composition: with `twoside` set, the text block is reflected about the page's vertical centre on
  * verso pages, so the margin nearest the binding stays against the spine on both sides of a sheet. The reflection
  * is of the resolved frame, so it holds however the geometry was specified, and it carries the running header and
  * footer with the body — a head pushed to the outer edge stays outer. Parity is read from `pageno`, the folio the
  * page prints, so a renumbered front matter keeps margins and folios agreeing.
  */
class TwoSidedPageTests extends AnyFreeSpec with Matchers:

  /** A stand-in for page material that records nothing but can be told apart by identity. */
  private class Probe extends ContentBox:
    val width: Double                                   = 0
    val xAdvance: Double                                = 0
    val ascent: Double                                  = 0
    val descent: Double                                 = 0
    def draw(t: Typesetter, x: Double, y: Double): Unit = ()

  private class RecordingDoc(t0: Typesetter) extends DocumentMode(t0):
    val sheets = ArrayBuffer[SheetBox]()
    override def shipout(sheet: Box): Unit = sheet match
      case s: SheetBox => sheets += s
      case _           =>

  /** A 200-wide page whose 100-wide text block sits 30 from the left — so the recto's margins are 30 inner and 70
    * outer, wide enough apart that a mirror cannot be mistaken for a rounding difference.
    */
  private def fixture(twoside: Boolean, pageno: Int): (HeadlessTypesetter, RecordingDoc) =
    val t = new HeadlessTypesetter
    t.set("paperwidth", 200.0)
    t.set("paperheight", 300.0)
    t.set("hoffset", 30.0)
    t.set("hsize", 100.0)
    t.set("voffset", 20.0)
    t.set("vsize", 200.0)
    t.set("pageno", pageno.toDouble)
    t.set("twoside", if twoside then 1.0 else 0.0)
    (t, new RecordingDoc(t))

  /** The x offsets of everything placed on the one sheet the document shipped. */
  private def offsets(doc: RecordingDoc): Seq[Double] = doc.sheets.head.placed.map(_._2)

  "one-sided, every page places its body at hoffset" in {
    for folio <- Seq(1, 2, 3, 4) do
      val (_, doc) = fixture(twoside = false, pageno = folio)
      doc add new Probe
      offsets(doc) shouldBe Seq(30.0)
  }

  "two-sided, a recto keeps the frame and a verso reflects it" in {
    val (_, recto) = fixture(twoside = true, pageno = 1)
    recto add new Probe
    offsets(recto) shouldBe Seq(30.0) // odd folio: the frame as resolved

    val (_, verso) = fixture(twoside = true, pageno = 2)
    verso add new Probe
    offsets(verso) shouldBe Seq(70.0) // even folio: 200 - 30 - 100, the recto's outer margin
  }

  "the reflection follows the folio, not the count of pages composed" in {
    // A front matter renumbered to restart at 1 makes the third page composed an odd folio again, and it must be
    // laid out as a recto — the margins agree with the number the page prints, as \cleardoublepage's test does.
    val (t, doc) = fixture(twoside = true, pageno = 1)

    doc add new Probe // folio 1 — recto
    doc add new Probe // folio 2 — verso
    t.setGlobal("pageno", 1.0)
    doc add new Probe // renumbered back to folio 1 — recto again

    doc.sheets.map(_.placed.head._2) shouldBe Seq(30.0, 70.0, 30.0)
  }

  "the running header and footer mirror with the body" in {
    val (t, doc) = fixture(twoside = true, pageno = 2)
    val header   = new Probe
    val footer   = new Probe

    t.pageDecorator = () => (header, footer)
    doc add new Probe

    // body, header and footer all sit at the mirrored offset: a head set flush to one edge stays on that side of
    // the page relative to the text block, which is what makes an outer-aligned folio come out outer on both sides.
    offsets(doc) shouldBe Seq(70.0, 70.0, 70.0)
  }

  "the vertical frame is untouched by the mirror" in {
    val (_, doc) = fixture(twoside = true, pageno = 2)
    doc add new Probe

    doc.sheets.head.placed.map(_._3) shouldBe Seq(20.0) // voffset, unchanged: only the horizontal axis reflects
  }
