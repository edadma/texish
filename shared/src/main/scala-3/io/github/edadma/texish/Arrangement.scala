package io.github.edadma.texish

import scala.collection.mutable.ArrayBuffer

/** A page arrangement is texish's output routine: it decides how finished logical pages are placed onto physical
  * sheets. The page builder hands each composed page — a page-sized [[SheetBox]] carrying the body and its running
  * header and footer — to the arrangement in document order; the arrangement lays pages onto sheets and ships each
  * completed sheet through [[DocumentMode.shipout]], the one primitive that turns a box into a printed sheet.
  *
  * This is the seam TeX exposes as `\shipout` and a redefinable `\output`: the default ships every page as its own
  * sheet, but an arrangement may buffer pages and re-emit them in another order and grouping — two-up on a wider
  * sheet, a tiled grid, or a saddle-stitch booklet whose page order only settles once the last page is known.
  *
  * An arrangement also declares the physical sheet size for a given logical page size, read once when the output
  * surface is created, so every sheet in a run is one size (a booklet's A5-landscape sheet carrying two A6 pages,
  * say). Streaming arrangements place pages as they arrive and leave `flush` empty; deferred ones buffer in `add`
  * and do all their placement in `flush`, which runs when the document ends.
  */
trait Arrangement:
  /** The physical sheet size for a logical page of `pw` by `ph`. */
  def sheetSize(pw: Double, ph: Double): (Double, Double)

  /** Place one composed logical page, in document order. */
  def add(page: Box, doc: DocumentMode): Unit

  /** Ship any pages still buffered; called once when the document ends. */
  def flush(doc: DocumentMode): Unit = ()

/** One logical page per sheet — the default. Pages stream straight to the output as they are built. */
object SimpleArrangement extends Arrangement:
  def sheetSize(pw: Double, ph: Double): (Double, Double) = (pw, ph)
  def add(page: Box, doc: DocumentMode): Unit             = doc.shipout(page)

/** A `cols` by `rows` grid of logical pages tiled onto one sheet, in reading order — left to right, top to bottom.
  * Pages fill a sheet and ship it; a final partial sheet ships with its empty cells left blank. This is plain
  * n-up imposition, the `\includepdf[nup=…]` of pdfpages, useful for handouts and proof sheets.
  */
class NupArrangement(rows: Int, cols: Int) extends Arrangement:
  private val perSheet = rows * cols
  private val buf      = ArrayBuffer[Box]()

  def sheetSize(pw: Double, ph: Double): (Double, Double) = (cols * pw, rows * ph)

  def add(page: Box, doc: DocumentMode): Unit =
    buf += page
    if buf.length == perSheet then emit(doc)

  override def flush(doc: DocumentMode): Unit = if buf.nonEmpty then emit(doc)

  private def emit(doc: DocumentMode): Unit =
    val (pw, ph) = doc.pageSize
    val placed = buf.zipWithIndex.map { case (pg, i) =>
      (pg, (i % cols) * pw, (i / cols) * ph)
    }
    doc.shipout(new SheetBox(cols * pw, rows * ph, placed.toSeq))
    buf.clear()

/** Shared machinery for the saddle-stitch booklet arrangements. Pages are buffered as they arrive and the order is
  * computed in `flush`, since a booklet's page order only settles once the last page is known. A run is padded with
  * blanks to a whole number of folded sheets (four pages per sheet) and split into signatures — folded groups nested
  * inside one another. With no explicit signature size the whole book is one signature (every sheet nested, one
  * central staple line); an explicit size folds the book into fixed groups, the way pdfpages' `signature` key does,
  * for a book too thick to fold as one.
  *
  * The common product is the ordered list of two-up sheet-sides the run folds into. Within a signature of `n` pages
  * (local indices `0 until n`), sheet `s` carries, on its front, page `n-1-2s` at the left and page `2s` at the
  * right; on its back, page `2s+1` at the left and page `n-2-2s` at the right. Front and back come out consecutively,
  * outermost sheet first, which is the order a duplex printer expects. The two-up arrangement ships each side as its
  * own sheet; the four-up arrangement gangs two sides' worth of sheets onto one taller sheet for cutting.
  */
abstract class SaddleBooklet(signature: Option[Int]) extends Arrangement:
  protected val buf = ArrayBuffer[Box]()

  def add(page: Box, doc: DocumentMode): Unit = buf += page

  protected def roundUp(x: Int, m: Int): Int = ((x + m - 1) / m) * m

  /** The ordered `(left, right)` page pairs for every sheet-side of the whole run — front then back of each folded
    * sheet, outermost sheet first, across all signatures — with `blank` padding a short final sheet. */
  protected def sheetSides(blank: Box): Seq[(Box, Box)] =
    val len     = buf.length
    val sigSize = signature.getOrElse(roundUp(len, 4))
    val padded  = buf.toArray ++ Array.fill(roundUp(len, sigSize) - len)(blank: Box)

    for
      group <- padded.grouped(sigSize).toSeq
      n = group.length
      s <- 0 until n / 4
      side <- Seq((group(n - 1 - 2 * s), group(2 * s)), (group(2 * s + 1), group(n - 2 - 2 * s)))
    yield side

/** Two-up saddle-stitch booklet imposition: logical pages are set at half width and printed two-up on a sheet
  * twice as wide, in the folding order that makes a stack of sheets each folded once down the middle and stapled
  * read in sequence. It is the two-up member of the booklet family — each sheet folded once, so A6 pages fall two
  * to an A5-landscape sheet. Folding each sheet a second time gives a four-up booklet on a sheet twice as tall as
  * well as wide (A6 pages onto A4), a separate arrangement.
  */
class TwoUpBookletArrangement(signature: Option[Int]) extends SaddleBooklet(signature):
  def sheetSize(pw: Double, ph: Double): (Double, Double) = (2 * pw, ph)

  override def flush(doc: DocumentMode): Unit =
    if buf.isEmpty then return

    val (pw, ph) = doc.pageSize
    val blank    = new SheetBox(pw, ph, Nil)

    for (left, right) <- sheetSides(blank) do
      doc.shipout(new SheetBox(2 * pw, ph, Seq((left, 0.0, 0.0), (right, pw, 0.0))))
    buf.clear()

/** Four-up saddle-stitch booklet imposition for cutting: the two-up sheet-sides are ganged two folded sheets onto a
  * sheet twice as tall as well as wide (A6 pages onto A4). Print double-sided, cut the sheet in half across the
  * middle into two half-height sheets, then fold and nest those exactly as the two-up booklet. No page prints
  * upside-down and nothing is slit — the layout is the two-up order stacked two sheets high, so once cut and
  * collated outermost sheet first the pages read in sequence. A run is padded to a whole number of taller sheets.
  *
  * Registration assumes long-edge duplex: the back is laid out in the same upright top/bottom order as the front, so
  * the printer's book-style flip about the long edge backs the upper and lower halves correctly. Each taller sheet
  * ships its front then its back consecutively. The upper half of a sheet carries the outer of its two folded
  * sheets and the lower half the inner, so cutting yields the sheets already in nesting order top to bottom.
  */
class FourUpBookletArrangement(signature: Option[Int]) extends SaddleBooklet(signature):
  def sheetSize(pw: Double, ph: Double): (Double, Double) = (2 * pw, 2 * ph)

  override def flush(doc: DocumentMode): Unit =
    if buf.isEmpty then return

    val (pw, ph) = doc.pageSize
    val blank    = new SheetBox(pw, ph, Nil)

    // group the two-up sides into folded sheets (front then back), then gang two folded sheets per taller sheet
    val folded     = sheetSides(blank).grouped(2).toSeq                   // each: Seq(front, back) of one folded sheet
    val blankSheet = Seq((blank: Box, blank: Box), (blank: Box, blank: Box))
    val sheets     = if folded.length % 2 == 1 then folded :+ blankSheet else folded

    def a4(top: (Box, Box), bot: (Box, Box)): SheetBox =
      new SheetBox(
        2 * pw,
        2 * ph,
        Seq((top._1, 0.0, 0.0), (top._2, pw, 0.0), (bot._1, 0.0, ph), (bot._2, pw, ph)),
      )

    for pair <- sheets.grouped(2) do
      val upper = pair(0) // the outer folded sheet — upper half of the taller sheet
      val lower = pair(1) // the inner folded sheet — lower half
      doc.shipout(a4(upper(0), lower(0))) // front: both fronts
      doc.shipout(a4(upper(1), lower(1))) // back: both backs
    buf.clear()
