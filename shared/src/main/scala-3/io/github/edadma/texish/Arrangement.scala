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

/** One-fold saddle-stitch booklet imposition: logical pages are set at half width and printed two-up on a sheet
  * twice as wide, in the folding order that makes a stack of sheets each folded once down the middle and stapled
  * read in sequence. It is the single-fold member of the booklet family — A6 pages fall two to an A5-landscape
  * sheet. Folding each sheet a second time, four-up on a sheet twice as tall as well as wide (A6 pages onto A4),
  * is a separate arrangement.
  *
  * The order only settles once every page is known, so pages are buffered and imposed in `flush`. A run of pages
  * is padded with blanks to a whole number of sheets (four pages per sheet) and split into signatures — folded
  * groups nested inside one another. With no explicit signature size the whole book is one signature (every sheet
  * nested, one central staple line); an explicit size folds the book into fixed groups, the way pdfpages'
  * `signature` key does, for a book too thick to fold as one.
  *
  * Within a signature of `n` pages (local indices `0 until n`), sheet `s` carries, on its front, page `n-1-2s` at
  * the left and page `2s` at the right; on its back, page `2s+1` at the left and page `n-2-2s` at the right. Front
  * and back ship consecutively, outermost sheet first, which is the order a duplex printer expects.
  */
class OneFoldArrangement(signature: Option[Int]) extends Arrangement:
  private val buf = ArrayBuffer[Box]()

  def sheetSize(pw: Double, ph: Double): (Double, Double) = (2 * pw, ph)

  def add(page: Box, doc: DocumentMode): Unit = buf += page

  override def flush(doc: DocumentMode): Unit =
    if buf.isEmpty then return

    val (pw, ph) = doc.pageSize
    val blank    = new SheetBox(pw, ph, Nil)

    // one signature is a whole number of four-page sheets; the whole book is one signature unless a size is given
    val sigSize = signature.getOrElse(roundUp(buf.length, 4))
    val padded  = buf.toArray ++ Array.fill(roundUp(buf.length, sigSize) - buf.length)(blank: Box)

    for group <- padded.grouped(sigSize) do imposeSignature(group, pw, ph, doc)
    buf.clear()

  private def imposeSignature(pages: Array[Box], pw: Double, ph: Double, doc: DocumentMode): Unit =
    val n = pages.length
    def sheet(left: Box, right: Box): Unit =
      doc.shipout(new SheetBox(2 * pw, ph, Seq((left, 0.0, 0.0), (right, pw, 0.0))))

    for s <- 0 until n / 4 do
      sheet(pages(n - 1 - 2 * s), pages(2 * s))     // front
      sheet(pages(2 * s + 1), pages(n - 2 - 2 * s)) // back

  private def roundUp(x: Int, m: Int): Int = ((x + m - 1) / m) * m
