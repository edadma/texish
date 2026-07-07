package io.github.edadma.texish

import scala.collection.mutable.ArrayBuffer

/** The bottom of the mode stack: it receives each finished logical page from [[PageMode]] and drives it to the
  * output. A page is first composed into a page-sized [[SheetBox]] — its body placed at the text origin with the
  * running header and footer around it — then handed to the current [[Arrangement]], which decides how pages fall
  * onto physical sheets and ships each sheet through [[shipout]]. With the default [[SimpleArrangement]] that is one
  * page per sheet; a booklet or n-up arrangement groups several.
  */
class DocumentMode(val t: Typesetter) extends Mode:
  val printedPages = new ArrayBuffer[Any]

  /** The output routine: how composed pages are placed onto sheets. Set once in the preamble (before content) by
    * `\arrange`; defaults to one page per sheet.
    */
  var arrangement: Arrangement = SimpleArrangement

  /** Count of logical pages composed so far — a running total distinct from `pageno`, which is a renumberable
    * folio. Advanced as each page is handed to the arrangement.
    */
  var page: Int = 0

  def init(): Unit = ()

  /** The logical page size — the paper a single page is set on, which an arrangement tiles onto larger sheets. */
  def pageSize: (Double, Double) = (t.getNumber("paperwidth"), t.getNumber("paperheight"))

  infix def add(box: Box): Unit =
    // The header and footer are resolved here, while `pageno` still holds the shipping page's folio, so running
    // material reads the right number even when the arrangement ships the composed page much later (a booklet
    // buffers every page until the last is known). The composed page carries them with it wherever it lands.
    arrangement.add(composePage(box), this)
    page += 1

    // pageno is a logical folio, not the physical sheet index: it starts at 1 and advances by one after each page
    // is composed, so the next page reads the next folio. Because it is never reassigned from a physical counter, a
    // document may renumber pages the way plain TeX assigns \pageno — lowercase-roman front matter, then \set pageno
    // {1} to restart the body. The advance is global (like \global\advance\count0): a page can compose while an
    // environment group is open, and a plain `set` would write only that group's scope and be rolled back when it
    // closes, leaving the next page with a stale folio.
    t.setGlobal("pageno", t.getNumber("pageno").toInt + 1)

  /** Assemble one logical page into a single page-sized box: the body at the text origin, and — when a page
    * decorator is installed — the running header above the text block and the footer below it. Positions match what
    * the page builder computes from `hoffset`/`voffset`/`headsep`/`vsize`/`footskip`, so a composed simple page ships
    * to exactly the coordinates a directly drawn one would.
    */
  private def composePage(body: Box): SheetBox =
    val pw      = t.getNumber("paperwidth")
    val ph      = t.getNumber("paperheight")
    val hoffset = t.getNumber("hoffset")
    val voffset = t.getNumber("voffset")
    val placed  = ArrayBuffer[(Box, Double, Double)]((body, hoffset, voffset))

    val dec = t.pageDecorator
    if dec ne null then
      val (header, footer) = dec()

      if header ne null then placed += ((header, hoffset, voffset - t.getNumber("headsep") - header.height))
      // anchored to vsize, not the body box height, so the footer sits at the same place on a short final page
      if footer ne null then placed += ((footer, hoffset, voffset + t.getNumber("vsize") + t.getNumber("footskip")))

    new SheetBox(pw, ph, placed.toSeq)

  /** Ship one box as one physical sheet: open a page target, paint the box at its origin, and close the target,
    * collecting it into [[printedPages]]. This is texish's `\shipout` — the sole primitive that commits ink to a
    * sheet. Every arrangement funnels through here, so imposition lives entirely above it in box space and needs no
    * cooperation from the rendering backend.
    */
  def shipout(sheet: Box): Unit =
    printedPages += t.createPageTarget
    t.draw(sheet, 0, 0)
    t.ejectPageTarget()

  override def done(): Unit =
    pop
    arrangement.flush(this)

  def result: Box = ???
