package io.github.edadma.texish

import io.github.edadma.texish.parser.Value

import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

class DocumentMode(val t: Typesetter) extends Mode:
  val printedPages   = new ArrayBuffer[Any]
  var page: Int      = 0
  var eject: Boolean = false

  def init(): Unit = ()

  def layout(b: Box): Box = b

  infix def add(box: Box): Unit =
    // pageno is a logical folio, not the physical sheet index: it starts at 1 and is advanced by one after each
    // page ships, so shipout-time material (running headers and footers) reads the shipping page's number here,
    // before the advance. Because it is never reassigned from `page`, a document may renumber pages the way plain
    // TeX lets you assign \pageno — e.g. lowercase-roman front matter, then \set pageno {1} to restart the body.
    // The advance is global (like TeX's \global\advance\count0): a page can ship while an environment group (a
    // list, a quote) is open, and a plain `set` would write only that group's scope and be rolled back when it
    // closes — leaving the next page to ship with a stale folio.
    t.get("layout") match
      case Some(Value.Text("zfold")) => handleZFoldLayout(box)
      case _                         => handleSimpleLayout(box)

    page += 1
    t.setGlobal("pageno", t.getNumber("pageno").toInt + 1)

  // panels share a physical sheet, so running headers/footers (pageDecorator) don't apply to this layout
  def handleZFoldLayout(b: Box): Unit =
    val hfolds = 3
    val vfolds = 2

    val folds = vfolds * hfolds

    val fold   = page % folds
    val hfold  = page % hfolds
    val vfold  = page / hfolds
    val width  = t.getNumber("paperwidth") / hfolds
    val height = t.getNumber("paperheight") / vfolds

    if fold == 0 then
      printedPages += t.createPageTarget
      eject = true

    t.draw(
      layout(b),
      hfold * width + t.getNumber("hoffset"),
      vfold * height + t.getNumber("voffset"),
    )

    if fold == folds - 1 then
      t.ejectPageTarget()
      eject = false

  def handleSimpleLayout(b: Box): Unit =
    printedPages += t.createPageTarget

    val hoffset = t.getNumber("hoffset")
    val voffset = t.getNumber("voffset")
    val dec     = t.pageDecorator

    if dec ne null then
      val (header, footer) = dec()

      if header ne null then t.draw(header, hoffset, voffset - t.getNumber("headsep") - header.height)
      // anchored to vsize, not the body box height, so the footer sits at the same place on a short final page
      if footer ne null then t.draw(footer, hoffset, voffset + t.getNumber("vsize") + t.getNumber("footskip"))

    t.draw(
      layout(b),
      hoffset,
      voffset,
    )
    t.ejectPageTarget()

  override def done(): Unit =
    pop

    if eject then
      t.ejectPageTarget()

  def result: Box = ???
