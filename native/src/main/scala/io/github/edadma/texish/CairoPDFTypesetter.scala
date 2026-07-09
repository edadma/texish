package io.github.edadma.texish

import io.github.edadma.libcairo.{Tags, pdfSurfaceCreate}

/** Renders the whole document onto a single Cairo PDF surface, one page per `showPage`, streaming to the file at
  * `output`. All drawing is inherited from [[CairoTypesetter]].
  */
class CairoPDFTypesetter(val output: String) extends CairoTypesetter:

  def init(width: Double, height: Double): Unit =
    surface = pdfSurfaceCreate(output, width, height)
    ctx = surface.create
    applyFontOptions()

  def createPageTarget: Any = ensureInitializedForContent()

  def ejectPageTarget(): Unit = ctx.showPage()

  // The PDF surface turns a tagged region into a real link annotation: the link's hotspot is the bounding box
  // of whatever is drawn between begin and end. A single-quoted uri attribute escapes any apostrophe in the URL.
  override def beginLink(uri: String): Unit = ctx.tagBegin(Tags.LINK, s"uri='${uri.replace("'", "%27")}'")
  override def endLink(): Unit              = ctx.tagEnd(Tags.LINK)

  def destroy(): Unit =
    destroyImages()
    ctx.destroy()
    surface.destroy()
