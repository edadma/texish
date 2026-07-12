package io.github.edadma.texish

import io.github.edadma.libcairo.{PdfVersion, Tags, pdfSurfaceCreate}

/** Renders the whole document onto a single Cairo PDF surface, one page per `showPage`, streaming to the file at
  * `output`. All drawing is inherited from [[CairoTypesetter]].
  */
class CairoPDFTypesetter(val output: String) extends CairoTypesetter:

  def init(width: Double, height: Double): Unit =
    surface = pdfSurfaceCreate(output, width, height)
    // Restrict to PDF 1.4 so Cairo writes a classic cross-reference table and plaintext objects rather than
    // the 1.5+ cross-reference and object streams. That keeps the file simple enough to append an sRGB
    // OutputIntent to after Cairo finishes (see SrgbOutputIntent), and 1.4 is the friendlier target for print
    // workflows anyway. Must be set before the surface is drawn on.
    surface.restrictToVersion(PdfVersion.V1_4)
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
    // Cairo has now flushed the finished PDF to `output`. Tag it as sRGB so viewers colour-manage its DeviceRGB
    // content (the embedded images) as sRGB rather than shifting it.
    SrgbOutputIntent.inject(output)
