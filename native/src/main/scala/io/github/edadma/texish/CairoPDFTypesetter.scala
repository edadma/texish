package io.github.edadma.texish

import io.github.edadma.libcairo.pdfSurfaceCreate

/** Renders the whole document onto a single Cairo PDF surface, one page per `showPage`, streaming to the file at
  * `output`. All drawing is inherited from [[CairoTypesetter]].
  */
class CairoPDFTypesetter(val output: String) extends CairoTypesetter:

  def init(width: Double, height: Double): Unit =
    surface = pdfSurfaceCreate(output, width, height)
    ctx = surface.create

  def createPageTarget: Any = ensureInitializedForContent()

  def ejectPageTarget(): Unit = ctx.showPage()

  def destroy(): Unit =
    ctx.destroy()
    surface.destroy()
