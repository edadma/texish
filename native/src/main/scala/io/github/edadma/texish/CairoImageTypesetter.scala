package io.github.edadma.texish

import io.github.edadma.libcairo.{Format, imageSurfaceCreate}

/** Renders each page onto its own Cairo ARGB32 image surface, in memory, at the given resolution. The shipped
  * surfaces are collected in the document's `printedPages` — exactly as the JVM Graphics2D backend collects a
  * `BufferedImage` per page — so a host can display them in a GUI or save them. Once a page ships, its surface
  * belongs to the caller, who is responsible for destroying it; the backend never frees a surface it has handed
  * out.
  *
  * The engine works in big points; `dpi` sets the device scale (`dpi / 72`) applied to each page context so that
  * point coordinates map to device pixels. The measurement context created at construction is left unscaled:
  * text extents are reported in user space regardless of the transform, and — because the subclass `deviceScale`
  * field is not yet initialized while the superclass constructor calls `init` — scaling it there would apply a
  * zero transform and leave the context permanently in Cairo's error state.
  */
class CairoImageTypesetter(dpi: Double = 100) extends CairoTypesetter:

  val output: String = null

  private val deviceScale = dpi / 72

  private var pageWidth: Double    = 0
  private var pageHeight: Double   = 0
  private var ready: Boolean       = false
  private var pageStarted: Boolean = false

  private def pixels(points: Double): Int = math.max(1, math.round(points * deviceScale).toInt)

  def init(width: Double, height: Double): Unit =
    pageWidth = width
    pageHeight = height

    // a 1x1 surface backs the unscaled context used to measure text before the first page exists (see the class
    // note: scaling here would fault, and measurement does not need the transform)
    if !ready then
      surface = imageSurfaceCreate(Format.ARGB32, 1, 1)
      ctx = surface.create
      ready = true

  def createPageTarget: Any =
    ensureInitializedForContent()

    // the previous context's drawing is done. A shipped page's surface stays alive for the caller; only the
    // measurement scratch, which nothing collected, is freed along with its context.
    ctx.destroy()

    if !pageStarted then surface.destroy()

    surface = imageSurfaceCreate(Format.ARGB32, pixels(pageWidth), pixels(pageHeight))
    ctx = surface.create

    // map the engine's point coordinates onto device pixels for this page
    ctx.scale(deviceScale, deviceScale)

    // image surfaces start fully transparent; lay down an opaque white page to draw onto
    ctx.setSourceRGBA(1, 1, 1, 1)
    ctx.paint()
    pageStarted = true
    surface

  def ejectPageTarget(): Unit = surface.flush()

  def destroy(): Unit =
    if ready then ctx.destroy()
    // collected page surfaces belong to the caller; only an uncollected scratch is the backend's to free
    if ready && !pageStarted then surface.destroy()
