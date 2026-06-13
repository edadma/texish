package io.github.edadma.typesetter

import io.github.edadma.freetype.{Library, initFreeType}
import io.github.edadma.libcairo.{
  Context,
  FontFace as CairoFontFace,
  Surface,
  fontFaceCreateForFTFace,
  imageSurfaceCreateFromPNG,
  TextExtents as CairoTextExtents,
}

import scala.compiletime.uninitialized

/** A Cairo font handle, applied to a context as a face + size rather than via `cairo_set_scaled_font`: a
  * `cairo_scaled_font_t` binds the transform of the context it was created on and faults when set on a different
  * page's context, whereas a face + size has no such binding and draws cleanly on every page.
  */
case class CairoRenderFont(face: CairoFontFace, size: Double)

/** The drawing shared by the native Cairo backends. Everything that operates on a Cairo context — text, lines,
  * fills, fonts, images — lives here and is identical whatever the page is drawn onto. Subclasses decide only
  * what surface backs the context and how pages are delimited: [[CairoPDFTypesetter]] draws every page onto one
  * PDF surface and streams it to a file; [[CairoImageTypesetter]] gives each page its own in-memory image
  * surface so a host can display or save it.
  */
abstract class CairoTypesetter extends Typesetter:

  type ImageHandle = Surface
  type FontFace    = CairoFontFace
  type RenderFont  = CairoRenderFont

  protected var surface: Surface     = uninitialized
  protected var ctx: Context         = uninitialized
  protected lazy val freetype: Library = initFreeType.getOrElse(sys.error("error initializing FreeType"))

  def setFont(font: RenderFont): Unit =
    ctx.setFontFace(font.face)
    ctx.setFontSize(font.size)

  def setColor(color: Color): Unit = ctx.setSourceRGBA(color.red, color.green, color.blue, color.alpha)

  def setLineWidth(width: Double): Unit = ctx.setLineWidth(width)

  def drawString(text: String, x: Double, y: Double): Unit =
    ctx.moveTo(x, y)
    ctx.showText(text)

  def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit =
    ctx.moveTo(x1, y1)
    ctx.lineTo(x2, y2)
    ctx.stroke()

  def drawRect(x: Double, y: Double, width: Double, height: Double): Unit = ()

  def fillRect(x: Double, y: Double, width: Double, height: Double): Unit =
    ctx.rectangle(x, y, width, height)
    ctx.fill()

  def loadFont(path: String): FontFace =
    fontFaceCreateForFTFace(
      freetype
        .newFace(path, 0)
        .getOrElse(sys.error(s"error loading face: $path"))
        .faceptr,
      0,
    )

  def getTextExtents(text: String, font: RenderFont): TextExtents =
    setFont(font)

    val CairoTextExtents(a, b, c, d, e, f) = ctx.textExtents(text)

    TextExtents(a, b, c, d, e, f)

  def makeFont(font: FontFace, size: Double): RenderFont = CairoRenderFont(font, size)

  def charWidth(font: RenderFont, c: Char): Double =
    setFont(font)
    ctx.textExtents(c.toString).xAdvance

  def loadImage(path: String): (ImageHandle, Int, Int) =
    val loaded = imageSurfaceCreateFromPNG(path).reference

    (loaded, loaded.getWidth, loaded.getHeight)

  def drawImage(image: ImageHandle, x: Double, y: Double): Unit =
    ctx.save()
    ctx.setSourceSurface(image, x, y)
    ctx.paint()
    ctx.restore()
