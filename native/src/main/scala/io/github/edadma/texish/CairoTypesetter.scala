package io.github.edadma.texish

import io.github.edadma.freetype.{Face as FTFace, Library, initFreeType}
import io.github.edadma.libcairo.{
  Context,
  FontFace as CairoFontFace,
  Format,
  Glyph,
  LineCap as CairoLineCap,
  LineJoin as CairoLineJoin,
  Surface,
  fontFaceCreateForFTFace,
  imageSurfaceCreate,
  imageSurfaceCreateFromPNG,
  TextExtents as CairoTextExtents,
}
import io.github.edadma.turbojpeg

import java.io.{File, FileInputStream}

import scala.compiletime.uninitialized

/** A loaded typeface: the Cairo font face used to draw and measure, paired with the FreeType face it was
  * built from. The FreeType face is retained because the glyph seam needs it — `getCharIndex` maps a
  * codepoint to a glyph index, and (since the Cairo face was created from this FreeType face) that index
  * is exactly what `cairo_show_glyphs` / `cairo_glyph_extents` consume.
  */
case class CairoFace(cairo: CairoFontFace, ft: FTFace)

/** A Cairo font handle, applied to a context as a face + size rather than via `cairo_set_scaled_font`: a
  * `cairo_scaled_font_t` binds the transform of the context it was created on and faults when set on a different
  * page's context, whereas a face + size has no such binding and draws cleanly on every page. The FreeType
  * face rides along for the glyph seam (see [[CairoFace]]).
  */
case class CairoRenderFont(face: CairoFontFace, size: Double, ft: FTFace)

/** The drawing shared by the native Cairo backends. Everything that operates on a Cairo context — text, lines,
  * fills, fonts, images — lives here and is identical whatever the page is drawn onto. Subclasses decide only
  * what surface backs the context and how pages are delimited: [[CairoPDFTypesetter]] draws every page onto one
  * PDF surface and streams it to a file; [[CairoImageTypesetter]] gives each page its own in-memory image
  * surface so a host can display or save it.
  */
abstract class CairoTypesetter extends Typesetter:

  type ImageHandle = Surface
  type FontFace    = CairoFace
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
    val ft = freetype.newFace(path, 0).getOrElse(sys.error(s"error loading face: $path"))

    CairoFace(fontFaceCreateForFTFace(ft.faceptr, 0), ft)

  def getTextExtents(text: String, font: RenderFont): TextExtents =
    setFont(font)

    val CairoTextExtents(a, b, c, d, e, f) = ctx.textExtents(text)

    TextExtents(a, b, c, d, e, f)

  def makeFont(font: FontFace, size: Double): RenderFont = CairoRenderFont(font.cairo, size, font.ft)

  def charWidth(font: RenderFont, c: Char): Double =
    setFont(font)
    ctx.textExtents(c.toString).xAdvance

  def glyphIndex(font: RenderFont, codepoint: Int): Int = font.ft.getCharIndex(codepoint.toLong)

  def glyphExtents(font: RenderFont, glyph: Int): TextExtents =
    setFont(font)

    val CairoTextExtents(a, b, c, d, e, f) = ctx.glyphExtents(Seq(Glyph(glyph.toLong, 0, 0)))

    TextExtents(a, b, c, d, e, f)

  def drawGlyph(font: RenderFont, glyph: Int, x: Double, y: Double): Unit =
    setFont(font)
    ctx.showGlyphs(Seq(Glyph(glyph.toLong, x, y)))

  def sfntTable(font: RenderFont, tag: String): Option[Array[Byte]] = font.ft.loadSfntTable(tag)

  def loadImage(path: String): (ImageHandle, Int, Int) =
    val loaded =
      if path.toLowerCase.endsWith(".jpg") || path.toLowerCase.endsWith(".jpeg") then loadJpeg(path)
      else imageSurfaceCreateFromPNG(path).reference

    (loaded, loaded.getWidth, loaded.getHeight)

  // Cairo reads PNG itself but knows nothing about JPEG, so decode the JPEG with TurboJPEG straight into a
  // freshly created ARGB32 surface's own pixel buffer (zero-copy). TurboJPEG's BGRA output sets every alpha
  // byte opaque, and on a little-endian host that byte order is exactly Cairo's premultiplied ARGB32 layout.
  private def loadJpeg(path: String): Surface =
    val file  = new File(path)
    val bytes = new Array[Byte](file.length().toInt)
    val in    = new FileInputStream(file)
    try in.read(bytes)
    finally in.close()

    val header  = turbojpeg.info(bytes)
    val surface = imageSurfaceCreate(Format.ARGB32, header.width, header.height)
    surface.flush()
    val decoder = turbojpeg.Decoder()
    try decoder.decompress(bytes, surface.getData, surface.getStride, turbojpeg.PixelFormat.BGRA)
    finally decoder.close()
    surface.markDirty()
    surface.reference

  def drawImage(image: ImageHandle, x: Double, y: Double, w: Double, h: Double): Unit =
    ctx.save()
    ctx.translate(x, y)
    if image.getWidth > 0 && image.getHeight > 0 then ctx.scale(w / image.getWidth, h / image.getHeight)
    ctx.setSourceSurface(image, 0, 0)
    ctx.paint()
    ctx.restore()

  // Picture-graphics seam — thin pass-throughs onto the Cairo context, which models exactly this vocabulary.

  override def gsave(): Unit    = ctx.save()
  override def grestore(): Unit = ctx.restore()

  override def translate(dx: Double, dy: Double): Unit = ctx.translate(dx, dy)
  override def scale(sx: Double, sy: Double): Unit     = ctx.scale(sx, sy)
  override def rotate(radians: Double): Unit           = ctx.rotate(radians)

  override def newPath(): Unit           = ctx.newPath()
  override def moveTo(x: Double, y: Double): Unit = ctx.moveTo(x, y)
  override def lineTo(x: Double, y: Double): Unit = ctx.lineTo(x, y)

  override def curveTo(c1x: Double, c1y: Double, c2x: Double, c2y: Double, x: Double, y: Double): Unit =
    ctx.curveTo(c1x, c1y, c2x, c2y, x, y)

  override def arc(cx: Double, cy: Double, r: Double, a0: Double, a1: Double, negative: Boolean): Unit =
    if negative then ctx.arcNegative(cx, cy, r, a0, a1) else ctx.arc(cx, cy, r, a0, a1)

  override def closePath(): Unit = ctx.closePath()

  override def fillPath(preserve: Boolean): Unit = if preserve then ctx.fillPreserve() else ctx.fill()
  override def strokePath(): Unit                = ctx.stroke()
  override def clipPath(): Unit                  = ctx.clip()

  override def setDash(pattern: Seq[Double], offset: Double): Unit = ctx.setDash(pattern, offset)

  override def setLineCap(cap: LineCap): Unit = ctx.setLineCap(cap match
    case LineCap.Butt   => CairoLineCap.BUTT
    case LineCap.Round  => CairoLineCap.ROUND
    case LineCap.Square => CairoLineCap.SQUARE,
  )

  override def setLineJoin(join: LineJoin): Unit = ctx.setLineJoin(join match
    case LineJoin.Miter => CairoLineJoin.MITER
    case LineJoin.Round => CairoLineJoin.ROUND
    case LineJoin.Bevel => CairoLineJoin.BEVEL,
  )
