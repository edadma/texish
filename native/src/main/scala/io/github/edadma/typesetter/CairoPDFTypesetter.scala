package io.github.edadma.typesetter

import io.github.edadma.freetype.{Library, initFreeType}
import io.github.edadma.libcairo.{
  Context,
  FontFace,
  FontSlant,
  FontWeight,
  Format,
  ScaledFont,
  Surface,
  fontFaceCreateForFTFace,
  imageSurfaceCreate,
  pdfSurfaceCreate,
  TextExtents as CairoTextExtents,
}

import scala.compiletime.uninitialized

class CairoPDFTypesetter extends Typesetter:

  private var surface: Surface  = uninitialized
  private var ctx: Context      = uninitialized
  private var freetype: Library = uninitialized

  def initTarget(): Unit =
    freetype = initFreeType.getOrElse(sys.error("error initializing FreeType"))

  def createPageTarget(path: String, width: Double, height: Double): Any =
    if surface eq null then
      surface = pdfSurfaceCreate(path, width, height)
      ctx = surface.create

  def ejectPageTarget(): Unit = ctx.showPage()

  def getDPI: Double = 72

  def setFont(font: Any): Unit = ctx.setScaledFont(font.asInstanceOf[ScaledFont])

  def setColor(color: Color): Unit = ctx.setSourceRGBA(color.red, color.green, color.blue, color.alpha)

  def drawString(text: String, x: Double, y: Double): Unit =
    ctx.moveTo(x, y)
    ctx.showText(text)

  def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = ()

  def drawRect(x: Double, y: Double, width: Double, height: Double): Unit = ()

  def fillRect(x: Double, y: Double, width: Double, height: Double): Unit = ()

  def loadFont(path: String): FontFace =
    fontFaceCreateForFTFace(
      freetype
        .newFace(path, 0)
        .getOrElse(sys.error(s"error loading face: $path"))
        .faceptr,
      0,
    )

  def getTextExtents(text: String, font: Any): TextExtents =
    ctx.setScaledFont(font.asInstanceOf[ScaledFont])

    val CairoTextExtents(a, b, c, d, e, f) = ctx.textExtents(text)

    TextExtents(a, b, c, d, e, f)

  def makeFont(font: Any, size: Double): Any =
    ctx.setFontFace(font.asInstanceOf[FontFace])
    ctx.setFontSize(size)
    ctx.getScaledFont

  def charWidth(font: Any, c: Char): Double =
    setFont(font)
    ctx.textExtents(c.toString).width

  def loadImage(path: String): (Any, Int, Int) = (null, 0, 0)

  def drawImage(image: Any, x: Double, y: Double): Unit = ()
//    g.drawImage(image.asInstanceOf[BufferedImage], x.toInt, y.toInt, null)

  def destroy(): Unit =
    ctx.destroy()
    surface.destroy()
