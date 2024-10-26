package io.github.edadma.typesetter

import io.github.edadma.freetype.{initFreeType, Library}
import io.github.edadma.libcairo.{
  Context,
  FontFace,
  FontSlant,
  FontWeight,
  Format,
  Surface,
  TextExtents,
  fontFaceCreateForFTFace,
  imageSurfaceCreate,
  pdfSurfaceCreate,
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

  def setFont(font: Any): Unit = ctx.setFontFace(font.asInstanceOf[FontFace])

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
    val layout = new TextLayout(text, font.asInstanceOf[JFont], frc)
    val bounds = layout.getBounds

    val ascent = -bounds.getY
//    val descent = layout.getDescent
    val width   = bounds.getWidth
    val height  = bounds.getHeight
    val advance = layout.getAdvance

    TextExtents(
      xBearing = bounds.getX,
      yBearing = -ascent, // In Graphics2D, the ascent is negative yBearing (above the baseline)
      width = width,
      height = height,
      xAdvance = advance,
      yAdvance = 0, // In horizontal typesetting, yAdvance is 0
    )

  def makeFont(font: Any, size: Double): Any = font.asInstanceOf[JFont].deriveFont(size.toFloat)

  def charWidth(font: Any, c: Char): Double =
    setFont(font)
    g.getFontMetrics.charWidth(c)

  def loadImage(path: String): (Any, Int, Int) =
    val image = ImageIO.read(new File(path))

    (image, image.getWidth, image.getHeight)

  def drawImage(image: Any, x: Double, y: Double): Unit =
    g.drawImage(image.asInstanceOf[BufferedImage], x.toInt, y.toInt, null)
