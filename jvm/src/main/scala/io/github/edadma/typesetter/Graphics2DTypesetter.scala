package io.github.edadma.typesetter

import java.awt.font.{FontRenderContext, TextLayout}
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Graphics2D, RenderingHints, Font as JFont}
import java.io.File
import javax.imageio.ImageIO
import scala.compiletime.uninitialized

/** Raster backend over java.awt. The engine hands this class coordinates and sizes in big points (1/72 inch); the
  * device transform (dpi/72) is applied in exactly one place — a scale on the Graphics2D — so layout is identical
  * across backends and only rasterization density differs. Text is measured against an identity FontRenderContext, so
  * extents are reported back to the engine in points.
  */
class Graphics2DTypesetter(dpi: Double = 100) extends Typesetter:

  type ImageHandle = BufferedImage

  val output: String = null

  private val deviceScale = dpi / 72

  private var page: BufferedImage    = uninitialized
  private var g: Graphics2D          = uninitialized
  private var frc: FontRenderContext = uninitialized

  def init(width: Double, height: Double): Unit =
    page = new BufferedImage(
      (width * deviceScale).toInt max 1,
      (height * deviceScale).toInt max 1,
      BufferedImage.TYPE_INT_ARGB,
    )
    g = page.createGraphics
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.scale(deviceScale, deviceScale)
    frc = new FontRenderContext(null, true, true)

  def createPageTarget: Any =
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, (page.getWidth / deviceScale).toInt + 1, (page.getHeight / deviceScale).toInt + 1)
    page

  def ejectPageTarget(): Unit = ()

  def setFont(font: Any): Unit = g.setFont(font.asInstanceOf[JFont])

  def setColor(color: Color): Unit =
    g.setColor(new java.awt.Color(color.redInt, color.greenInt, color.blueInt, color.alphaInt))

  def setLineWidth(width: Double): Unit = g.setStroke(new BasicStroke(width.toFloat))

  def drawString(text: String, x: Double, y: Double): Unit = g.drawString(text, x.toFloat, y.toFloat)
  def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit =
    g.drawLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
  def drawRect(x: Double, y: Double, width: Double, height: Double): Unit =
    g.drawRect(x.toInt, y.toInt, width.toInt, height.toInt)
  def fillRect(x: Double, y: Double, width: Double, height: Double): Unit =
    g.fillRect(x.toInt, y.toInt, width.toInt, height.toInt)

  def loadFont(path: String): JFont = java.awt.Font.createFont(java.awt.Font.TRUETYPE_FONT, new java.io.File(path))

  def getTextExtents(text: String, font: Any): TextExtents =
    val layout = new TextLayout(text, font.asInstanceOf[JFont], frc)
    val bounds = layout.getBounds

    val ascent = -bounds.getY
//    val descent = layout.getDescent
    val width   = layout.getAdvance // was bounds.getWidth but that put adjacent letters too close
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
    val layout = new TextLayout(c.toString, font.asInstanceOf[JFont], frc)

    layout.getAdvance

  def loadImage(path: String): (ImageHandle, Int, Int) =
    val image = ImageIO.read(new File(path))

    // image pixels are treated as big points (1px = 1pt), matching the PDF backend
    (image, image.getWidth, image.getHeight)

  def drawImage(image: ImageHandle, x: Double, y: Double): Unit =
    g.drawImage(image, x.toInt, y.toInt, null)

  def destroy(): Unit = ()
