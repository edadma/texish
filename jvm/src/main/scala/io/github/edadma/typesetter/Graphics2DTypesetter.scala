package io.github.edadma.typesetter

import java.awt.font.{FontRenderContext, TextLayout}
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Graphics2D, RenderingHints, Font as JFont}
import java.io.File
import javax.imageio.ImageIO
import scala.compiletime.uninitialized

class Graphics2DTypesetter extends Typesetter:

  type ImageHandle = BufferedImage

  val output: String = null

  private var page: BufferedImage    = uninitialized
  private var g: Graphics2D          = uninitialized
  private var frc: FontRenderContext = uninitialized

  g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

  def initTarget(): Unit =
    page = new BufferedImage(10, 10, BufferedImage.TYPE_INT_ARGB)
    g = page.createGraphics()
    frc = g.getFontRenderContext

  def createPageTarget(width: Double, height: Double): Any =
    page = new BufferedImage(
      width.toInt,
      height.toInt,
      BufferedImage.TYPE_INT_ARGB,
    )
    g = page.createGraphics()
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, page.getWidth, page.getHeight)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    page

  def ejectPageTarget(): Unit = ()

  /*
  Precision-7530
  --------------
  xrandr | grep -w connected
  eDP-1 connected primary 1920x1080+0+0 (normal left inverted right x axis y axis) 340mm x 190mm

  DPI is 143.435294114
   */
  def getDPI: Double = 100 // 143.435294114 // Toolkit.getDefaultToolkit.getScreenResolution

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
    setFont(font)
    g.getFontMetrics.charWidth(c)

  def loadImage(path: String): (ImageHandle, Int, Int) =
    val image = ImageIO.read(new File(path))

    (image, image.getWidth, image.getHeight)

  def drawImage(image: ImageHandle, x: Double, y: Double): Unit =
    g.drawImage(image, x.toInt, y.toInt, null)

//    val glyphs = font.createGlyphVector(frc, text)
//    val lb = glyphs.getLogicalBounds
//    val vb = glyphs.getVisualBounds
//
//    println(lb.getX)
//    println(lb.getWidth)
//    println(vb.getWidth)
//    TextExtents(lb.getX, vb.getY, vb.getWidth, vb.getHeight, lb.getWidth, 0)

  def destroy(): Unit = ()
