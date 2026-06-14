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
  type FontFace    = JFont
  type RenderFont  = JFont

  val output: String = null

  private val deviceScale = dpi / 72

  private var pageWidth: Double      = 0
  private var pageHeight: Double     = 0
  private var page: BufferedImage    = uninitialized
  private var g: Graphics2D          = uninitialized
  private var frc: FontRenderContext = uninitialized

  def init(width: Double, height: Double): Unit =
    pageWidth = width
    pageHeight = height
    newPageImage()
    frc = new FontRenderContext(null, true, true)

  // each page is its own image: the document collects every shipped page, so the target can't be reused
  private def newPageImage(): Unit =
    page = new BufferedImage(
      (pageWidth * deviceScale).toInt max 1,
      (pageHeight * deviceScale).toInt max 1,
      BufferedImage.TYPE_INT_ARGB,
    )
    g = page.createGraphics
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    // drawing must use the same fractional metrics the FontRenderContext measures with: with integer glyph
    // advances the drawn text drifts off the measured positions, a glyph or two per word, eating interword space
    g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
    g.scale(deviceScale, deviceScale)

  def createPageTarget: Any =
    newPageImage()
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, (page.getWidth / deviceScale).toInt + 1, (page.getHeight / deviceScale).toInt + 1)
    page

  def ejectPageTarget(): Unit = ()

  def setFont(font: RenderFont): Unit = g.setFont(font)

  def setColor(color: Color): Unit =
    g.setColor(new java.awt.Color(color.redInt, color.greenInt, color.blueInt, color.alphaInt))

  def setLineWidth(width: Double): Unit = g.setStroke(new BasicStroke(width.toFloat))

  // geometry goes through Shape so fractional coordinates and sizes rasterize by coverage: a rule thinner
  // than a device pixel (0.4pt at screen resolutions) still leaves proportional ink instead of truncating to
  // an integer height of zero
  def drawString(text: String, x: Double, y: Double): Unit = g.drawString(text, x.toFloat, y.toFloat)
  def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit =
    g.draw(new java.awt.geom.Line2D.Double(x1, y1, x2, y2))
  def drawRect(x: Double, y: Double, width: Double, height: Double): Unit =
    g.draw(new java.awt.geom.Rectangle2D.Double(x, y, width, height))
  def fillRect(x: Double, y: Double, width: Double, height: Double): Unit =
    g.fill(new java.awt.geom.Rectangle2D.Double(x, y, width, height))

  def loadFont(path: String): FontFace = java.awt.Font.createFont(java.awt.Font.TRUETYPE_FONT, new java.io.File(path))

  def getTextExtents(text: String, font: RenderFont): TextExtents =
    val layout = new TextLayout(text, font, frc)
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

  def makeFont(font: FontFace, size: Double): RenderFont = font.deriveFont(size.toFloat)

  def charWidth(font: RenderFont, c: Char): Double =
    val layout = new TextLayout(c.toString, font, frc)

    layout.getAdvance

  def glyphIndex(font: RenderFont, codepoint: Int): Int =
    font.createGlyphVector(frc, new String(Character.toChars(codepoint))).getGlyphCode(0)

  // createGlyphVector(frc, int[]) treats the ints as glyph codes (no cmap mapping), which is exactly
  // what we want once we already hold an index. GlyphMetrics gives the advance and the visual bounds
  // relative to the baseline origin (y grows downward, so the top of the glyph has negative y).
  def glyphExtents(font: RenderFont, glyph: Int): TextExtents =
    val gv     = font.createGlyphVector(frc, Array(glyph))
    val m      = gv.getGlyphMetrics(0)
    val bounds = m.getBounds2D

    TextExtents(
      xBearing = bounds.getX,
      yBearing = bounds.getY,
      width = bounds.getWidth,
      height = bounds.getHeight,
      xAdvance = m.getAdvanceX,
      yAdvance = m.getAdvanceY,
    )

  def drawGlyph(font: RenderFont, glyph: Int, x: Double, y: Double): Unit =
    g.drawGlyphVector(font.createGlyphVector(frc, Array(glyph)), x.toFloat, y.toFloat)

  def loadImage(path: String): (ImageHandle, Int, Int) =
    val image = ImageIO.read(new File(path))

    // image pixels are treated as big points (1px = 1pt), matching the PDF backend
    (image, image.getWidth, image.getHeight)

  def drawImage(image: ImageHandle, x: Double, y: Double): Unit =
    g.drawImage(image, x.toInt, y.toInt, null)

  def destroy(): Unit = ()
