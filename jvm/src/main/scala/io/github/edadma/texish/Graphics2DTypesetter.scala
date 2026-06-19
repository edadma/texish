package io.github.edadma.texish

import java.awt.font.{FontRenderContext, TextLayout}
import java.awt.geom.{AffineTransform, GeneralPath}
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Graphics2D, Paint as JPaint, RenderingHints, Shape as JShape, Font as JFont}
import java.io.File
import java.nio.file.{Files, Paths}
import javax.imageio.ImageIO
import scala.compiletime.uninitialized

import io.github.edadma.texish.opentype.Sfnt

/** A loaded typeface, retaining the file it came from so the glyph/math layer can read raw SFNT tables
  * (java.awt.Font exposes no way back to its source bytes). */
case class AwtFace(font: JFont, path: String)

/** A sized font, carrying the source path forward from its [[AwtFace]] for the same reason. */
case class AwtRenderFont(font: JFont, path: String)

/** Raster backend over java.awt. The engine hands this class coordinates and sizes in big points (1/72 inch); the
  * device transform (dpi/72) is applied in exactly one place — a scale on the Graphics2D — so layout is identical
  * across backends and only rasterization density differs. Text is measured against an identity FontRenderContext, so
  * extents are reported back to the engine in points.
  */
class Graphics2DTypesetter(dpi: Double = 100) extends Typesetter:

  type ImageHandle = BufferedImage
  type FontFace    = AwtFace
  type RenderFont  = AwtRenderFont

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
    setColor(backgroundColor)
    g.fillRect(0, 0, (page.getWidth / deviceScale).toInt + 1, (page.getHeight / deviceScale).toInt + 1)
    page

  def ejectPageTarget(): Unit = ()

  def setFont(font: RenderFont): Unit = g.setFont(font.font)

  def setColor(color: Color): Unit =
    g.setColor(new java.awt.Color(color.redInt, color.greenInt, color.blueInt, color.alphaInt))

  def setLineWidth(width: Double): Unit =
    lineWidth = width.toFloat
    rebuildStroke()

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

  def loadFont(path: String): FontFace =
    AwtFace(java.awt.Font.createFont(java.awt.Font.TRUETYPE_FONT, new java.io.File(path)), path)

  def getTextExtents(text: String, font: RenderFont): TextExtents =
    val layout = new TextLayout(text, font.font, frc)
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

  def makeFont(font: FontFace, size: Double): RenderFont = AwtRenderFont(font.font.deriveFont(size.toFloat), font.path)

  def charWidth(font: RenderFont, c: Char): Double =
    val layout = new TextLayout(c.toString, font.font, frc)

    layout.getAdvance

  def glyphIndex(font: RenderFont, codepoint: Int): Int =
    font.font.createGlyphVector(frc, new String(Character.toChars(codepoint))).getGlyphCode(0)

  // createGlyphVector(frc, int[]) treats the ints as glyph codes (no cmap mapping), which is exactly
  // what we want once we already hold an index. GlyphMetrics gives the advance and the visual bounds
  // relative to the baseline origin (y grows downward, so the top of the glyph has negative y).
  def glyphExtents(font: RenderFont, glyph: Int): TextExtents =
    val gv     = font.font.createGlyphVector(frc, Array(glyph))
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
    g.drawGlyphVector(font.font.createGlyphVector(frc, Array(glyph)), x.toFloat, y.toFloat)

  // java.awt.Font won't surface its raw table bytes, so reach the SFNT directory by reading the font
  // file the face was loaded from and slicing the requested table out of it.
  def sfntTable(font: RenderFont, tag: String): Option[Array[Byte]] =
    val bytes = Files.readAllBytes(Paths.get(font.path))

    Sfnt(bytes).table(tag).map(r => bytes.slice(r.offset, r.offset + r.length))

  def loadImage(path: String): (ImageHandle, Int, Int) =
    val image = ImageIO.read(new File(path))

    // image pixels are treated as big points (1px = 1pt), matching the PDF backend
    (image, image.getWidth, image.getHeight)

  def drawImage(image: ImageHandle, x: Double, y: Double, w: Double, h: Double): Unit =
    val tx = java.awt.geom.AffineTransform.getTranslateInstance(x, y)
    if image.getWidth > 0 && image.getHeight > 0 then tx.scale(w / image.getWidth, h / image.getHeight)
    g.drawImage(image, tx, null)

  override def imagesSupported: Boolean = true

  // Build an ARGB image from straight (non-premultiplied) ARGB pixels — exactly TYPE_INT_ARGB's layout.
  override def createImage(width: Int, height: Int, argb: Array[Int]): ImageHandle =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    image.setRGB(0, 0, width, height, argb, 0, width)
    image

  // Picture-graphics seam. java.awt has no implicit current-path or device stroke state, so this layer keeps
  // its own: a GeneralPath accumulator built up by the path ops and painted by fill/stroke, and the stroke
  // parameters (width, dash, caps, joins) tracked as fields so each can be set independently and combined into
  // one BasicStroke. Transforms and clip compose on the Graphics2D itself.

  private var path: GeneralPath = new GeneralPath

  // BasicStroke(width) defaults to square caps and miter joins; mirror that so an engine rule drawn through
  // setLineWidth keeps rendering exactly as before this seam existed.
  private var lineWidth: Float        = 1f
  private var lineCap: Int            = BasicStroke.CAP_SQUARE
  private var lineJoin: Int           = BasicStroke.JOIN_MITER
  private var dashArray: Array[Float] = null
  private var dashPhase: Float        = 0f

  private def rebuildStroke(): Unit =
    g.setStroke(
      if dashArray == null || dashArray.isEmpty then new BasicStroke(lineWidth, lineCap, lineJoin)
      else new BasicStroke(lineWidth, lineCap, lineJoin, 10f, dashArray, dashPhase),
    )

  private case class GState(
      transform: AffineTransform,
      clip: JShape | Null,
      paint: JPaint,
      lineWidth: Float,
      lineCap: Int,
      lineJoin: Int,
      dashArray: Array[Float],
      dashPhase: Float,
  )

  private var gstack: List[GState] = Nil

  override def gsave(): Unit =
    gstack ::= GState(g.getTransform, g.getClip, g.getPaint, lineWidth, lineCap, lineJoin, dashArray, dashPhase)

  override def grestore(): Unit =
    gstack match
      case s :: rest =>
        gstack = rest
        g.setTransform(s.transform)
        g.setClip(s.clip)
        g.setPaint(s.paint)
        lineWidth = s.lineWidth
        lineCap = s.lineCap
        lineJoin = s.lineJoin
        dashArray = s.dashArray
        dashPhase = s.dashPhase
        rebuildStroke()
      case Nil => // unbalanced restore: ignore, as the rasterizer's own stack would

  override def translate(dx: Double, dy: Double): Unit = g.translate(dx, dy)
  override def scale(sx: Double, sy: Double): Unit     = g.scale(sx, sy)
  override def rotate(radians: Double): Unit           = g.rotate(radians)

  override def newPath(): Unit                    = path = new GeneralPath
  override def moveTo(x: Double, y: Double): Unit = path.moveTo(x, y)
  override def lineTo(x: Double, y: Double): Unit = path.lineTo(x, y)

  override def curveTo(c1x: Double, c1y: Double, c2x: Double, c2y: Double, x: Double, y: Double): Unit =
    path.curveTo(c1x, c1y, c2x, c2y, x, y)

  // Approximate the arc with cubic Béziers (≤90° per segment) using the same parametric circle
  // point = (cx + r·cosθ, cy + r·sinθ) that Cairo's native arc traces, so both backends draw the same curve
  // under the same transform. The control-handle length k = 4/3·tan(Δ/4) is the standard arc-to-Bézier fit.
  override def arc(cx: Double, cy: Double, r: Double, a0: Double, a1: Double, negative: Boolean): Unit =
    var end = a1
    if !negative then while end < a0 do end += 2 * math.Pi
    else while end > a0 do end -= 2 * math.Pi

    val sweep = end - a0
    val nseg  = math.max(1, math.ceil(math.abs(sweep) / (math.Pi / 2)).toInt)
    val delta = sweep / nseg
    val k     = 4.0 / 3.0 * math.tan(delta / 4)

    val sx = cx + r * math.cos(a0)
    val sy = cy + r * math.sin(a0)
    if path.getCurrentPoint == null then path.moveTo(sx, sy) else path.lineTo(sx, sy)

    var theta = a0
    var i     = 0
    while i < nseg do
      val t2  = theta + delta
      val p1x = cx + r * math.cos(theta)
      val p1y = cy + r * math.sin(theta)
      val p2x = cx + r * math.cos(t2)
      val p2y = cy + r * math.sin(t2)
      path.curveTo(
        p1x - k * r * math.sin(theta),
        p1y + k * r * math.cos(theta),
        p2x + k * r * math.sin(t2),
        p2y - k * r * math.cos(t2),
        p2x,
        p2y,
      )
      theta = t2
      i += 1

  override def closePath(): Unit = path.closePath()

  override def fillPath(preserve: Boolean): Unit =
    g.fill(path)
    if !preserve then path = new GeneralPath

  override def strokePath(): Unit =
    g.draw(path)
    path = new GeneralPath

  override def clipPath(): Unit =
    g.clip(path)
    path = new GeneralPath

  override def setDash(pattern: Seq[Double], offset: Double): Unit =
    dashArray = if pattern.isEmpty then null else pattern.map(_.toFloat).toArray
    dashPhase = offset.toFloat
    rebuildStroke()

  override def setLineCap(cap: LineCap): Unit =
    lineCap = cap match
      case LineCap.Butt   => BasicStroke.CAP_BUTT
      case LineCap.Round  => BasicStroke.CAP_ROUND
      case LineCap.Square => BasicStroke.CAP_SQUARE
    rebuildStroke()

  override def setLineJoin(join: LineJoin): Unit =
    lineJoin = join match
      case LineJoin.Miter => BasicStroke.JOIN_MITER
      case LineJoin.Round => BasicStroke.JOIN_ROUND
      case LineJoin.Bevel => BasicStroke.JOIN_BEVEL
    rebuildStroke()

  def destroy(): Unit = ()
