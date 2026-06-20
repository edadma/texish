package io.github.edadma.texish

import io.github.edadma.texish.opentype.{OtfFont, PathSeg}

import org.scalajs.dom

import scala.compiletime.uninitialized
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.*

/** A loaded typeface for the canvas backend: the parsed font — used for layout metrics and for filling the math
  * glyphs that have no codepoint — paired with the CSS family name its bytes were registered under, so ordinary
  * text can be drawn with the browser's own hinted rasteriser through `fillText`. */
case class CanvasFace(font: OtfFont, family: String)

/** A sized canvas font: the typeface, the point size, and the registered CSS family. */
case class CanvasRenderFont(font: OtfFont, size: Double, family: String)

/** One rendered page: an offscreen canvas drawn at device resolution. After the page is ejected, [[result]]
  * holds a second canvas cropped tight to the drawn ink (sized in points, so it displays at physical size). */
final class CanvasPage(val canvas: dom.html.Canvas, val ctx: dom.CanvasRenderingContext2D):
  var result: dom.html.Canvas = canvas

/** Renders the document with the HTML canvas, for the browser. Ordinary text is drawn glyph by glyph with the
  * browser's hinted `fillText` at positions the layout engine computed — so it is as crisp as native text,
  * unlike outline-filled SVG paths — while the size-variant and assembly glyphs of math, which have no
  * codepoint to address, are filled as canvas paths the way the SVG backend fills them as `<path>`. The picture
  * seam maps straight onto the canvas's own transform and path API. Layout is identical to the SVG and PDF
  * backends: metrics still come from the pure-Scala font engine. The output is raster (drawn at the device
  * pixel ratio), so it is the in-browser companion to the vector SVG backend used for build-time output. */
class CanvasTypesetter extends Typesetter with EmbeddedFontSet:

  type ImageHandle = AnyRef
  type FontFace    = CanvasFace
  type RenderFont  = CanvasRenderFont

  val output: String = null

  /** Device pixels per point: the CSS points-per-inch ratio times the display's pixel ratio, so a page drawn in
    * point coordinates fills the canvas at full device resolution and stays crisp on a high-DPI screen. */
  private val scale: Double = (96.0 / 72.0) * dom.window.devicePixelRatio

  /** The margin, in points, left around the ink when a page is cropped to its content. */
  var cropMargin: Double = 1.0

  // The first cropped page's geometry (in points), recorded while cropping so a fragment renderer can align an
  // inline formula on the surrounding text's baseline.
  private var firstFragment: Option[FragmentMetrics] = None
  override def fragmentMetrics: Option[FragmentMetrics] = firstFragment

  private var pageWidth: Double  = 0
  private var pageHeight: Double = 0
  private var page: CanvasPage   = uninitialized

  private var curColor: Color        = Color("black")
  private var curFont: CanvasRenderFont = uninitialized
  private var curLineWidth: Double   = 1.0
  private var curDash: Seq[Double]   = Nil
  private var curCap: LineCap        = LineCap.Square
  private var curJoin: LineJoin      = LineJoin.Miter

  private final case class GState(color: Color, lineWidth: Double, dash: Seq[Double], cap: LineCap, join: LineJoin)
  private var gstack: List[GState] = Nil

  // ─── font registration ────────────────────────────────────────────────────────

  // The FontFace / document.fonts API is reached through js.Dynamic: this version of scalajs-dom has no facade
  // for it.
  private def fontFaceSet: js.Dynamic = dom.document.asInstanceOf[js.Dynamic].fonts

  /** Register a font's bytes with the browser under `family` and kick off its load, so `fillText` can draw with
    * it. The decode for layout metrics happens through `OtfFont` separately. */
  private def registerBrowserFont(family: String, bytes: Array[Byte]): Unit =
    val buffer = bytes.toTypedArray.buffer
    val face   = js.Dynamic.newInstance(js.Dynamic.global.FontFace)(family, buffer.asInstanceOf[js.Any])
    fontFaceSet.add(face)
    face.load()

  def loadFont(path: String): FontFace =
    val bytes  = EmbeddedFonts.bytes(path)
    val family = "tx-" + path.map(c => if c.isLetterOrDigit then c else '-')
    registerBrowserFont(family, bytes)
    CanvasFace(new OtfFont(bytes), family)

  def makeFont(font: FontFace, size: Double): RenderFont = CanvasRenderFont(font.font, size, font.family)

  /** A promise that resolves once every registered font has finished loading in the browser; the renderer waits
    * on it before drawing, since `fillText` needs the face present. */
  def fontsReady: js.Promise[js.Any] = fontFaceSet.ready.asInstanceOf[js.Promise[js.Any]]

  // ─── page lifecycle ─────────────────────────────────────────────────────────────

  def init(width: Double, height: Double): Unit =
    pageWidth = width
    pageHeight = height

  def createPageTarget: Any =
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
    canvas.width = math.ceil(pageWidth * scale).toInt
    canvas.height = math.ceil(pageHeight * scale).toInt
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    // Draw in point coordinates; the device-pixel scale lives in the base transform. The background is left
    // transparent so the ink can be found by alpha when the page is cropped, and so inline output blends in.
    ctx.setTransform(scale, 0, 0, scale, 0, 0)
    ctx.textBaseline = "alphabetic"
    ctx.asInstanceOf[js.Dynamic].fontKerning = "none" // the engine positions every glyph; no browser kerning
    gstack = Nil
    page = new CanvasPage(canvas, ctx)
    page

  def ejectPageTarget(): Unit = page.result = cropToInk(page.canvas)

  /** Crop a full-page canvas to a tight box around its drawn ink (any non-transparent pixel), plus a small
    * margin, and return a new canvas sized in points so it displays at physical size. */
  private def cropToInk(src: dom.html.Canvas): dom.html.Canvas =
    val w   = src.width
    val h   = src.height
    val ctx = src.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val data = ctx.getImageData(0, 0, w, h).data
    var minX = w; var minY = h; var maxX = -1; var maxY = -1
    var y = 0
    while y < h do
      var x = 0
      while x < w do
        if data((y * w + x) * 4 + 3) > 8 then // alpha
          if x < minX then minX = x
          if x > maxX then maxX = x
          if y < minY then minY = y
          if y > maxY then maxY = y
        x += 1
      y += 1

    if maxX < minX then newCanvas(1, 1)
    else
      val m  = (cropMargin * scale).toInt
      val x0 = math.max(0, minX - m)
      val y0 = math.max(0, minY - m)
      val x1 = math.min(w, maxX + 1 + m)
      val y1 = math.min(h, maxY + 1 + m)
      val sw = x1 - x0
      val sh = y1 - y0
      val out  = newCanvas(sw, sh)
      val octx = out.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
      octx.drawImage(src, x0.toDouble, y0.toDouble, sw.toDouble, sh.toDouble, 0, 0, sw.toDouble, sh.toDouble)
      out.style.width = s"${fmt(sw / scale)}pt"
      out.style.height = s"${fmt(sh / scale)}pt"
      // The crop box is in device pixels; convert to points (the engine's unit) and place the baseline within
      // it, so an inline formula can be aligned on the surrounding text's baseline.
      if firstFragment.isEmpty then
        firstFragment = Some(FragmentMetrics(sw / scale, sh / scale, bodyBaseline - y0 / scale))
      out

  private def newCanvas(w: Int, h: Int): dom.html.Canvas =
    val c = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
    c.width = w
    c.height = h
    c

  /** The cropped canvas of each shipped page, in order — ready to insert into the document. */
  def pageCanvases: Seq[dom.html.Canvas] =
    getDocument.printedPages.toSeq.collect { case p: CanvasPage => p.result }

  // ─── font / colour / stroke state ─────────────────────────────────────────────

  def setFont(font: RenderFont): Unit   = curFont = font
  def setColor(color: Color): Unit      = curColor = color
  def setLineWidth(width: Double): Unit = curLineWidth = width

  private def applyFill(): Unit  = page.ctx.fillStyle = cssColor(curColor)
  private def applyStroke(): Unit =
    val ctx = page.ctx
    ctx.strokeStyle = cssColor(curColor)
    ctx.lineWidth = curLineWidth
    ctx.setLineDash(curDash.toJSArray)
    ctx.lineCap = capName(curCap)
    ctx.lineJoin = joinName(curJoin)

  // ─── metrics (from the font engine, so layout matches the SVG and PDF backends) ──

  def charWidth(font: RenderFont, c: Char): Double =
    font.font.advanceWidthEm(font.font.glyphIndex(c.toInt)) * font.size

  def getTextExtents(text: String, font: RenderFont): TextExtents =
    var advance = 0.0
    var minY    = Double.MaxValue
    var maxY    = Double.MinValue
    var minX    = Double.MaxValue
    var maxX    = Double.MinValue
    eachGlyph(text) { cp =>
      val gid              = font.font.glyphIndex(cp)
      val (x0, y0, x1, y1) = inkBounds(font.font.outline(gid))
      if x1 >= x0 then
        minX = math.min(minX, advance + x0 * font.size)
        maxX = math.max(maxX, advance + x1 * font.size)
        minY = math.min(minY, y0 * font.size)
        maxY = math.max(maxY, y1 * font.size)
      advance += font.font.advanceWidthEm(gid) * font.size
    }
    if maxY < minY then TextExtents(0, 0, advance, 0, advance, 0)
    else
      TextExtents(
        xBearing = if minX <= maxX then minX else 0,
        yBearing = -maxY,
        width = if minX <= maxX then maxX - minX else 0,
        height = maxY - minY,
        xAdvance = advance,
        yAdvance = 0,
      )

  def glyphIndex(font: RenderFont, codepoint: Int): Int = font.font.glyphIndex(codepoint)

  def glyphExtents(font: RenderFont, glyph: Int): TextExtents =
    val (x0, y0, x1, y1) = inkBounds(font.font.outline(glyph))
    val advance          = font.font.advanceWidthEm(glyph) * font.size
    if x1 < x0 then TextExtents(0, 0, 0, 0, advance, 0)
    else
      TextExtents(
        xBearing = x0 * font.size,
        yBearing = -y1 * font.size,
        width = (x1 - x0) * font.size,
        height = (y1 - y0) * font.size,
        xAdvance = advance,
        yAdvance = 0,
      )

  def sfntTable(font: RenderFont, tag: String): Option[Array[Byte]] = font.font.tableBytes(tag)

  // ─── text & glyph drawing ───────────────────────────────────────────────────────

  def drawString(text: String, x: Double, y: Double): Unit =
    if text.isEmpty || curFont == null then return
    val ctx = page.ctx
    applyFill()
    ctx.font = s"${fmt(curFont.size)}px '${curFont.family}'"
    // Place each glyph at the x the layout engine computed (advancing by the font's own widths), so the line
    // is positioned exactly as in the SVG and PDF output, while the glyph itself is the browser's hinted form.
    var ax = x
    eachGlyph(text) { cp =>
      val gid = curFont.font.glyphIndex(cp)
      ctx.fillText(stringOf(cp), ax, y)
      ax += curFont.font.advanceWidthEm(gid) * curFont.size
    }

  // Math mode positions every glyph by index — including ordinary letters, digits and operators, which do have
  // a codepoint. Draw those with the browser's hinted fillText, exactly like body text, so they are crisp; only
  // the size-variant and assembly glyphs that have no codepoint fall back to an outline fill.
  def drawGlyph(font: RenderFont, glyph: Int, x: Double, y: Double): Unit =
    font.font.codepointForGlyph(glyph) match
      case Some(cp) =>
        val ctx = page.ctx
        applyFill()
        ctx.font = s"${fmt(font.size)}px '${font.family}'"
        ctx.fillText(stringOf(cp), x, y)
      case None =>
        val p = new dom.Path2D()
        for seg <- font.font.outline(glyph) do
          seg match
            case PathSeg.MoveTo(px, py) => p.moveTo(x + px * font.size, y - py * font.size)
            case PathSeg.LineTo(px, py) => p.lineTo(x + px * font.size, y - py * font.size)
            case PathSeg.CurveTo(x1, y1, x2, y2, px, py) =>
              p.bezierCurveTo(
                x + x1 * font.size,
                y - y1 * font.size,
                x + x2 * font.size,
                y - y2 * font.size,
                x + px * font.size,
                y - py * font.size,
              )
            case PathSeg.Close => p.closePath()
        applyFill()
        page.ctx.fill(p)

  // ─── immediate rules ──────────────────────────────────────────────────────────

  def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit =
    val ctx = page.ctx
    applyStroke()
    ctx.beginPath()
    ctx.moveTo(x1, y1)
    ctx.lineTo(x2, y2)
    ctx.stroke()

  def drawRect(x: Double, y: Double, width: Double, height: Double): Unit =
    applyStroke()
    page.ctx.strokeRect(x, y, width, height)

  def fillRect(x: Double, y: Double, width: Double, height: Double): Unit =
    applyFill()
    page.ctx.fillRect(x, y, width, height)

  // ─── picture-graphics seam (native canvas transform + path API) ─────────────────

  override def gsave(): Unit =
    page.ctx.save()
    gstack ::= GState(curColor, curLineWidth, curDash, curCap, curJoin)

  override def grestore(): Unit =
    page.ctx.restore()
    gstack match
      case s :: rest =>
        gstack = rest
        curColor = s.color
        curLineWidth = s.lineWidth
        curDash = s.dash
        curCap = s.cap
        curJoin = s.join
      case Nil =>

  override def translate(dx: Double, dy: Double): Unit = page.ctx.translate(dx, dy)
  override def scale(sx: Double, sy: Double): Unit     = page.ctx.scale(sx, sy)
  override def rotate(radians: Double): Unit           = page.ctx.rotate(radians)

  override def newPath(): Unit                    = page.ctx.beginPath()
  override def moveTo(x: Double, y: Double): Unit = page.ctx.moveTo(x, y)
  override def lineTo(x: Double, y: Double): Unit = page.ctx.lineTo(x, y)
  override def curveTo(c1x: Double, c1y: Double, c2x: Double, c2y: Double, x: Double, y: Double): Unit =
    page.ctx.bezierCurveTo(c1x, c1y, c2x, c2y, x, y)
  override def arc(cx: Double, cy: Double, r: Double, a0: Double, a1: Double, negative: Boolean): Unit =
    page.ctx.arc(cx, cy, r, a0, a1, negative)
  override def closePath(): Unit = page.ctx.closePath()

  override def fillPath(preserve: Boolean): Unit =
    applyFill()
    page.ctx.fill()
  override def strokePath(): Unit =
    applyStroke()
    page.ctx.stroke()
  override def clipPath(): Unit = page.ctx.clip()

  override def setDash(pattern: Seq[Double], offset: Double): Unit =
    curDash = pattern
    page.ctx.lineDashOffset = offset
  override def setLineCap(cap: LineCap): Unit   = curCap = cap
  override def setLineJoin(join: LineJoin): Unit = curJoin = join

  // ─── images (not yet supported) ─────────────────────────────────────────────────

  def loadImage(path: String): (ImageHandle, Int, Int) =
    throw new UnsupportedOperationException("the canvas backend does not support raster images yet")
  def drawImage(image: ImageHandle, x: Double, y: Double, w: Double, h: Double): Unit = ()

  def destroy(): Unit = ()

  // ─── helpers ────────────────────────────────────────────────────────────────────

  private def stringOf(cp: Int): String = new String(Character.toChars(cp))

  private def capName(c: LineCap): String = c match
    case LineCap.Butt   => "butt"
    case LineCap.Round  => "round"
    case LineCap.Square => "square"

  private def joinName(j: LineJoin): String = j match
    case LineJoin.Miter => "miter"
    case LineJoin.Round => "round"
    case LineJoin.Bevel => "bevel"

  private inline def eachGlyph(text: String)(f: Int => Unit): Unit =
    var i = 0
    while i < text.length do
      val c = text.charAt(i)
      if Character.isHighSurrogate(c) && i + 1 < text.length && Character.isLowSurrogate(text.charAt(i + 1)) then
        f(Character.toCodePoint(c, text.charAt(i + 1)))
        i += 2
      else
        f(c.toInt)
        i += 1

  private def inkBounds(segs: Vector[PathSeg]): (Double, Double, Double, Double) =
    if segs.isEmpty then return (Double.MaxValue, Double.MaxValue, Double.MinValue, Double.MinValue)
    var minX = Double.MaxValue; var minY = Double.MaxValue
    var maxX = Double.MinValue; var maxY = Double.MinValue
    var cx   = 0.0; var cy = 0.0
    def see(x: Double, y: Double): Unit =
      minX = math.min(minX, x); minY = math.min(minY, y)
      maxX = math.max(maxX, x); maxY = math.max(maxY, y)
    for seg <- segs do
      seg match
        case PathSeg.MoveTo(x, y) => cx = x; cy = y; see(x, y)
        case PathSeg.LineTo(x, y) => cx = x; cy = y; see(x, y)
        case PathSeg.CurveTo(x1, y1, x2, y2, x, y) =>
          val (sx, sy) = (cx, cy)
          var i        = 1
          while i <= 8 do
            val t = i / 8.0; val u = 1 - t
            see(
              u * u * u * sx + 3 * u * u * t * x1 + 3 * u * t * t * x2 + t * t * t * x,
              u * u * u * sy + 3 * u * u * t * y1 + 3 * u * t * t * y2 + t * t * t * y,
            )
            i += 1
          cx = x; cy = y
        case PathSeg.Close => ()
    (minX, minY, maxX, maxY)

  private def cssColor(c: Color): String =
    if c.alpha >= 1.0 then f"rgb(${c.redInt},${c.greenInt},${c.blueInt})"
    else f"rgba(${c.redInt},${c.greenInt},${c.blueInt},${c.alpha}%.3f)"

  private def fmt(v: Double): String =
    if v.isNaN || v.isInfinite || math.abs(v) < 5e-4 then "0"
    else
      val s = BigDecimal(v).setScale(3, BigDecimal.RoundingMode.HALF_UP).bigDecimal.stripTrailingZeros.toPlainString
      if s == "-0" then "0" else s

end CanvasTypesetter
