package io.github.edadma.texish

import io.github.edadma.texish.opentype.{OtfFont, PathSeg}

import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

/** A loaded typeface for the SVG backend: the parsed OpenType font, kept with the path it was read from for
  * diagnostics. The font reads its own outlines and tables in pure Scala, so this backend runs unchanged on
  * the JVM (build-time SVG) and Scala.js (the in-browser renderer) alike. */
case class SvgFace(font: OtfFont, path: String)

/** A sized SVG font: the typeface paired with the point size to scale its em-space outlines by. */
case class SvgRenderFont(font: OtfFont, size: Double)

/** An affine transform `(x, y) ↦ (a·x + c·y + e, b·x + d·y + f)`, the backend's software current
  * transformation matrix. The picture seam composes onto it and every emitted coordinate is pushed through
  * it, so the SVG is written in flat device space with no `transform` attributes — which keeps clip regions
  * and placed glyphs correct without relying on nested SVG group transforms. */
private case class SvgMatrix(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double):
  def apply(x: Double, y: Double): (Double, Double) = (a * x + c * y + e, b * x + d * y + f)

  // this ∘ other: apply `other` first, then this — so composing a local translate/scale/rotate onto the CTM
  // puts new user coordinates through the local transform before the established one, as a graphics CTM does.
  infix def *(o: SvgMatrix): SvgMatrix =
    SvgMatrix(
      a * o.a + c * o.b,
      b * o.a + d * o.b,
      a * o.c + c * o.d,
      b * o.c + d * o.d,
      a * o.e + c * o.f + e,
      b * o.e + d * o.f + f,
    )

private object SvgMatrix:
  val identity: SvgMatrix                        = SvgMatrix(1, 0, 0, 1, 0, 0)
  def translate(dx: Double, dy: Double): SvgMatrix = SvgMatrix(1, 0, 0, 1, dx, dy)
  def scale(sx: Double, sy: Double): SvgMatrix     = SvgMatrix(sx, 0, 0, sy, 0, 0)
  def rotate(t: Double): SvgMatrix                 = SvgMatrix(math.cos(t), math.sin(t), -math.sin(t), math.cos(t), 0, 0)

/** One shipped page, collected by [[DocumentMode.printedPages]]. Drawing appends to `content`; clip regions
  * append a `<clipPath>` to `defs`. When the page is ejected its full `<svg>` document is assembled into
  * [[svg]], so a host can read it back from `printedPages`. */
final class SvgPage(val width: Double, val height: Double):
  private[texish] val content                = new StringBuilder
  private[texish] val defs                   = new StringBuilder
  private[texish] var clipCounter            = 0

  // The bounding box of the drawn ink, in device space, accumulated as geometry is emitted (the page
  // background is excluded). Used to tight-crop the page to its content when cropping is enabled.
  private[texish] var inkMinX = Double.MaxValue
  private[texish] var inkMinY = Double.MaxValue
  private[texish] var inkMaxX = Double.MinValue
  private[texish] var inkMaxY = Double.MinValue

  private[texish] def see(x: Double, y: Double): Unit =
    if x < inkMinX then inkMinX = x
    if y < inkMinY then inkMinY = y
    if x > inkMaxX then inkMaxX = x
    if y > inkMaxY then inkMaxY = y

  private[texish] def hasInk: Boolean = inkMaxX >= inkMinX

  /** The assembled standalone `<svg>` document, available once the page has been ejected. */
  var svg: String = ""

/** A vector path under construction in the picture seam. Coordinates are stored already pushed through the
  * CTM — frozen when the segment is added, exactly as Cairo and the canvas freeze theirs — so a transform set
  * between building a path and painting it affects only later segments and the stroke pen, never geometry
  * already recorded. */
private enum SvgPathCmd:
  case M(x: Double, y: Double)
  case L(x: Double, y: Double)
  case C(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double)
  case Z

/** Renders the document to SVG, one self-contained `<svg>` document per page. Everything is filled as path
  * geometry: text and math glyphs are drawn by extracting their outlines from the font and emitting `<path>`,
  * so the output embeds no fonts and is geometry-identical to the PDF — and reaches the glyph-index-addressed
  * size variants and assembly pieces of math, which a `<text>` element could not. Coordinates match the page
  * device space the rest of the engine works in: points, y growing downward, origin at the top-left corner.
  */
class SvgTypesetter extends Typesetter:

  type ImageHandle = AnyRef
  type FontFace    = SvgFace
  type RenderFont  = SvgRenderFont

  val output: String = null

  /** When true, each page's `<svg>` is tight-cropped to its drawn ink (plus a small margin) instead of using
    * the full page box — so a short fragment renders at its natural size, the way an inline math snippet
    * should, rather than as a mostly-empty page shrunk to fit. The build-time document renderer leaves this
    * off to emit real pages; the in-browser renderer turns it on. */
  var cropToContent: Boolean = false

  /** The margin, in points, left around the ink when [[cropToContent]] crops a page. */
  var cropMargin: Double = 1.0

  // The first cropped page's geometry, recorded at eject so a fragment renderer can read it back as
  // FragmentMetrics together with the body baseline.
  private var firstFragment: Option[FragmentMetrics] = None
  override def fragmentMetrics: Option[FragmentMetrics] = firstFragment

  private var pageWidth: Double  = 0
  private var pageHeight: Double = 0
  private var page: SvgPage      = uninitialized

  private var curColor: Color           = Color("black")
  private var curFont: SvgRenderFont    = uninitialized
  private var curLineWidth: Double      = 1.0
  private var curDash: Seq[Double]      = Nil
  private var curDashPhase: Double      = 0
  private var curCap: LineCap           = LineCap.Square
  private var curJoin: LineJoin         = LineJoin.Miter

  private var ctm: SvgMatrix             = SvgMatrix.identity
  private var path: ArrayBuffer[SvgPathCmd] = ArrayBuffer.empty

  // The graphics-state stack. Each saved frame also records how many clip groups (`<g clip-path>`) were opened
  // since the matching gsave, so grestore can close exactly that many `</g>`.
  private case class GState(
      ctm: SvgMatrix,
      color: Color,
      lineWidth: Double,
      dash: Seq[Double],
      dashPhase: Double,
      cap: LineCap,
      join: LineJoin,
  )
  private var gstack: List[GState] = Nil
  private var clipDepth: List[Int] = 0 :: Nil // clip groups opened in the current (innermost) gsave frame

  def init(width: Double, height: Double): Unit =
    pageWidth = width
    pageHeight = height

  def createPageTarget: Any =
    page = new SvgPage(pageWidth, pageHeight)
    ctm = SvgMatrix.identity
    path = ArrayBuffer.empty
    gstack = Nil
    clipDepth = 0 :: Nil
    // paint the page background, so embedded output shows the paper colour rather than the host's backdrop
    page.content
      .append("<rect x=\"0\" y=\"0\" width=\"")
      .append(fmt(pageWidth))
      .append("\" height=\"")
      .append(fmt(pageHeight))
      .append("\" fill=\"")
      .append(cssColor(backgroundColor))
      .append("\"")
      .append(opacityAttr("fill-opacity", backgroundColor))
      .append("/>\n")
    page

  def ejectPageTarget(): Unit =
    // close any clip groups left open, defensively (a well-formed page balances them itself)
    while clipDepth.head > 0 do
      page.content.append("</g>\n")
      clipDepth = (clipDepth.head - 1) :: clipDepth.tail

    // The viewport: the full page, or — when cropping — a tight box around the drawn ink plus a small margin,
    // clamped to the page, so a short fragment renders at its natural size instead of as a shrunken page.
    val (vx, vy, vw, vh) =
      if cropToContent && page.hasInk then
        val x0 = math.max(0.0, page.inkMinX - cropMargin)
        val y0 = math.max(0.0, page.inkMinY - cropMargin)
        val x1 = math.min(pageWidth, page.inkMaxX + cropMargin)
        val y1 = math.min(pageHeight, page.inkMaxY + cropMargin)
        (x0, y0, x1 - x0, y1 - y0)
      else (0.0, 0.0, pageWidth, pageHeight)

    // Record the first cropped page's geometry and where the body baseline falls within it, for fragment
    // rendering (an inline formula aligned on the surrounding text's baseline).
    if firstFragment.isEmpty && cropToContent && page.hasInk then
      firstFragment = Some(FragmentMetrics(vw, vh, bodyBaseline - vy))

    val sb = new StringBuilder
    // Size the element in points (the engine's unit) so the browser renders it at true physical size; the
    // viewBox stays in the same point-valued user space, so a "pt" suffix on width/height is all it takes.
    sb.append("<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"")
      .append(fmt(vw))
      .append("pt\" height=\"")
      .append(fmt(vh))
      .append("pt\" viewBox=\"")
      .append(fmt(vx))
      .append(' ')
      .append(fmt(vy))
      .append(' ')
      .append(fmt(vw))
      .append(' ')
      .append(fmt(vh))
      .append("\">\n")
    if page.defs.nonEmpty then sb.append("<defs>\n").append(page.defs).append("</defs>\n")
    sb.append(page.content)
    sb.append("</svg>\n")
    page.svg = sb.toString

  // ─── font / colour / stroke state ─────────────────────────────────────────────

  def setFont(font: RenderFont): Unit = curFont = font
  def setColor(color: Color): Unit    = curColor = color
  def setLineWidth(width: Double): Unit = curLineWidth = width

  def loadFont(path: String): FontFace = SvgFace(new OtfFont(readFontBytes(path)), path)

  def makeFont(font: FontFace, size: Double): RenderFont = SvgRenderFont(font.font, size)

  /** Read a font file's bytes. On the JVM this reads from disk; the JS layer overrides font loading to supply
    * the bundled bytes it embeds, since a browser has no filesystem. */
  protected def readFontBytes(path: String): Array[Byte] =
    java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(path))

  // ─── metrics ──────────────────────────────────────────────────────────────────

  def charWidth(font: RenderFont, c: Char): Double =
    font.font.advanceWidthEm(font.font.glyphIndex(c.toInt)) * font.size

  def getTextExtents(text: String, font: RenderFont): TextExtents =
    var advance = 0.0
    var minY    = Double.MaxValue
    var maxY    = Double.MinValue
    var minX    = Double.MaxValue
    var maxX    = Double.MinValue
    eachGlyph(text) { cp =>
      val gid = font.font.glyphIndex(cp)
      val (x0, y0, x1, y1) = inkBounds(font.font.outline(gid))
      if x1 >= x0 then
        minX = math.min(minX, advance + x0 * font.size)
        maxX = math.max(maxX, advance + x1 * font.size)
        minY = math.min(minY, y0 * font.size)
        maxY = math.max(maxY, y1 * font.size)
      advance += font.font.advanceWidthEm(gid) * font.size
    }
    if maxY < minY then TextExtents(0, 0, advance, 0, advance, 0) // no ink (e.g. a run of spaces)
    else
      // y-up font space → the engine's convention: yBearing is negative above the baseline
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

  // ─── text & glyph drawing (outline fill) ──────────────────────────────────────

  def drawString(text: String, x: Double, y: Double): Unit =
    if text.isEmpty || curFont == null then return
    val d  = new StringBuilder
    var ax = x
    eachGlyph(text) { cp =>
      val gid = curFont.font.glyphIndex(cp)
      appendGlyphPath(d, curFont.font, gid, ax, y, curFont.size)
      ax += curFont.font.advanceWidthEm(gid) * curFont.size
    }
    if d.nonEmpty then fillPathData(d.toString)

  def drawGlyph(font: RenderFont, glyph: Int, x: Double, y: Double): Unit =
    val d = new StringBuilder
    appendGlyphPath(d, font.font, glyph, x, y, font.size)
    if d.nonEmpty then fillPathData(d.toString)

  /** Append a glyph's outline to `d` as SVG path data, placed at baseline origin `(x, y)`, scaled to `size`,
    * its y-up font space flipped into the page's y-down device space and pushed through the current CTM. */
  private def appendGlyphPath(d: StringBuilder, font: OtfFont, glyph: Int, x: Double, y: Double, size: Double): Unit =
    for seg <- font.outline(glyph) do
      seg match
        case PathSeg.MoveTo(px, py)               => moveCmd(d, ctm(x + px * size, y - py * size))
        case PathSeg.LineTo(px, py)               => lineCmd(d, ctm(x + px * size, y - py * size))
        case PathSeg.CurveTo(x1, y1, x2, y2, px, py) =>
          curveCmd(d, ctm(x + x1 * size, y - y1 * size), ctm(x + x2 * size, y - y2 * size), ctm(x + px * size, y - py * size))
        case PathSeg.Close => d.append("Z")

  private def fillPathData(d: String): Unit =
    page.content
      .append("<path d=\"")
      .append(d)
      .append("\" fill=\"")
      .append(cssColor(curColor))
      .append('"')
      .append(opacityAttr("fill-opacity", curColor))
      .append("/>\n")

  // ─── immediate rules (drawLine / drawRect / fillRect) ─────────────────────────
  //
  // The page draws rules in device space (the CTM is the identity there), but routing the corners through the
  // CTM and emitting a polygon keeps these correct even under a transform, so nothing depends on where they
  // are called from.

  def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit =
    val (ax, ay) = ctm(x1, y1)
    val (bx, by) = ctm(x2, y2)
    page.see(ax, ay); page.see(bx, by)
    page.content
      .append("<line x1=\"").append(fmt(ax)).append("\" y1=\"").append(fmt(ay))
      .append("\" x2=\"").append(fmt(bx)).append("\" y2=\"").append(fmt(by))
      .append("\" stroke=\"").append(cssColor(curColor)).append('"')
      .append(opacityAttr("stroke-opacity", curColor))
      .append(" stroke-width=\"").append(fmt(curLineWidth * ctmScale)).append("\"/>\n")

  def drawRect(x: Double, y: Double, width: Double, height: Double): Unit =
    val pts = rectCorners(x, y, width, height)
    page.content
      .append("<polygon points=\"").append(pts)
      .append("\" fill=\"none\" stroke=\"").append(cssColor(curColor)).append('"')
      .append(opacityAttr("stroke-opacity", curColor))
      .append(" stroke-width=\"").append(fmt(curLineWidth * ctmScale)).append("\"/>\n")

  def fillRect(x: Double, y: Double, width: Double, height: Double): Unit =
    val pts = rectCorners(x, y, width, height)
    page.content
      .append("<polygon points=\"").append(pts)
      .append("\" fill=\"").append(cssColor(curColor)).append('"')
      .append(opacityAttr("fill-opacity", curColor))
      .append("/>\n")

  private def rectCorners(x: Double, y: Double, w: Double, h: Double): String =
    val sb = new StringBuilder
    for (cx, cy) <- Seq((x, y), (x + w, y), (x + w, y + h), (x, y + h)) do
      val (dx, dy) = ctm(cx, cy)
      page.see(dx, dy)
      if sb.nonEmpty then sb.append(' ')
      sb.append(fmt(dx)).append(',').append(fmt(dy))
    sb.toString

  // ─── hyperlinks ───────────────────────────────────────────────────────────────

  override def beginLink(uri: String): Unit =
    page.content.append("<a xlink:href=\"").append(xmlAttr(uri)).append("\">\n")
  override def endLink(): Unit = page.content.append("</a>\n")

  // ─── picture-graphics seam ─────────────────────────────────────────────────────

  override def gsave(): Unit =
    gstack ::= GState(ctm, curColor, curLineWidth, curDash, curDashPhase, curCap, curJoin)
    clipDepth = 0 :: clipDepth

  override def grestore(): Unit =
    // gsave pushes the state and clip frames together, so both pop only on a match: an unbalanced grestore is
    // ignored (as the other backends' state stacks ignore it) instead of underflowing the clip-frame list
    gstack match
      case s :: rest =>
        var opened = clipDepth.head
        while opened > 0 do
          page.content.append("</g>\n")
          opened -= 1
        clipDepth = clipDepth.tail
        gstack = rest
        ctm = s.ctm
        curColor = s.color
        curLineWidth = s.lineWidth
        curDash = s.dash
        curDashPhase = s.dashPhase
        curCap = s.cap
        curJoin = s.join
      case Nil =>

  override def translate(dx: Double, dy: Double): Unit = ctm = ctm * SvgMatrix.translate(dx, dy)
  override def scale(sx: Double, sy: Double): Unit     = ctm = ctm * SvgMatrix.scale(sx, sy)
  override def rotate(radians: Double): Unit           = ctm = ctm * SvgMatrix.rotate(radians)

  override def newPath(): Unit = path = ArrayBuffer.empty
  override def moveTo(x: Double, y: Double): Unit =
    val (dx, dy) = ctm(x, y)
    path += SvgPathCmd.M(dx, dy)
  override def lineTo(x: Double, y: Double): Unit =
    val (dx, dy) = ctm(x, y)
    path += SvgPathCmd.L(dx, dy)
  override def curveTo(c1x: Double, c1y: Double, c2x: Double, c2y: Double, x: Double, y: Double): Unit =
    val (a1, b1) = ctm(c1x, c1y)
    val (a2, b2) = ctm(c2x, c2y)
    val (ax, ay) = ctm(x, y)
    path += SvgPathCmd.C(a1, b1, a2, b2, ax, ay)
  override def closePath(): Unit = path += SvgPathCmd.Z

  // Flatten the arc to cubic Béziers (≤90° per segment) on the same parametric circle Cairo traces, so both
  // backends draw the same curve under the same transform; the standard k = 4/3·tan(Δ/4) handle length.
  override def arc(cx: Double, cy: Double, r: Double, a0: Double, a1: Double, negative: Boolean): Unit =
    var end = a1
    if !negative then while end < a0 do end += 2 * math.Pi
    else while end > a0 do end -= 2 * math.Pi
    val sweep = end - a0
    val nseg  = math.max(1, math.ceil(math.abs(sweep) / (math.Pi / 2)).toInt)
    val delta = sweep / nseg
    val k     = 4.0 / 3.0 * math.tan(delta / 4)
    val sx    = cx + r * math.cos(a0)
    val sy    = cy + r * math.sin(a0)
    if path.isEmpty then moveTo(sx, sy) else lineTo(sx, sy)
    var theta = a0
    var i     = 0
    while i < nseg do
      val t2  = theta + delta
      val p2x = cx + r * math.cos(t2)
      val p2y = cy + r * math.sin(t2)
      curveTo(
        cx + r * math.cos(theta) - k * r * math.sin(theta),
        cy + r * math.sin(theta) + k * r * math.cos(theta),
        p2x + k * r * math.sin(t2),
        p2y - k * r * math.cos(t2),
        p2x,
        p2y,
      )
      theta = t2
      i += 1

  override def fillPath(preserve: Boolean): Unit =
    page.content
      .append("<path d=\"").append(pathData())
      .append("\" fill=\"").append(cssColor(curColor)).append('"')
      .append(opacityAttr("fill-opacity", curColor))
      .append("/>\n")
    if !preserve then path = ArrayBuffer.empty

  override def strokePath(): Unit =
    // the pen takes the CTM at stroke time, as Cairo's does
    val s = ctmScale
    page.content
      .append("<path d=\"").append(pathData())
      .append("\" fill=\"none\" stroke=\"").append(cssColor(curColor)).append('"')
      .append(opacityAttr("stroke-opacity", curColor))
      .append(" stroke-width=\"").append(fmt(curLineWidth * s)).append('"')
      .append(strokeAttrs(s))
      .append("/>\n")
    path = ArrayBuffer.empty

  override def clipPath(): Unit =
    val id = s"clip${page.clipCounter}"
    page.clipCounter += 1
    page.defs.append("<clipPath id=\"").append(id).append("\"><path d=\"").append(pathData()).append("\"/></clipPath>\n")
    page.content.append("<g clip-path=\"url(#").append(id).append(")\">\n")
    clipDepth = (clipDepth.head + 1) :: clipDepth.tail
    path = ArrayBuffer.empty

  override def setDash(pattern: Seq[Double], offset: Double): Unit =
    curDash = pattern
    curDashPhase = offset
  override def setLineCap(cap: LineCap): Unit   = curCap = cap
  override def setLineJoin(join: LineJoin): Unit = curJoin = join

  /** The current path as SVG path data. The stored coordinates are already device space (frozen through the
    * CTM when each segment was added), so they are emitted as they are. */
  private def pathData(): String =
    val d = new StringBuilder
    for cmd <- path do
      cmd match
        case SvgPathCmd.M(x, y)                 => moveCmd(d, (x, y))
        case SvgPathCmd.L(x, y)                 => lineCmd(d, (x, y))
        case SvgPathCmd.C(x1, y1, x2, y2, x, y) => curveCmd(d, (x1, y1), (x2, y2), (x, y))
        case SvgPathCmd.Z                       => d.append("Z")
    d.toString

  /** The CTM's mean linear scale — the factor a rasterizer applies to the pen when stroking under this
    * transform. Path geometry is emitted in device space, so the scalar stroke width and dash lengths must be
    * scaled the same way or strokes render thinner than on the PDF and raster backends. Exact for uniform
    * scale and rotation; the geometric mean for an anisotropic scale, whose elliptical pen SVG's scalar
    * stroke-width cannot represent. */
  private def ctmScale: Double = math.sqrt(math.abs(ctm.a * ctm.d - ctm.b * ctm.c))

  private def strokeAttrs(scale: Double): String =
    val sb = new StringBuilder
    if curDash.nonEmpty then
      // dash lengths live in user space like the pen width, so they carry the same CTM scale
      sb.append(" stroke-dasharray=\"")
      for (v, i) <- curDash.zipWithIndex do
        if i > 0 then sb.append(',')
        sb.append(fmt(v * scale))
      sb.append('"')
      if curDashPhase != 0 then sb.append(" stroke-dashoffset=\"").append(fmt(curDashPhase * scale)).append('"')
    curCap match
      case LineCap.Butt   => sb.append(" stroke-linecap=\"butt\"")
      case LineCap.Round  => sb.append(" stroke-linecap=\"round\"")
      case LineCap.Square => sb.append(" stroke-linecap=\"square\"")
    curJoin match
      case LineJoin.Miter => sb.append(" stroke-linejoin=\"miter\"")
      case LineJoin.Round  => sb.append(" stroke-linejoin=\"round\"")
      case LineJoin.Bevel  => sb.append(" stroke-linejoin=\"bevel\"")
    sb.toString

  // ─── images (not yet supported on this backend) ───────────────────────────────

  def loadImage(path: String): (ImageHandle, Int, Int) =
    throw new UnsupportedOperationException("the SVG backend does not support raster images yet")
  def drawImage(image: ImageHandle, x: Double, y: Double, w: Double, h: Double): Unit = ()

  def destroy(): Unit = ()

  /** The SVG document of each shipped page, in order — what a host writes to `.svg` files or parses into the
    * DOM. Valid after the document has been laid out and ejected. */
  def pageSvgs: Seq[String] = getDocument.printedPages.toSeq.collect { case p: SvgPage => p.svg }

  // ─── helpers ────────────────────────────────────────────────────────────────

  private def moveCmd(d: StringBuilder, p: (Double, Double)): Unit =
    page.see(p._1, p._2)
    d.append("M").append(fmt(p._1)).append(' ').append(fmt(p._2)).append(' ')
  private def lineCmd(d: StringBuilder, p: (Double, Double)): Unit =
    page.see(p._1, p._2)
    d.append("L").append(fmt(p._1)).append(' ').append(fmt(p._2)).append(' ')
  private def curveCmd(d: StringBuilder, c1: (Double, Double), c2: (Double, Double), e: (Double, Double)): Unit =
    page.see(c1._1, c1._2); page.see(c2._1, c2._2); page.see(e._1, e._2)
    d.append("C").append(fmt(c1._1)).append(' ').append(fmt(c1._2)).append(' ')
      .append(fmt(c2._1)).append(' ').append(fmt(c2._2)).append(' ')
      .append(fmt(e._1)).append(' ').append(fmt(e._2)).append(' ')

  /** Iterate the Unicode codepoints of `text` (surrogate pairs combined), calling `f` for each. */
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

  /** The em-space ink bounding box (x0, y0, x1, y1, y up) of an outline, sampling each cubic so the box is
    * tight. Returns an inverted (empty) box for a glyph with no outline. */
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
          var i = 1
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
    f"#${c.redInt}%02x${c.greenInt}%02x${c.blueInt}%02x"

  private def opacityAttr(name: String, c: Color): String =
    if c.alpha >= 1.0 then "" else s""" $name="${fmt(c.alpha)}""""

  private def xmlAttr(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")

  /** Format a coordinate compactly and locale-independently: at most three decimals, trailing zeros trimmed. */
  private def fmt(v: Double): String =
    if v.isNaN || v.isInfinite || math.abs(v) < 5e-4 then "0" // rounds to 0 at three decimals; also dodges BigDecimal's zero-strip quirk
    else
      val s = BigDecimal(v).setScale(3, BigDecimal.RoundingMode.HALF_UP).bigDecimal.stripTrailingZeros.toPlainString
      if s == "-0" then "0" else s

end SvgTypesetter
