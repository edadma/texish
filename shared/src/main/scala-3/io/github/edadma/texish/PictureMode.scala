package io.github.edadma.texish

import scala.collection.mutable.ArrayBuffer

/** Picture mode: the drawing context a `\picture{…}{ body }` opens, parallel to [[MathMode]] for `$…$`. As the
  * body parses, the language layer's drawing primitives call the methods here, which append to an ordered
  * display list of [[PictureOp]]s; [[result]] freezes that list into a [[PictureBox]] of the picture's declared
  * size. Coordinates are picture-local (y-up, origin bottom-left); the box applies the device transform.
  *
  * The model is immediate-mode-with-state. Two kinds of state live here at collection time:
  *
  *   - the **fill and stroke colours**, which are baked into each shape's [[PictureOp.Paint]] as it is issued —
  *     so a later `\stroke`/`\fill` change cannot retroactively recolour an already-drawn shape; and
  *   - the **running current point**, the end of the last path segment, against which a relative coordinate is
  *     resolved (see [[rel]]).
  *
  * Everything else — line width, dash, caps, joins, and the coordinate transform — is backend state set by its
  * own op and unwound by [[groupEnd]]'s [[PictureOp.GRestore]]. [[groupBegin]]/[[groupEnd]] additionally save
  * and restore the collection-time state above, so a `\group` is a complete save/restore of the drawing state.
  */
class PictureMode(val t: Typesetter, val width: Double, val height: Double) extends Mode:
  private val ops = ArrayBuffer[PictureOp]()

  private var fillColour:   Option[Color] = None
  private var strokeColour: Option[Color] = None

  private var fillOpacity:   Double = 1.0
  private var strokeOpacity: Double = 1.0

  private var curX: Double = 0
  private var curY: Double = 0

  // Tangent tracking for `\path` arrowheads. While `tracking` is on (a `\path` carrying an arrow: option), each
  // segment records where the path starts and ends and the travel direction at each end, so an arrowhead can be
  // oriented to the path's true tangent — for a Bézier the end tangent is the last control point → endpoint, for an
  // arc it is perpendicular to the end radius — rather than to the straight start→end chord. Direction magnitudes
  // are arbitrary (the arrowhead normalises them).
  private var tracking = false
  private var pathStart:    Option[(Double, Double)] = None
  private var pathStartDir: Option[(Double, Double)] = None
  private var pathEnd:      Option[(Double, Double)] = None
  private var pathEndDir:   Option[(Double, Double)] = None

  private def nonZero(v: (Double, Double)): Boolean = math.hypot(v._1, v._2) > 1e-9

  private def firstNonZero(vs: (Double, Double)*): (Double, Double) = vs.find(nonZero).getOrElse(vs.last)

  // Record a segment running from the current point to (endX, endY), with travel direction `outDir` leaving its
  // start and `inDir` arriving at its end. Only the first segment's outgoing direction fixes the path's start
  // tangent; every segment updates the end. A degenerate (zero-length) direction does not clobber a good one.
  private def trackSegment(outDir: (Double, Double), inDir: (Double, Double), endX: Double, endY: Double): Unit =
    if tracking then
      if pathStart.isEmpty then pathStart = Some((curX, curY))
      if pathStartDir.isEmpty && nonZero(outDir) then pathStartDir = Some(outDir)
      if nonZero(inDir) then pathEndDir = Some(inDir)
      pathEnd = Some((endX, endY))

  /** Collection-time state a `\group` saves and restores: the active colours, opacities, and the current point.
    * The transform and stroke parameters are restored by the backend's own gstate, so they are not kept here. */
  private case class GState(
      fill: Option[Color],
      stroke: Option[Color],
      fillOp: Double,
      strokeOp: Double,
      x: Double,
      y: Double,
  )
  private val gstack = ArrayBuffer[GState]()

  def init(): Unit = ()

  private def emit(op: PictureOp): Unit = ops += op

  /** The display list collected so far, for introspection and testing. */
  def displayList: Vector[PictureOp] = ops.toVector

  /** The end of the last path segment, the origin for a relative coordinate. */
  def currentPoint: (Double, Double) = (curX, curY)

  /** Move the running current point. The coordinate reader calls this as it resolves the coordinates of a single
    * command, so a `++` relative coordinate later in the same group measures from the one before it (path segments
    * set it too, via [[moveTo]] / [[lineTo]], for `++` chained across commands). */
  def setCurrentPoint(x: Double, y: Double): Unit =
    curX = x; curY = y

  /** Resolve a coordinate that may be relative: a relative `(dx, dy)` is measured from the current point, an
    * absolute one is taken as-is. The TikZ `++` form lands here once the parser has split off the marker. */
  def rel(x: Double, y: Double, relative: Boolean): (Double, Double) =
    if relative then (curX + x, curY + y) else (x, y)

  // ─── drawing state ──────────────────────────────────────────────────────────

  def setStroke(c: Color): Unit = strokeColour = Some(c)
  def noStroke(): Unit          = strokeColour = None
  def setFill(c: Color): Unit   = fillColour = Some(c)
  def noFill(): Unit            = fillColour = None

  /** Fill / stroke opacity, a multiplier on the colour's own alpha applied when a shape is painted. `\opacity`
    * sets both; `\fillopacity` / `\strokeopacity` set one. Translucent fills let overlapping shapes — bubbles,
    * confidence bands — show through one another. Clamped to [0, 1]. */
  def setFillOpacity(v: Double): Unit   = fillOpacity = v.max(0.0).min(1.0)
  def setStrokeOpacity(v: Double): Unit = strokeOpacity = v.max(0.0).min(1.0)

  private def withOpacity(c: Color, op: Double): Color = c.copy(alpha = c.alpha * op)

  /** The active fill / stroke colour, with the current opacity folded in, for a `\glyph` drawing in the
    * picture's current ink. */
  def fillColor: Option[Color]   = fillColour.map(withOpacity(_, fillOpacity))
  def strokeColor: Option[Color] = strokeColour.map(withOpacity(_, strokeOpacity))

  /** The ink for a mark the drawing command colours itself — an arrowhead, a placed glyph — rather than one the
    * picture's fill/stroke state paints through [[paint]]. The two axes are consulted in the order that suits
    * the mark: an arrow is a stroke that happens to end in a filled head, a glyph is a fill. With neither set,
    * the mark takes the pen in force where the picture sits, so a picture that names no colour of its own draws
    * in the document's ink — and follows it into a dark scheme — rather than in a fixed black. */
  def strokeInk: Color = strokeColor.orElse(fillColor).getOrElse(t.currentColor)
  def fillInk: Color   = fillColor.orElse(strokeColor).getOrElse(t.currentColor)

  /** The most recently set stroke width, so a `\path` arrowhead can push its tip far enough past the path end to
    * cover the stroke's end cap (the stroke runs to the exact endpoint, where a pointed head is infinitely thin). */
  def lineWidth: Double = lineWidthValue
  private var lineWidthValue: Double = 1.0

  def setLineWidth(w: Double): Unit               = { lineWidthValue = w; emit(PictureOp.SetLineWidth(w)) }
  def setDash(pattern: Vector[Double], offset: Double): Unit = emit(PictureOp.SetDash(pattern, offset))
  def setLineCap(cap: LineCap): Unit              = emit(PictureOp.SetLineCap(cap))
  def setLineJoin(join: LineJoin): Unit           = emit(PictureOp.SetLineJoin(join))

  // ─── transforms ───────────────────────────────────────────────────────────────

  def translate(dx: Double, dy: Double): Unit = emit(PictureOp.Translate(dx, dy))
  def scale(sx: Double, sy: Double): Unit     = emit(PictureOp.Scale(sx, sy))
  def rotate(radians: Double): Unit           = emit(PictureOp.Rotate(radians))

  // ─── grouping and clipping ──────────────────────────────────────────────────────

  /** Open a `\group`: snapshot the collection-time state and save the backend graphics state. */
  def groupBegin(): Unit =
    gstack += GState(fillColour, strokeColour, fillOpacity, strokeOpacity, curX, curY)
    emit(PictureOp.GSave)

  /** Close a `\group`: restore the backend graphics state and the snapshotted collection-time state. */
  def groupEnd(): Unit =
    emit(PictureOp.GRestore)
    val s = gstack.remove(gstack.length - 1)
    fillColour = s.fill
    strokeColour = s.stroke
    fillOpacity = s.fillOp
    strokeOpacity = s.strokeOp
    curX = s.x
    curY = s.y

  /** Intersect the clip region with the current path, for the rest of the enclosing group. The path is built
    * by the preceding [[newPath]]/segment calls, exactly as a `\path` is. */
  def clip(): Unit = emit(PictureOp.Clip)

  // ─── path building ──────────────────────────────────────────────────────────────

  /** Begin tracking the path being built so its endpoints' tangents can orient arrowheads; resets the recorded
    * anchors. Called by `\path` when it carries an arrow: option. */
  def beginTrackedPath(): Unit =
    tracking = true
    pathStart = None; pathStartDir = None; pathEnd = None; pathEndDir = None

  /** Stop tracking. Called before any arrowheads are drawn, so their own path segments are not recorded. */
  def endTrackedPath(): Unit = tracking = false

  /** The path's end and the incoming travel direction there, for an end arrowhead; None if it drew no segments. */
  def pathEndAnchor: Option[((Double, Double), (Double, Double))] =
    for { e <- pathEnd; d <- pathEndDir } yield (e, d)

  /** The path's start and the outgoing travel direction there, for a start arrowhead (which points backwards). */
  def pathStartAnchor: Option[((Double, Double), (Double, Double))] =
    for { s <- pathStart; d <- pathStartDir } yield (s, d)

  def newPath(): Unit = emit(PictureOp.NewPath)

  def moveTo(x: Double, y: Double): Unit =
    if tracking && pathStart.isEmpty then pathStart = Some((x, y)) // the first moveTo fixes the path's start point
    emit(PictureOp.MoveTo(x, y)); curX = x; curY = y

  def lineTo(x: Double, y: Double): Unit =
    val dir = (x - curX, y - curY)
    trackSegment(dir, dir, x, y)
    emit(PictureOp.LineTo(x, y)); curX = x; curY = y

  def curveTo(c1x: Double, c1y: Double, c2x: Double, c2y: Double, x: Double, y: Double): Unit =
    // The Bézier leaves its start toward the first control point and arrives at its end from the last control point;
    // a control point coincident with its endpoint falls through to the next-best direction.
    val outDir = firstNonZero((c1x - curX, c1y - curY), (c2x - curX, c2y - curY), (x - curX, y - curY))
    val inDir  = firstNonZero((x - c2x, y - c2y), (x - c1x, y - c1y), (x - curX, y - curY))
    trackSegment(outDir, inDir, x, y)
    emit(PictureOp.CurveTo(c1x, c1y, c2x, c2y, x, y)); curX = x; curY = y

  def arc(cx: Double, cy: Double, r: Double, a0: Double, a1: Double, negative: Boolean): Unit =
    if tracking then
      // The arc's tangent is perpendicular to its radius; travel is counter-clockwise unless `negative`.
      val startTan = if negative then (math.sin(a0), -math.cos(a0)) else (-math.sin(a0), math.cos(a0))
      val endTan   = if negative then (math.sin(a1), -math.cos(a1)) else (-math.sin(a1), math.cos(a1))
      if pathStart.isEmpty then pathStart = Some((cx + r * math.cos(a0), cy + r * math.sin(a0)))
      if pathStartDir.isEmpty then pathStartDir = Some(startTan)
      pathEndDir = Some(endTan)
      pathEnd = Some((cx + r * math.cos(a1), cy + r * math.sin(a1)))
    emit(PictureOp.Arc(cx, cy, r, a0, a1, negative))
    curX = cx + r * math.cos(a1)
    curY = cy + r * math.sin(a1)

  def close(): Unit = emit(PictureOp.Close)

  /** Paint the path built so far with the active fill and/or stroke colours, each scaled by its opacity — the
    * end of a `\path` or a shape. */
  def paint(): Unit =
    emit(PictureOp.Paint(fillColour.map(withOpacity(_, fillOpacity)), strokeColour.map(withOpacity(_, strokeOpacity))))

  /** Paint the current path with explicit colours, bypassing the mode's active fill/stroke. For composite shapes
    * like an arrowhead, whose ink the drawing command chooses (the pen colour, not the picture's `\fill`), rather
    * than the shape inheriting the picture's fill/stroke state. */
  def paintWith(fill: Option[Color], stroke: Option[Color]): Unit = emit(PictureOp.Paint(fill, stroke))

  // ─── shapes (lower to a fresh path plus one paint) ───────────────────────────────

  def line(x1: Double, y1: Double, x2: Double, y2: Double): Unit =
    newPath(); moveTo(x1, y1); lineTo(x2, y2); paint()

  def rect(x: Double, y: Double, w: Double, h: Double): Unit =
    newPath(); moveTo(x, y); lineTo(x + w, y); lineTo(x + w, y + h); lineTo(x, y + h); close(); paint()

  def circle(cx: Double, cy: Double, r: Double): Unit =
    newPath(); arc(cx, cy, r, 0, 2 * math.Pi, false); close(); paint()

  /** An axis-aligned ellipse, drawn as four cubic Béziers so the stroke keeps a uniform width (scaling a circle
    * would distort the pen). `kappa` is the standard control-handle length for a quarter ellipse. */
  def ellipse(cx: Double, cy: Double, rx: Double, ry: Double): Unit =
    val k  = 0.5522847498307936
    val kx = rx * k
    val ky = ry * k
    newPath()
    moveTo(cx + rx, cy)
    curveTo(cx + rx, cy + ky, cx + kx, cy + ry, cx, cy + ry)
    curveTo(cx - kx, cy + ry, cx - rx, cy + ky, cx - rx, cy)
    curveTo(cx - rx, cy - ky, cx - kx, cy - ry, cx, cy - ry)
    curveTo(cx + kx, cy - ry, cx + rx, cy - ky, cx + rx, cy)
    close(); paint()

  def polygon(points: Vector[(Double, Double)]): Unit =
    if points.nonEmpty then
      newPath()
      moveTo(points.head._1, points.head._2)
      points.tail.foreach((px, py) => lineTo(px, py))
      close(); paint()

  def polyline(points: Vector[(Double, Double)]): Unit =
    if points.nonEmpty then
      newPath()
      moveTo(points.head._1, points.head._2)
      points.tail.foreach((px, py) => lineTo(px, py))
      paint()

  def arcShape(cx: Double, cy: Double, r: Double, a0: Double, a1: Double, negative: Boolean): Unit =
    newPath(); arc(cx, cy, r, a0, a1, negative); paint()

  // ─── placement ──────────────────────────────────────────────────────────────────

  /** Place an already-laid-out box (`\at` content, or a `\glyph`'s [[GlyphBox]]) at a coordinate with an
    * anchor. The box draws upright regardless of the picture's y flip. */
  def place(box: Box, anchor: Anchor, x: Double, y: Double): Unit =
    emit(PictureOp.Place(box, anchor, x, y))

  /** A box produced by ordinary content inside the picture body drops in at the current point on its baseline. */
  infix def add(box: Box): Unit = emit(PictureOp.Place(box, Anchor.Baseline, curX, curY))

  def result: Box | Null = new PictureBox(t, width, height, ops.toVector)
