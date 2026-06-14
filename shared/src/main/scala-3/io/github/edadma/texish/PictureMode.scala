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

  private var curX: Double = 0
  private var curY: Double = 0

  /** Collection-time state a `\group` saves and restores: the active colours and the current point. The
    * transform and stroke parameters are restored by the backend's own gstate, so they are not kept here. */
  private case class GState(fill: Option[Color], stroke: Option[Color], x: Double, y: Double)
  private val gstack = ArrayBuffer[GState]()

  def init(): Unit = ()

  private def emit(op: PictureOp): Unit = ops += op

  /** The display list collected so far, for introspection and testing. */
  def displayList: Vector[PictureOp] = ops.toVector

  /** The end of the last path segment, the origin for a relative coordinate. */
  def currentPoint: (Double, Double) = (curX, curY)

  /** Resolve a coordinate that may be relative: a relative `(dx, dy)` is measured from the current point, an
    * absolute one is taken as-is. The TikZ `++` form lands here once the parser has split off the marker. */
  def rel(x: Double, y: Double, relative: Boolean): (Double, Double) =
    if relative then (curX + x, curY + y) else (x, y)

  // ─── drawing state ──────────────────────────────────────────────────────────

  def setStroke(c: Color): Unit = strokeColour = Some(c)
  def noStroke(): Unit          = strokeColour = None
  def setFill(c: Color): Unit   = fillColour = Some(c)
  def noFill(): Unit            = fillColour = None

  def setLineWidth(w: Double): Unit               = emit(PictureOp.SetLineWidth(w))
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
    gstack += GState(fillColour, strokeColour, curX, curY)
    emit(PictureOp.GSave)

  /** Close a `\group`: restore the backend graphics state and the snapshotted collection-time state. */
  def groupEnd(): Unit =
    emit(PictureOp.GRestore)
    val s = gstack.remove(gstack.length - 1)
    fillColour = s.fill
    strokeColour = s.stroke
    curX = s.x
    curY = s.y

  /** Intersect the clip region with the current path, for the rest of the enclosing group. The path is built
    * by the preceding [[newPath]]/segment calls, exactly as a `\path` is. */
  def clip(): Unit = emit(PictureOp.Clip)

  // ─── path building ──────────────────────────────────────────────────────────────

  def newPath(): Unit = emit(PictureOp.NewPath)

  def moveTo(x: Double, y: Double): Unit =
    emit(PictureOp.MoveTo(x, y)); curX = x; curY = y

  def lineTo(x: Double, y: Double): Unit =
    emit(PictureOp.LineTo(x, y)); curX = x; curY = y

  def curveTo(c1x: Double, c1y: Double, c2x: Double, c2y: Double, x: Double, y: Double): Unit =
    emit(PictureOp.CurveTo(c1x, c1y, c2x, c2y, x, y)); curX = x; curY = y

  def arc(cx: Double, cy: Double, r: Double, a0: Double, a1: Double, negative: Boolean): Unit =
    emit(PictureOp.Arc(cx, cy, r, a0, a1, negative))
    curX = cx + r * math.cos(a1)
    curY = cy + r * math.sin(a1)

  def close(): Unit = emit(PictureOp.Close)

  /** Paint the path built so far with the active fill and/or stroke colours — the end of a `\path` or a shape. */
  def paint(): Unit = emit(PictureOp.Paint(fillColour, strokeColour))

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
