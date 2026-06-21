package io.github.edadma.texish

/** A box whose inner content is drawn under a linear transform — rotation, scaling, or reflection — applied
  * about the inner box's reference point (its left baseline). The reported metrics are the axis-aligned bounding
  * box of the transformed content, so surrounding material flows around the rotated or scaled result. This box
  * backs \rotatebox, \scalebox, \reflectbox and \resizebox.
  *
  * The 2×2 matrix maps an inner point `(px, py)` in device space (y increasing downward, measured from the
  * reference point) to `(m00*px + m01*py, m10*px + m11*py)`. The same transform is replayed on the backend by
  * `applyXform`, which issues the equivalent `scale`/`rotate` calls, so the drawn result matches the bounds
  * computed here. Drawing is bracketed by `gsave`/`grestore`, and a leading `translate` shifts the transformed
  * content so its left/top edges line up with the box's own origin.
  */
class TransformBox(
    inner: Box,
    m00: Double,
    m01: Double,
    m10: Double,
    m11: Double,
    applyXform: Typesetter => Unit,
) extends ContentBox:

  // The inner content occupies x in [0, width] and (device) y in [-ascent, descent] about the reference point.
  // Transforming the four corners and taking their extent gives the bounding box of the transformed content.
  private val corners =
    for cx <- Seq(0.0, inner.width); cy <- Seq(-inner.ascent, inner.descent)
    yield (m00 * cx + m01 * cy, m10 * cx + m11 * cy)
  private val minX = corners.map(_._1).min
  private val maxX = corners.map(_._1).max
  private val minY = corners.map(_._2).min
  private val maxY = corners.map(_._2).max

  val width: Double    = maxX - minX
  val ascent: Double   = math.max(0.0, -minY)
  val descent: Double  = math.max(0.0, maxY)
  val xAdvance: Double = width

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    t.gsave()
    t.translate(x - minX, y) // line the transformed content's left edge up with the box origin, baseline at y
    applyXform(t)
    inner.draw(t, 0, 0)
    t.grestore()

  override def toString: String =
    s"TransformBox(m=($m00,$m01,$m10,$m11), inner=$inner)"
