package io.github.edadma.typesetter

/** The font metrics that govern limits set above and below a large operator, in points: the minimum gap and
  * the minimum baseline rise/drop for each limit. From the OpenType MATH constants when present, otherwise
  * TeX's Computer Modern `big_op_spacing` values.
  */
case class LimitsParams(
    upperGapMin:         Double,
    upperBaselineRiseMin: Double,
    lowerGapMin:         Double,
    lowerBaselineDropMin: Double,
)

object LimitsParams:
  def texDefaults(size: Double): LimitsParams = LimitsParams(
    upperGapMin          = 0.2 * size,  // big_op_spacing1/3 territory
    upperBaselineRiseMin = 0.4 * size,
    lowerGapMin          = 0.2 * size,
    lowerBaselineDropMin = 0.6 * size,
  )

/** A large operator (`\sum`, `\prod`, `\bigcup`, `\lim`) with its scripts set as limits — centred above and
  * below the operator rather than to its side. Each limit is placed so it clears the operator by at least the
  * font's minimum gap and sits at least the minimum baseline rise/drop away, all three boxes centred on the
  * widest. This is how operators set in display style (and inline under `\limits`) carry their bounds.
  */
class LimitsBox(t: Typesetter, op: Box, upper: Option[Box], lower: Option[Box], p: LimitsParams) extends ContentBox:

  private val upperRise: Double =
    upper.map(u => math.max(p.upperBaselineRiseMin, p.upperGapMin + u.descent)).getOrElse(0.0)
  private val lowerDrop: Double =
    lower.map(l => math.max(p.lowerBaselineDropMin, p.lowerGapMin + l.ascent)).getOrElse(0.0)

  val width: Double =
    math.max(op.width, math.max(upper.map(_.width).getOrElse(0.0), lower.map(_.width).getOrElse(0.0)))
  val xAdvance: Double = width

  val ascent: Double  = op.ascent + upper.map(upperRise + _.ascent).getOrElse(0.0)
  val descent: Double = op.descent + lower.map(lowerDrop + _.descent).getOrElse(0.0)

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    op.draw(t, x + (width - op.width) / 2, y)
    upper.foreach(u => u.draw(t, x + (width - u.width) / 2, y - op.ascent - upperRise))
    lower.foreach(l => l.draw(t, x + (width - l.width) / 2, y + op.descent + lowerDrop))

  override def toString: String = s"LimitsBox(op=$op, upper=$upper, lower=$lower)"
