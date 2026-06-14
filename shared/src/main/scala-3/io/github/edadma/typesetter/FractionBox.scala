package io.github.edadma.typesetter

/** The font metrics that govern a fraction's layout, in points, read from the OpenType MATH constants (the
  * display-style variants when a fraction is set in display style) or TeX's Computer Modern values when the
  * font carries no MATH table. The fraction bar sits on the math axis; the gaps keep the numerator and
  * denominator clear of it, and the shifts are the nominal baseline offsets the gaps raise when needed.
  */
case class FractionParams(
    axisHeight:    Double,
    ruleThickness: Double,
    numGapMin:     Double,
    denomGapMin:   Double,
    numShiftUp:    Double,
    denomShiftDown: Double,
)

object FractionParams:
  /** TeX's Computer Modern fraction parameters as fractions of an em, scaled to a font size; the display
    * forms open the gaps and shifts up. Used only when the math font has no MATH table. */
  def texDefaults(size: Double, display: Boolean): FractionParams =
    val theta = 0.04 * size
    FractionParams(
      axisHeight     = 0.25 * size,
      ruleThickness  = theta,
      numGapMin      = (if display then 3 * theta else theta),
      denomGapMin    = (if display then 3 * theta else theta),
      numShiftUp     = (if display then 0.677 else 0.394) * size,
      denomShiftDown = (if display then 0.686 else 0.345) * size,
    )

/** A built fraction: a numerator over a denominator, separated by a rule centered on the math axis. The
  * numerator is raised and the denominator lowered just enough that each clears the rule by the font's
  * minimum gap, and both are centered horizontally over the wider of the two. The box's baseline is the
  * surrounding line's baseline, so the whole fraction sits centered on the axis, exactly as TeX builds it.
  */
class FractionBox(t: Typesetter, numerator: Box, denominator: Box, p: FractionParams) extends ContentBox:

  val width: Double = math.max(numerator.width, denominator.width)

  private val half = p.ruleThickness / 2

  // raise the numerator so its bottom clears the top of the rule by at least the minimum gap, but never less
  // than the nominal shift; lower the denominator likewise below the rule
  private val shiftUp: Double =
    math.max(p.numShiftUp, p.axisHeight + half + p.numGapMin + numerator.descent)
  private val shiftDown: Double =
    math.max(p.denomShiftDown, p.denomGapMin + half - p.axisHeight + denominator.ascent)

  val ascent: Double   = shiftUp + numerator.ascent
  val descent: Double  = shiftDown + denominator.descent
  val xAdvance: Double = width

  private val rule = RuleBox(t, width, p.ruleThickness, 0)

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    numerator.draw(t, x + (width - numerator.width) / 2, y - shiftUp)
    if p.ruleThickness > 0 then rule.draw(t, x, y - p.axisHeight + half)
    denominator.draw(t, x + (width - denominator.width) / 2, y + shiftDown)

  override def toString: String =
    s"FractionBox(num=$numerator, denom=$denominator, shiftUp=$shiftUp, shiftDown=$shiftDown)"
