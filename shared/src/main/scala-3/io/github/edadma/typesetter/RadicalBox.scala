package io.github.edadma.typesetter

/** The font metrics that govern a radical's layout, in points: the thickness of the vinculum (the overbar),
  * the vertical gap kept between the top of the radicand and that bar (wider in display style), and a little
  * extra ascender above the bar. From the OpenType MATH constants when present, otherwise TeX's Computer
  * Modern values.
  */
case class RadicalParams(
    ruleThickness:    Double,
    verticalGap:      Double,
    extraAscender:    Double,
    degreeRaise:      Double, // fraction of the surd's height the degree's bottom is raised by
    kernBeforeDegree: Double,
    kernAfterDegree:  Double,
)

object RadicalParams:
  def texDefaults(size: Double, display: Boolean): RadicalParams =
    val theta = 0.045 * size
    RadicalParams(
      ruleThickness    = theta,
      verticalGap      = (if display then 0.15 else 0.06) * size,
      extraAscender    = theta,
      degreeRaise      = 0.6,
      kernBeforeDegree = 5.0 / 18.0 * size,   // plain TeX's \mkern5mu before the index
      kernAfterDegree  = -10.0 / 18.0 * size, // and \mkern-10mu after, tucking it over the surd
    )

/** A built radical: a surd glyph on the left and the radicand to its right, with a vinculum (overbar) drawn
  * from the surd's top across the radicand, the font's gap of clearance above the radicand. The surd glyph
  * is chosen tall enough to span the radicand; when the available glyph is taller than strictly needed, the
  * excess is split evenly above and below — raising the bar and letting the surd dip below the baseline —
  * exactly as TeXbook rule 11 distributes it, so the radicand stays centred in the surd.
  *
  * An optional `degree` is the small index of a higher root (`\sqrt[3]{…}`): set in scriptscript style and
  * tucked into the surd's left kink, its bottom raised by a fraction of the surd's height, with the font's
  * kerns before and after positioning it horizontally over the stem.
  */
class RadicalBox(t: Typesetter, surd: Box, radicand: Box, p: RadicalParams, degree: Option[Box] = None)
    extends ContentBox:

  // the surd should cover the radicand plus the gap and the bar; any height beyond that is shared above/below
  private val needed = p.verticalGap + p.ruleThickness + radicand.ascent + radicand.descent
  private val excess = math.max(0.0, surd.height - needed)

  private val gap        = p.verticalGap + excess / 2     // raise the bar by half the surplus
  private val barBottom  = radicand.ascent + gap
  private val barTop     = barBottom + p.ruleThickness

  // how far the surd is shifted right to make room for a degree, and the degree's own baseline offset (its
  // distance below the line; negative means above the baseline, where the index normally sits)
  private val surdOffset: Double = degree match
    case Some(d) => math.max(0.0, p.kernBeforeDegree + d.width + p.kernAfterDegree)
    case None    => 0.0

  private val degreeBaseline: Double = degree match
    case Some(d) => surd.height - barTop - p.degreeRaise * surd.height - d.descent // below baseline (signed)
    case None    => 0.0

  val width: Double = degree match
    case Some(d) => math.max(p.kernBeforeDegree + d.width, surdOffset + surd.width + radicand.width)
    case None    => surd.width + radicand.width
  val xAdvance: Double = width

  val ascent: Double =
    val base = barTop + p.extraAscender
    degree match
      case Some(d) => math.max(base, -degreeBaseline + d.ascent) // the index may rise above the vinculum
      case None    => base

  // the surd's bottom, placed so its top meets the bar, sits the other half of the surplus below the radicand
  val descent: Double =
    val base = math.max(radicand.descent, surd.height - barTop)
    degree match
      case Some(d) => math.max(base, degreeBaseline + d.descent) // normally negative — the index is up high
      case None    => base

  private val rule = RuleBox(t, radicand.width, p.ruleThickness, 0)

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    degree.foreach(d => d.draw(t, x + p.kernBeforeDegree, y + degreeBaseline))
    surd.draw(t, x + surdOffset, y - barTop + surd.ascent) // surd top aligned with the top of the bar
    rule.draw(t, x + surdOffset + surd.width, y - barBottom)
    radicand.draw(t, x + surdOffset + surd.width, y)

  override def toString: String = s"RadicalBox(surd=$surd, radicand=$radicand, barTop=$barTop, degree=$degree)"
