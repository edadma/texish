package io.github.edadma.typesetter

/** The font metrics that govern a radical's layout, in points: the thickness of the vinculum (the overbar),
  * the vertical gap kept between the top of the radicand and that bar (wider in display style), and a little
  * extra ascender above the bar. From the OpenType MATH constants when present, otherwise TeX's Computer
  * Modern values.
  */
case class RadicalParams(ruleThickness: Double, verticalGap: Double, extraAscender: Double)

object RadicalParams:
  def texDefaults(size: Double, display: Boolean): RadicalParams =
    val theta = 0.045 * size
    RadicalParams(
      ruleThickness = theta,
      verticalGap   = (if display then 0.15 else 0.06) * size,
      extraAscender = theta,
    )

/** A built radical: a surd glyph on the left and the radicand to its right, with a vinculum (overbar) drawn
  * from the surd's top across the radicand, the font's gap of clearance above the radicand. The surd glyph
  * is chosen tall enough to span the radicand; when the available glyph is taller than strictly needed, the
  * excess is split evenly above and below — raising the bar and letting the surd dip below the baseline —
  * exactly as TeXbook rule 11 distributes it, so the radicand stays centred in the surd.
  */
class RadicalBox(t: Typesetter, surd: Box, radicand: Box, p: RadicalParams) extends ContentBox:

  // the surd should cover the radicand plus the gap and the bar; any height beyond that is shared above/below
  private val needed = p.verticalGap + p.ruleThickness + radicand.ascent + radicand.descent
  private val excess = math.max(0.0, surd.height - needed)

  private val gap        = p.verticalGap + excess / 2     // raise the bar by half the surplus
  private val barBottom  = radicand.ascent + gap
  private val barTop     = barBottom + p.ruleThickness

  val width: Double    = surd.width + radicand.width
  val xAdvance: Double = width
  val ascent: Double   = barTop + p.extraAscender
  // the surd's bottom, placed so its top meets the bar, sits the other half of the surplus below the radicand
  val descent: Double  = math.max(radicand.descent, surd.height - barTop)

  private val rule = RuleBox(t, radicand.width, p.ruleThickness, 0)

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    surd.draw(t, x, y - barTop + surd.ascent) // surd top aligned with the top of the bar
    rule.draw(t, x + surd.width, y - barBottom)
    radicand.draw(t, x + surd.width, y)

  override def toString: String = s"RadicalBox(surd=$surd, radicand=$radicand, barTop=$barTop)"
