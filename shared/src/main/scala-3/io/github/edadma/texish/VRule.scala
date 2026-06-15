package io.github.edadma.texish

/** A vertical rule whose height runs to the line it sits in: it occupies `thickness` of horizontal space but takes
  * its ascent and depth from the enclosing [[HBox]], which replaces it with a concrete [[RuleBox]] at build time
  * (so a `\vrule` in a table cell spans that cell's height). Until resolved it draws nothing.
  */
class VRule(t: Typesetter, val thickness: Double, val color: Color) extends NoGlueBox:
  def this(t: Typesetter, thickness: Double) = this(t, thickness, t.currentColor)
  require(thickness >= 0, "rule thickness is non-negative")

  val width: Double    = thickness
  val ascent: Double   = 0
  val descent: Double  = 0
  val xAdvance: Double = thickness

  /** This rule resolved to a concrete rule of the given height (ascent above the baseline, descent below). */
  def sized(a: Double, d: Double): RuleBox = new RuleBox(t, thickness, a, d, color)

  def draw(t: Typesetter, x: Double, y: Double): Unit = () // unresolved: nothing to draw
