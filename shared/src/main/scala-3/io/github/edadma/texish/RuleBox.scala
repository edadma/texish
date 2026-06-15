package io.github.edadma.texish

/** A solid rectangular rule. `running` marks a rule whose width is a fallback to be replaced by its container's
  * width: an \hrule with no explicit width runs to the width of the box it lands in — e.g. the table width inside
  * an \halign's \noalign. A non-running rule keeps the width it was built with.
  */
class RuleBox(
    t: Typesetter,
    val width: Double,
    val ascent: Double,
    val descent: Double,
    color: Color,
    val running: Boolean = false,
) extends NoGlueBox:
  def this(t: Typesetter, width: Double, ascent: Double, descent: Double) =
    this(t, width, ascent, descent, t.currentColor)
  require(width >= 0, "rule width is non-negative")
  require(ascent >= 0, "rule ascent is non-negative")
  require(descent >= 0, "rule descent is non-negative")

  /** This rule re-sized to a resolved width, keeping its thickness and colour. */
  def withWidth(w: Double): RuleBox = new RuleBox(t, w, ascent, descent, color)

//  val (ascent, descent) =
//    if shift >= 0 then (thickness + shift, 0d)
//    else if thickness + shift < 0 then (0d, thickness - shift)
//    else (thickness + shift, -shift)

  val xAdvance: Double = width

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    t.setColor(color)
    t.fillRect(x, y - ascent, width, height)
