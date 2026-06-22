package io.github.edadma.texish

/** A math sub-formula with a horizontal rule drawn across its full width — above it for `\overline`, below it
  * for `\underline` — separated from the content by a small gap. The rule spans the content exactly; the box
  * grows by the gap plus the rule thickness on the ruled side, and keeps the content's reach on the other.
  *
  * This is distinct from the `\bar` accent, which centres a short macron over a single symbol: an overline
  * stretches the full width of whatever it covers, so `\overline{x + y}` bars the whole sum.
  */
class MathBarBox(t: Typesetter, inner: Box, gap: Double, thickness: Double, over: Boolean) extends ContentBox:

  val width: Double    = inner.width
  val xAdvance: Double = width
  val ascent: Double   = if over then inner.ascent + gap + thickness else inner.ascent
  val descent: Double  = if over then inner.descent else inner.descent + gap + thickness

  private val rule = RuleBox(t, width, thickness, 0.0)

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    inner.draw(t, x, y)
    if over then rule.draw(t, x, y - inner.ascent - gap)
    else rule.draw(t, x, y + inner.descent + gap + thickness)

  override def toString: String = s"MathBarBox(over=$over, inner=$inner)"
