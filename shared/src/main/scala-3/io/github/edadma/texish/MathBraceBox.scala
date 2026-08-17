package io.github.edadma.texish

/** A math sub-formula with a horizontal brace drawn across its full width — above it for `\overbrace`, below it
  * for `\underbrace` — separated from the content by a small gap. The brace is a glyph grown along the font's
  * horizontal variants until it spans the content, so it is a real brace at every width rather than a stretched
  * outline; where the font supplies no variant wide enough, the widest one is used and the brace sits centred
  * over the content.
  *
  * The box grows by the gap plus the brace's own height on the braced side and keeps the content's reach on the
  * other, exactly as [[MathBarBox]] does for a rule. A braced formula is an Op atom, so a script attached to it
  * rides above the brace (`\overbrace{a + b}^{n}`) rather than beside it.
  */
class MathBraceBox(inner: Box, brace: Box, gap: Double, over: Boolean) extends ContentBox:

  private val braceHeight = brace.ascent + brace.descent
  private val dx          = (inner.width - brace.width) / 2 // centre a brace the font could not grow to full width

  val width: Double    = inner.width
  val xAdvance: Double = inner.xAdvance
  val ascent: Double   = if over then inner.ascent + gap + braceHeight else inner.ascent
  val descent: Double  = if over then inner.descent else inner.descent + gap + braceHeight

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    inner.draw(t, x, y)

    // the brace is drawn on its own baseline, so place that baseline where the brace's near edge lands the gap
    // away from the content: its bottom above the content's top, or its top below the content's bottom
    if over then brace.draw(t, x + dx, y - inner.ascent - gap - brace.descent)
    else brace.draw(t, x + dx, y + inner.descent + gap + brace.ascent)

  override def toString: String = s"MathBraceBox(over=$over, inner=$inner)"
