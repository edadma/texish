package io.github.edadma.texish

/** An inner box drawn with an optional background fill and an optional rectangular frame, separated from the
  * content by `sep` of padding on every side. One box backs three commands: \fbox / \framebox (frame only),
  * \colorbox (fill only), and \fcolorbox (both). The inner box keeps its baseline; the padding — and, when a
  * frame is drawn, the rule thickness — grows the box outward on all four sides.
  */
class FrameBox(
    inner: Box,
    sep: Double,
    rule: Double,
    frameColor: Color | Null,
    val bgColor: Color | Null,
) extends ContentBox:
  private val framed = frameColor != null && rule > 0
  private val pad    = sep + (if framed then rule else 0.0)

  val width: Double    = inner.width + 2 * pad
  val ascent: Double   = inner.ascent + pad
  val descent: Double  = inner.descent + pad
  val xAdvance: Double = width

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    val top = y - ascent
    if bgColor != null then
      t.setColor(bgColor)
      t.fillRect(x, top, width, height)
    if framed then
      t.setColor(frameColor)
      t.fillRect(x, top, width, rule)                // top edge
      t.fillRect(x, y + descent - rule, width, rule) // bottom edge
      t.fillRect(x, top, rule, height)               // left edge
      t.fillRect(x + width - rule, top, rule, height) // right edge
    inner.draw(t, x + pad, y)

  override def toString: String = s"FrameBox(sep=$sep, rule=$rule, inner=$inner)"
