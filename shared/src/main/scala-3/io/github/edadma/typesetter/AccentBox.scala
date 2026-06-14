package io.github.edadma.typesetter

/** A nucleus with an accent set above it (`\hat`, `\bar`, `\vec`, `\widehat`, …). The accent glyph is drawn
  * over the nucleus, its top-accent attachment point aligned with the nucleus's so it centres on the visual
  * middle rather than the geometric one, and raised by `shift` — how much taller the nucleus is than the
  * font's accent base height, so the accent rests just above a tall letter and at a uniform height over short
  * ones. The box keeps the nucleus's width and descent and rises to clear the accent.
  */
class AccentBox(nucleus: Box, accent: Box, shift: Double, dx: Double) extends ContentBox:
  val width: Double    = nucleus.width
  val xAdvance: Double  = nucleus.xAdvance
  val ascent: Double   = math.max(nucleus.ascent, shift + accent.ascent)
  val descent: Double  = math.max(nucleus.descent, accent.descent - shift)

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    nucleus.draw(t, x, y)
    accent.draw(t, x + dx, y - shift) // the accent is designed to sit above a baseline; raise it onto the nucleus

  override def toString: String = s"AccentBox(nucleus=$nucleus, accent=$accent, shift=$shift, dx=$dx)"
