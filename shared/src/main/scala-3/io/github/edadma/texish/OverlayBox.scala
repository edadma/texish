package io.github.edadma.texish

/** A box that draws its content at a fixed offset from the reference point but reports no metrics of its own, so it
  * neither advances the vertical list nor inflates the line it rides on. It is the `\smash` and `\rlap` of TeX rolled
  * together: the content is painted, the layout pretends it occupies no space.
  *
  * This is how a wrapped figure is anchored beside running text. The figure is taller than the line that carries it,
  * so it cannot ride that line as ordinary content; instead it overlays the galley at the wrap's first line and the
  * accompanying [[Cutout]] reserves the horizontal room across the lines it spans. `dx` places it against the chosen
  * margin (zero for a left figure, the remaining width for a right one) and `dy` drops it from the baseline anchor
  * down over the band it occupies.
  */
class OverlayBox(content: Box, dx: Double, dy: Double) extends ContentBox:
  val ascent: Double   = 0.0
  val descent: Double  = 0.0
  override val width: Double = 0.0
  val xAdvance: Double = 0.0

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    content.draw(t, x + dx, y + dy)

  override def toString: String = s"OverlayBox(dx=$dx, dy=$dy, $content)"
