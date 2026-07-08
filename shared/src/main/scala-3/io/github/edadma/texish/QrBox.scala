package io.github.edadma.texish

import io.github.edadma.qr.QrCode

/** A square box that paints a QR Code symbol as filled cells. Each dark module becomes a `cell`-by-`cell` filled
  * rectangle; a quiet zone of `quiet` light modules surrounds the symbol so a scanner can find its edges. The box
  * reports a fixed side of `(qr.size + 2 * quiet) * cell`, sits on the baseline like a rule (all ascent, no
  * descent), and draws its own light background across the whole area so the quiet zone stays clear of other ink
  * even on a coloured page.
  *
  * The matrix is produced by the standalone `io.github.edadma.qr` library, so the drawing here is pure geometry —
  * it renders as crisp vector rectangles on every backend (SVG, Cairo PDF/PNG, Graphics2D, Canvas), with no raster
  * image and nothing resolution-dependent.
  */
class QrBox(qr: QrCode, cell: Double, quiet: Int, dark: Color, light: Color) extends ContentBox:
  private val span = qr.size + 2 * quiet

  val width: Double    = span * cell
  val ascent: Double   = span * cell
  val descent: Double  = 0.0
  val xAdvance: Double = span * cell

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    val top = y - ascent
    if light.alpha > 0 then
      t.setColor(light)
      t.fillRect(x, top, width, height)
    t.setColor(dark)
    for my <- 0 until qr.size; mx <- 0 until qr.size if qr(mx, my) do
      t.fillRect(x + (mx + quiet) * cell, top + (my + quiet) * cell, cell, cell)

  override def toString: String = s"QrBox(${qr.size}x${qr.size} modules, cell=$cell)"
