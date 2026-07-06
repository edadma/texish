package io.github.edadma.texish

/** A box that reserves an inner box's size along selected axes without contributing its full extent to the
  * surrounding list — the mechanism behind `\phantom` and `\smash`.
  *
  * `keepWidth`/`keepHeight` choose which axes the inner box's measurements are reported on; the other axis
  * collapses to zero. `visible` decides whether the inner box is actually drawn:
  *
  *   - `\phantom`  — keep both axes, draw nothing (an invisible spacer the exact size of its argument).
  *   - `\hphantom` — keep width only, draw nothing (height and depth collapse).
  *   - `\vphantom` — keep height and depth only, draw nothing (width collapses).
  *   - `\smash`    — keep width, draw the ink, but report zero height and depth so it overlaps its neighbours
  *                   vertically rather than spreading the line.
  */
class PhantomBox(val inner: Box, keepWidth: Boolean, keepHeight: Boolean, visible: Boolean) extends Box:
  val ascent: Double   = if keepHeight then inner.ascent else 0.0
  val descent: Double  = if keepHeight then inner.descent else 0.0
  val width: Double    = if keepWidth then inner.width else 0.0
  val xAdvance: Double = if keepWidth then inner.xAdvance else 0.0
  val isSpace: Boolean = false

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    if visible then inner.draw(t, x, y) // \smash draws at the baseline; the phantoms draw nothing
