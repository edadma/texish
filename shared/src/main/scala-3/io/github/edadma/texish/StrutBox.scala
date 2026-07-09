package io.github.edadma.texish

/** A strut: an invisible, zero-width box with a fixed height and depth. Dropped onto a line, it forces that line to
  * at least the strut's metrics, so leading stays regular whatever glyphs the line happens to hold — a line with no
  * descender is spaced exactly like one full of them. This is TeX's `\strut`, and it is how a footnote's first line
  * is held to a uniform height below the separator rule.
  */
class StrutBox(val ascent: Double, val descent: Double) extends Box:
  val width: Double    = 0
  val isSpace: Boolean = false
  val xAdvance: Double = 0

  def draw(t: Typesetter, x: Double, y: Double): Unit = box(t, x, y)

  override def toString: String = s"StrutBox(ascent=$ascent, descent=$descent)"
