package io.github.edadma.texish

class HBox(rawBoxes: Seq[Box]) extends ContentBox:

  // The line's height comes from its real content; a \vrule then runs to that height. Computing ascent/descent
  // from the non-vrule boxes first means a vrule never inflates the line — it spans whatever the content sets.
  private val sizing   = rawBoxes.filterNot(_.isInstanceOf[VRule])
  val ascent: Double   = if sizing.isEmpty then 0 else sizing.map(_.ascent).max
  val descent: Double  = if sizing.isEmpty then 0 else sizing.map(_.descent).max
  val boxes: Seq[Box]  = rawBoxes.map { case v: VRule => v.sized(ascent, descent); case b => b }
  val width: Double    = boxes.map(_.width).sum
  val xAdvance: Double = boxes.map(_.xAdvance).sum

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    // The first line drawn on a page sets the body baseline a fragment renderer aligns an inline formula by:
    // `y` here is the true line baseline, even for a fraction whose own glyphs all sit off it. The outermost
    // line box draws before any nested one, and the recorder keeps the first value, so this is that line.
    t.recordBodyBaseline(y)
    box(t, x, y)
    var currentX = x
    for box <- boxes do
      box.draw(t, currentX, y)
      currentX += box.width

  override def toString: String =
    s"HBox(width=$width, height=$height, ascent=$ascent, descent=$descent, boxes=$boxes)"
