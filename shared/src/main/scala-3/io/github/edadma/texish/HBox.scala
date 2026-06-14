package io.github.edadma.texish

class HBox(val boxes: Seq[Box]) extends ContentBox:

  val width: Double = boxes.map(_.width).sum
  val ascent: Double = if boxes.isEmpty then 0 else boxes.map(_.ascent).max
  val descent: Double = if boxes.isEmpty then 0 else boxes.map(_.descent).max
  val xAdvance: Double = boxes.map(_.xAdvance).sum

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    var currentX = x
    for box <- boxes do
      box.draw(t, currentX, y)
      currentX += box.width

  override def toString: String =
    s"HBox(width=$width, height=$height, ascent=$ascent, descent=$descent, boxes=$boxes)"
