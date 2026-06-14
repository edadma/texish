package io.github.edadma.texish

class VBox(val boxes: Seq[Box]) extends ContentBox:

  val width: Double = if boxes.isEmpty then 0 else boxes.map(_.width).max
  override val height: Double = boxes.map(_.height).sum
  val descent: Double = if boxes.isEmpty then 0 else boxes.last.descent
  val ascent: Double = height - descent
  val xAdvance: Double = width

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)

    var currentY = if boxes.isEmpty then y else y - ascent + boxes.head.ascent
    var list = boxes

    while list.nonEmpty do
      val box = list.head

      box.draw(t, x, currentY)
      currentY += box.descent

      val tail = list.tail

      if tail.nonEmpty then currentY += tail.head.ascent

      list = tail

  override def toString: String =
    s"VBox(width=$width, height=$height, boxes=$boxes)"
