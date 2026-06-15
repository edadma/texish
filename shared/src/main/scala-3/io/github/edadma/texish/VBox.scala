package io.github.edadma.texish

/** A vertical box: a column of boxes stacked top to bottom. The two concrete kinds differ only in where the
  * reference point — the baseline by which the box aligns with its neighbours — falls. Total height and the drawing
  * are identical; only the height/depth split differs. See [[VBox]] and [[VTop]].
  */
abstract class VerticalBox(val boxes: Seq[Box]) extends ContentBox:

  val width: Double          = if boxes.isEmpty then 0 else boxes.map(_.width).max
  override val height: Double = boxes.map(_.height).sum
  val xAdvance: Double       = width

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)

    var currentY = if boxes.isEmpty then y else y - ascent + boxes.head.ascent
    var list     = boxes

    while list.nonEmpty do
      val box = list.head

      box.draw(t, x, currentY)
      currentY += box.descent

      val tail = list.tail

      if tail.nonEmpty then currentY += tail.head.ascent

      list = tail

/** A \vbox: the reference point sits on the last line's baseline, so the box's depth is that last line's depth. This
  * is the usual vertical box, the one the page builder ships.
  */
class VBox(boxes: Seq[Box]) extends VerticalBox(boxes):

  val descent: Double = if boxes.isEmpty then 0 else boxes.last.descent
  val ascent: Double  = height - descent

  override def toString: String = s"VBox(width=$width, height=$height, boxes=$boxes)"

/** A \vtop: the reference point sits on the first line's baseline, so the box's height is just that first line's
  * height and everything below it counts as depth — useful for top-aligning material against surrounding text.
  */
class VTop(boxes: Seq[Box]) extends VerticalBox(boxes):

  val ascent: Double  = if boxes.isEmpty then 0 else boxes.head.ascent
  val descent: Double = height - ascent

  override def toString: String = s"VTop(width=$width, height=$height, boxes=$boxes)"
