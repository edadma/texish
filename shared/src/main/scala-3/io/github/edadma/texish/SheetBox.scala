package io.github.edadma.texish

/** A box of fixed dimensions that draws a list of children at absolute top-left offsets from its own top-left
  * corner, reporting the given size no matter where the children fall. It is the imposition primitive, used at
  * two levels: the page builder composes one logical page — its body with the running header and footer around
  * it — into a page-sized SheetBox, and a page arrangement composes one or more of those page boxes onto a
  * physical sheet. Because the size is fixed by construction, the children contribute no metrics of their own; a
  * sheet is as large as the paper, whatever is printed on it.
  *
  * Offsets are measured in the box's own top-left coordinate frame — `dy` grows downward — so a child placed at
  * `(dx, dy)` has its top-left corner there. This mirrors [[Typesetter.draw]], which lands a box's top-left at the
  * point it is given, so composing and shipping a SheetBox reproduces the same device coordinates as drawing the
  * children directly.
  */
class SheetBox(val width: Double, override val height: Double, val placed: Seq[(Box, Double, Double)]) extends ContentBox:
  val ascent: Double   = height
  val descent: Double  = 0.0
  val xAdvance: Double = width

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    val top = y - height
    for (child, dx, dy) <- placed do t.draw(child, x + dx, top + dy)

  override def toString: String = s"SheetBox(${width}x$height, ${placed.length} placed)"
