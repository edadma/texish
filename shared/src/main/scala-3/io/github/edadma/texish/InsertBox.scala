package io.github.edadma.texish

/** A footnote insertion, modelled on a simplified TeX \insert: the already-typeset footnote block rides the main
  * vertical list as a zero-size item attached to the line that references it. The page builder counts the content's
  * height against the page from the moment it is contributed, and at shipout moves the content to the foot of the
  * page below a separator rule — so a reference and its footnote always share a page. Not discardable: an insert
  * carried over a page break survives at the top of the new page, taking its footnote to that page's foot.
  */
class InsertBox(val content: Box) extends MigratingBox:
  val isSpace: Boolean = false

  override def toString: String = s"InsertBox($content)"
