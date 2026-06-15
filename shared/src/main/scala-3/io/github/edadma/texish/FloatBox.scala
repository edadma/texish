package io.github.edadma.texish

/** A floating insertion placed at the top of a page, modelled on the same migrating-insert mechanism as a footnote
  * but with top rather than foot placement. The already-typeset block rides the main vertical list as a zero-size
  * item; the page builder counts its height against the page from the moment it is contributed, and at shipout lifts
  * the content to the top of the page above a separating space — so a figure or table detaches from the text flow
  * and heads the page it lands on. Not discardable: a float carried over a page break survives at the top of the
  * new page, heading that page instead.
  */
class FloatBox(val content: Box) extends MigratingBox:
  val isSpace: Boolean = false

  override def toString: String = s"FloatBox($content)"
