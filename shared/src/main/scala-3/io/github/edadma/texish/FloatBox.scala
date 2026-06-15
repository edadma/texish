package io.github.edadma.texish

/** A floating insertion placed at the top or bottom of a page, modelled on the same migrating-insert mechanism as a
  * footnote but with edge rather than foot placement. The already-typeset block rides the main vertical list as a
  * zero-size item; the page builder counts its height against the page from the moment it is contributed, and at
  * shipout lifts the content to the top of the page (when `top`) or sinks it to the bottom — so a figure or table
  * detaches from the text flow and heads or foots the page it lands on. Not discardable: a float carried over a page
  * break survives to head or foot the new page instead.
  */
class FloatBox(val content: Box, val top: Boolean) extends MigratingBox:
  val isSpace: Boolean = false

  override def toString: String = s"FloatBox(${if top then "top" else "bottom"}, $content)"
