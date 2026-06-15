package io.github.edadma.texish

/** A floating insertion placed at an edge of a page, modelled on the same migrating-insert mechanism as a footnote
  * but with edge rather than foot placement. The already-typeset block rides the main vertical list as a zero-size
  * item; the page builder counts its height against the page from the moment it is contributed, and at shipout lifts
  * the content to the top of the page or sinks it to the bottom — so a figure or table detaches from the text flow
  * and heads or foots the page it lands on. Not discardable: a float carried over a page break survives to head or
  * foot the new page instead, and because the vertical list keeps contribution order, several deferred floats keep
  * their order across the pages they spill onto.
  *
  * `placement` is an ordered preference drawn from `'h'` (here, inline at the point of contribution), `'t'` (top of
  * page) and `'b'` (bottom of page), as in LaTeX's `[htb]`. A `'h'` preference is resolved at contribution time by
  * the page builder: it stays inline if the current page can still hold it, otherwise it falls back to the next
  * listed edge. Any `FloatBox` that reaches the page builder's shipout is therefore an edge float, and [[edgeTop]]
  * says which edge.
  */
class FloatBox(val content: Box, val placement: List[Char]) extends MigratingBox:
  val isSpace: Boolean = false

  /** Which edge this float rides when it reaches shipout: the first of `'t'`/`'b'` in the preference list, or the
    * top when the list names no edge.
    */
  def edgeTop: Boolean = placement.find(c => c == 't' || c == 'b').forall(_ == 't')

  override def toString: String = s"FloatBox(${placement.mkString}, $content)"
