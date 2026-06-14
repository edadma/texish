package io.github.edadma.texish

/** A mark, as in TeX: an invisible labelled point in the vertical list. When a page ships, the page builder records
  * which marks landed on it — the topmark/firstmark/botmark variables — so shipout-time material like a running
  * header can show, say, the range of reference points the page covers. A mark issued inside a paragraph migrates
  * out to the vertical list after its line. Unlike glue and penalties, a mark is not discardable: one sitting at the
  * top of a new page survives the break and becomes that page's first mark — and, as in TeX, it counts as a
  * non-discardable predecessor, so glue following it is a legal breakpoint.
  */
class MarkBox(val text: String) extends MigratingBox:
  val isSpace: Boolean = false

  override def toString: String = s"MarkBox($text)"
