package io.github.edadma.texish

/** A mark, as in TeX: an invisible labelled point in the vertical list. When a page ships, the page builder records
  * which marks landed on it — the topmark/firstmark/botmark variables — so shipout-time material like a running
  * header can show, say, the range of reference points the page covers. A mark issued inside a paragraph migrates
  * out to the vertical list after its line. Unlike glue and penalties, a mark is not discardable: one sitting at the
  * top of a new page survives the break and becomes that page's first mark — and, as in TeX, it counts as a
  * non-discardable predecessor, so glue following it is a legal breakpoint.
  *
  * There are two independent streams, so a two-sided running head can name a division on one side of the opening
  * and a subdivision on the other — LaTeX's `\leftmark`/`\rightmark` pair, and the reason it has one. `sub` picks
  * the second: `\mark` writes the first stream (topmark/firstmark/botmark), `\submark` the second
  * (topsubmark/firstsubmark/botsubmark). The streams are tracked separately and neither disturbs the other; a
  * chapter that wants the section name cleared under it issues an empty `\submark` of its own.
  */
class MarkBox(val text: String, val sub: Boolean = false) extends MigratingBox:
  val isSpace: Boolean = false

  override def toString: String = s"MarkBox($text${if sub then ", sub" else ""})"
