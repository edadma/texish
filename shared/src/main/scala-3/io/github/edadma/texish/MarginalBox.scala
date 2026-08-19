package io.github.edadma.texish

/** A note set in the margin beside the line that called for it.
  *
  * Like a footnote it is written inside the running text but does not belong to any line: the content is typeset
  * immediately, into its own vertical box of the note's measure, and the box rides the vertical list as a zero-size
  * item attached to the line it was written in (see [[MigratingBox]]). Occupying no space is what keeps it out of
  * the way — the line it annotates is set as though the note were not there, the page breaks where it would have
  * broken, and the note simply draws beside whatever line it ended up on.
  *
  * `offset` is measured from the left edge of the text block, so a note in the right margin passes `\hsize` plus the
  * gap and one in the left margin passes a negative offset — the caller decides, which is what lets a two-sided
  * document put its notes in the outer margin by asking which side the page falls on. `rise` lifts the note's top
  * above the point where it lands: an item migrating out of a line is placed just below that line, so a rise of one
  * baseline puts the note's first line level with the line it annotates.
  */
class MarginalBox(val content: Box, val offset: Double, val rise: Double) extends MigratingBox:
  val isSpace: Boolean = false

  override def draw(t: Typesetter, x: Double, y: Double): Unit =
    content.draw(t, x + offset, y - rise + content.ascent)

  override def toString: String = s"MarginalBox(offset=$offset, rise=$rise, $content)"
