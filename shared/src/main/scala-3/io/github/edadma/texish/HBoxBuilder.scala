package io.github.edadma.texish

class HBoxBuilder(
    val t: Typesetter,
    var toSize: Double | Null = null,
    spreadAmt: Double | Null = null,
    reorderBidi: Boolean = false,
) extends ListBoxBuilder
    with HorizontalMode:

  spread = spreadAmt

  protected val measure: Box => Double = _.width
  protected val skip: Double => Box    = HSpaceBox(_)
  protected val wrap: Seq[Box] => Box  = HBox(_)

  // An explicit horizontal box (\hbox, \mbox, \makebox, \centerline, …) shapes and reorders its own content the
  // way a paragraph line does, so a right-to-left title, heading, or reference set outside a running paragraph
  // reads and joins correctly. The base direction follows the current \rtl / \ltr state. Paragraph lines arrive
  // already reordered through their own builder and leave this off, so nothing is processed twice; a pure
  // left-to-right box short-circuits in needsReorder and is built exactly as before.
  override def result: Box =
    if reorderBidi && Bidi.needsReorder(boxes) then
      val base = if t.getNumber("pardir") == 1.0 then 1 else 0
      Bidi.reorderVisual(boxes, base)
    super.result
