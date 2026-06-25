package io.github.edadma.texish

import scala.collection.mutable.ArrayBuffer

//import pprint.pprintln

abstract class VerticalMode extends ListBoxBuilder:

  protected val measure: Box => Double = _.height
  protected val skip: Double => Box    = VSpaceBox(_)

  /** Cutouts active on this galley — figures that running text flows around. They persist as paragraphs are added
    * (a tall figure wraps the paragraphs that follow it) and are cleared when the galley is shipped out.
    */
  val cutouts = ArrayBuffer[Cutout]()

  /** The galley's current natural height: the top of whatever is contributed next, in galley coordinates. Interline
    * glue is already materialised as a box in the list (see `add`), so the plain sum of item heights is exact, and
    * it is the vertical position a paragraph or a freshly registered cutout starts from.
    */
  def naturalHeight: Double = boxes.iterator.map(_.height).sum

  /** The galley height at which `anchor` sits — the summed heights of the boxes before it — or None once the anchor
    * is gone from the list, which happens when the figure it marks has shipped with its page. Resolving the cutout
    * top this way, from the live list, is what keeps a wrap aligned with its figure across a page break.
    */
  private def positionOf(anchor: Box): Option[Double] =
    var acc = 0.0
    val it  = boxes.iterator
    while it.hasNext do
      val b = it.next()
      if b eq anchor then return Some(acc)
      acc += b.height
    None

  /** The `[top, bottom]` band a cutout currently occupies, or None if its anchor has shipped away. Resolving the top
    * from the live list — rather than a coordinate frozen when the cutout was registered — is what keeps a wrap
    * aligned with its figure: a fixed top drifts the moment a page break pads a fresh topskip above the figure.
    */
  def cutoutBand(c: Cutout): Option[(Double, Double)] =
    positionOf(c.anchor).map(top => (top, top + c.height))

  override infix def add(box: Box): Unit =
    // Control items (penalties, marks) are invisible to interline-glue insertion (TeX's \prevdepth tracks boxes,
    // not penalties), so the previous item for baseline purposes is the last non-control one. The glue still goes
    // after a penalty, giving the order box-penalty-glue-box: a break at the glue is then illegal (discardable
    // predecessor) and the penalty alone decides breakability between the two boxes.
    // TeX's \prevdepth tracks the depth of the last BOX on the list, not the last item: glue, kerns and penalties
    // pass through it. So the interline glue before a new box is computed from that last box even when an explicit
    // \vskip sits between them — the \vskip then ADDS to the leading rather than replacing it. (The earlier code
    // read the last item, so a \vskip suppressed the interline and jammed the next line up against the previous.)
    val prevBox = boxes.findLast(b => !b.isInstanceOf[ControlBox] && !b.isInstanceOf[NoGlueBox] && !b.isSpace).orNull

    if !box.isInstanceOf[ControlBox] && !box.isInstanceOf[NoGlueBox] && !box.isSpace && (prevBox ne null) then
      val baselineskip = t.getGlue("baselineskip") - prevBox.descent - box.ascent
      val skip =
        if baselineskip.naturalSize <= t.getNumber("lineskiplimit") then t.getGlue("lineskip")
        else baselineskip

      super.add(skip)
    end if

    super.add(box)
