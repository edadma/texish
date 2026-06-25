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

  /** The (left, right) inset a line occupying the vertical band `[y0, y1]` inherits from the active cutouts — the
    * worst intrusion from each side, so two figures facing each other narrow the line from both.
    */
  def insetsAt(y0: Double, y1: Double): (Double, Double) =
    cutouts.foldLeft((0.0, 0.0)) { case ((l, r), c) =>
      val (cl, cr) = c.insetsOver(y0, y1)
      (math.max(l, cl), math.max(r, cr))
    }

  override infix def add(box: Box): Unit =
    // Control items (penalties, marks) are invisible to interline-glue insertion (TeX's \prevdepth tracks boxes,
    // not penalties), so the previous item for baseline purposes is the last non-control one. The glue still goes
    // after a penalty, giving the order box-penalty-glue-box: a break at the glue is then illegal (discardable
    // predecessor) and the penalty alone decides breakability between the two boxes.
    val prev = boxes.findLast(!_.isInstanceOf[ControlBox]).orNull

    if !box.isInstanceOf[ControlBox] && !box.isInstanceOf[NoGlueBox] && (prev ne null) && !prev.isSpace && !box.isSpace
      && !prev.isInstanceOf[NoGlueBox]
    then
      val baselineskip = t.getGlue("baselineskip") - prev.descent - box.ascent
      val skip =
        if baselineskip.naturalSize <= t.getNumber("lineskiplimit") then t.getGlue("lineskip")
        else baselineskip

      super.add(skip)
    end if

    super.add(box)
