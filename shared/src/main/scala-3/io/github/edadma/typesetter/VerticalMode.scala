package io.github.edadma.typesetter

//import pprint.pprintln

abstract class VerticalMode extends ListBoxBuilder:

  protected val measure: Box => Double = _.height
  protected val skip: Double => Box    = VSpaceBox(_)

  override infix def add(box: Box): Unit =
    // Penalties are invisible to interline-glue insertion (TeX's \prevdepth tracks boxes, not penalties), so the
    // previous item for baseline purposes is the last non-penalty one. The glue still goes after the penalty, giving
    // the order box-penalty-glue-box: a break at the glue is then illegal (discardable predecessor) and the penalty
    // alone decides breakability between the two boxes.
    val prev = boxes.findLast(!_.isInstanceOf[Penalty]).orNull

    if !box.isInstanceOf[Penalty] && !box.isInstanceOf[NoGlueBox] && (prev ne null) && !prev.isSpace && !box.isSpace
      && !prev.isInstanceOf[NoGlueBox]
    then
      val baselineskip = t.getGlue("baselineskip") - prev.descent - box.ascent
      val skip =
        if baselineskip.naturalSize <= t.getNumber("lineskiplimit") then t.getGlue("lineskip")
        else baselineskip

      super.add(skip)
    end if

    super.add(box)
