package io.github.edadma.typesetter

//import pprint.pprintln

abstract class VerticalMode extends ListBoxBuilder:

  protected val measure: Box => Double = _.height
  protected val skip: Double => Box    = VSpaceBox(_)

  override infix def add(box: Box): Unit =
    if !box.isInstanceOf[NoGlueBox] && nonEmpty && !last.isSpace && !box.isSpace && !last.isInstanceOf[NoGlueBox] then
      val baselineskip = t.getGlue("baselineskip") - last.descent - box.ascent
      val skip =
        if baselineskip.naturalSize <= t.getNumber("lineskiplimit") then t.getGlue("lineskip")
        else baselineskip

      super.add(skip)
    end if

    super.add(box)
