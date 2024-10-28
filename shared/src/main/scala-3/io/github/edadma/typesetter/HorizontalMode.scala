package io.github.edadma.typesetter

import pprint.pprintln

trait HorizontalMode extends Builder:
  override infix def add(box: Box): Unit =
    if nonEmpty then
      (last, box) match
        case (l: CharBox, b: CharBox)
            if b.text == " " &&
              !(l.text.last == '.' && Abbreviation(l.text.dropRight(1))) &&
              ".!?:;".contains(l.text.last) =>
          super.add(t.getGlue("xspaceskip"))
        case (_, b: CharBox) if b.text == " " => super.add(t.getGlue("spaceskip"))
        case (l: CharBox, b: CharBox) if l.font == b.font && l.color == b.color =>
          update(length - 1, l.newCharBox(l.text ++ b.text))
        case _ => super.add(box)

//        case (_, _: (HBox | Glue))            => super.add(box)
//        case (_: (SpaceBox | Glue), _)        => super.add(box)
//        case (_, b: CharBox) if b.text == "," => super.add(box)
//        case (l: CharBox, b: CharBox)
//            if b.text.nonEmpty && !l.text.endsWith(",") && !l.text
//              .endsWith(")") && !b.text.startsWith("(") && (!l.text
//              .exists(_.isLetterOrDigit) || !b.text.exists(_.isLetterOrDigit)) =>
//          update(length - 1, l.newCharBox(l.text ++ b.text))
//        case (b: CharBox, _)
//            if b.text.nonEmpty &&
//              !(b.text.last == '.' && Abbreviation(b.text.dropRight(1))) &&
//              ".!?:;".contains(b.text.last) =>
//          super.add(t.getGlue("xspaceskip"))
//          super.add(box)
//        case _ =>
//          super.add(t.getGlue("spaceskip"))
//          super.add(box)
    else super.add(box)
