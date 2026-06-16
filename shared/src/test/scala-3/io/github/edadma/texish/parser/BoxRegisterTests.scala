package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, CharBox, HBox, HSpaceBox, HeadlessTypesetter, Typesetter, VBox, VerticalBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Box registers: \setbox saves a typeset \hbox / \vbox under a name; \wd / \ht / \dp measure it; \box and \copy
  * place it (\box voids the register, \copy keeps it); \unhbox / \unvbox splice its contents into the list. The
  * HeadlessTypesetter gives every character a width of 6 and metrics ascent 8 / descent 2, so dimensions are exact.
  */
class BoxRegisterTests extends AnyFreeSpec with Matchers:

  /** A typesetter that records every box handed to `add`, in order. Boxes built inside an \hbox/\vbox builder
    * are added to that builder (and recorded too); the box that is finally *placed* in the outer list is the
    * last add of the operation under test.
    */
  private class Capture extends HeadlessTypesetter:
    val added = ArrayBuffer[Box]()
    override infix def add(box: Box): Typesetter =
      added += box
      super.add(box)

  private def fixture(): (Capture, Processor) =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  "\\setbox saves a box that \\wd / \\ht / \\dp measure" in {
    val (_, proc) = fixture()
    proc.process("\\setbox a \\hbox{AB} \\set w {\\wd a} \\set h {\\ht a} \\set d {\\dp a}")
    proc.handler.get("a") shouldBe a[Value.Native]
    proc.handler.get("w") shouldBe Value.Dimen(12.0) // two chars × 6
    proc.handler.get("h") shouldBe Value.Dimen(8.0)  // ascent (height above the baseline)
    proc.handler.get("d") shouldBe Value.Dimen(2.0)  // descent (depth below the baseline)
  }

  "a \\vbox register measures the stacked metrics" in {
    val (_, proc) = fixture()
    proc.process("\\setbox v \\vbox{\\hbox{AB}} \\set w {\\wd v} \\set h {\\ht v} \\set d {\\dp v}")
    proc.handler.get("v") shouldBe a[Value.Native]
    proc.handler.get("w") shouldBe Value.Dimen(12.0)
    proc.handler.get("h") shouldBe Value.Dimen(8.0)
    proc.handler.get("d") shouldBe Value.Dimen(2.0)
  }

  "\\box places the saved box and empties the register" in {
    val (t, proc) = fixture()
    proc.process("\\setbox a \\hbox{AB} \\box a")
    t.added.last shouldBe a[HBox]
    proc.handler.get("a") shouldBe Value.Undefined
  }

  "\\copy places the saved box but keeps the register" in {
    val (t, proc) = fixture()
    proc.process("\\setbox a \\hbox{AB} \\copy a")
    t.added.last shouldBe a[HBox]
    proc.handler.get("a") shouldBe a[Value.Native]
  }

  "\\unhbox splices the box's contents, so the wrapping \\hbox never enters the list" in {
    val (t, proc) = fixture()
    proc.process("\\setbox a \\hbox{AB} \\unhbox a")
    t.added.last shouldBe a[CharBox]                          // the inner CharBox, spliced in
    t.added.exists(_.isInstanceOf[HBox]) shouldBe false       // the wrapper was unpacked, not placed
    proc.handler.get("a") shouldBe Value.Undefined
  }

  "\\unvbox splices a vbox's lines, so the wrapping \\vbox never enters the list" in {
    val (t, proc) = fixture()
    proc.process("\\setbox v \\vbox{\\hbox{AB}} \\unvbox v")
    t.added.last shouldBe a[HBox]                             // the line, spliced in
    t.added.exists(_.isInstanceOf[VBox]) shouldBe false
    proc.handler.get("v") shouldBe Value.Undefined
  }

  "a measured dimension feeds a dimension-consuming primitive (\\kern\\wd)" in {
    val (t, proc) = fixture()
    proc.process("\\setbox a \\hbox{ABCD} \\kern\\wd a")
    val space = t.added.last
    space shouldBe a[HSpaceBox]
    space.width shouldBe 24.0 // four chars × 6
  }

  "\\hbox to:{\\wd ref} stretches a box to another box's width" in {
    val (t, proc) = fixture()
    proc.process("\\setbox a \\hbox{ABCD} \\hbox to:{\\wd a}{C\\hfil}")
    val placed = t.added.last
    placed shouldBe a[HBox]
    placed.width shouldBe 24.0
  }

  "a measured dimension, stored, can be used in \\calc" in {
    val (_, proc) = fixture()
    proc.process("\\setbox a \\hbox{ABCD} \\set w {\\wd a} \\set s {\\calc{w + 6}}")
    proc.handler.get("s") shouldBe Value.Num(30.0)
  }

  "\\vtop saves a vtop box that \\unvbox can splice" in {
    val (t, proc) = fixture()
    proc.process("\\setbox v \\vtop{\\hbox{AB}} \\unvbox v")
    t.added.last shouldBe a[HBox]                              // the line, spliced in
    t.added.exists(_.isInstanceOf[VerticalBox]) shouldBe false // the vtop wrapper was unpacked, not placed
    proc.handler.get("v") shouldBe Value.Undefined
  }

  "\\hbox spread: widens a box past its natural size" in {
    val (_, proc) = fixture()
    proc.process("\\setbox a \\hbox spread:18{C\\hfil} \\set w {\\wd a}")
    proc.handler.get("w") shouldBe Value.Dimen(24.0) // natural 6 (one char) + 18 of spread
  }

  "a box rejects both to: and spread:" in {
    val (_, proc) = fixture()
    a[ParserException] should be thrownBy proc.process("\\hbox to:10 spread:5{x}")
  }

  "\\setbox without a following box is an error" in {
    val (_, proc) = fixture()
    a[ParserException] should be thrownBy proc.process("\\setbox a {plain text}")
  }

  "measuring an empty register is an error" in {
    val (_, proc) = fixture()
    a[ParserException] should be thrownBy proc.process("\\wd nosuchbox")
  }

  "measuring a non-box variable is an error" in {
    val (_, proc) = fixture()
    a[ParserException] should be thrownBy proc.process("\\set a {5} \\wd a")
  }

  "\\unhbox of a vbox register is an error" in {
    val (_, proc) = fixture()
    a[ParserException] should be thrownBy proc.process("\\setbox v \\vbox{\\hbox{AB}} \\unhbox v")
  }
