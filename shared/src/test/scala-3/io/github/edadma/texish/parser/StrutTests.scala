package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, DocumentMode, Glue, HBox, HeadlessTypesetter, StrutBox, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** \strut adds an invisible box one baselineskip tall to the current line, so leading stays regular whatever the
  * line's glyphs are — TeX's \strut, and the mechanism that holds a footnote's first line to a uniform height.
  */
class StrutTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    override infix def add(box: Box): Unit = super.add(box)

  private def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    t.document = new CapturingDocument(t)
    (t, proc)

  private def verticalList(t: HeadlessTypesetter): List[Box] =
    t.mode.asInstanceOf[Builder].list.toList

  private def struts(box: Box): List[StrutBox] = box match
    case v: VBox     => v.boxes.toList.flatMap(struts)
    case h: HBox     => h.boxes.toList.flatMap(struts)
    case s: StrutBox => List(s)
    case _           => Nil

  "\\strut adds a zero-width box one baselineskip tall, split about 0.7 above the baseline and 0.3 below" in {
    val (t, proc) = fixture()
    t.set("baselineskip", Glue(20))

    proc.process("\\strut word")
    t.paragraph()

    // it left vertical mode: there is exactly one paragraph line
    val lines = verticalList(t).collect { case h: HBox => h }
    lines.length shouldBe 1

    val found = lines.flatMap(struts)
    found.length shouldBe 1
    found.head.ascent shouldBe 14.0 +- 1e-9
    found.head.descent shouldBe 6.0 +- 1e-9
    found.head.width shouldBe 0.0
  }
