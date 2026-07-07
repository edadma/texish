package io.github.edadma.texish.parser

import io.github.edadma.texish.{
  Box,
  Builder,
  CharBox,
  DocumentMode,
  HBox,
  HSpaceBox,
  HeadlessTypesetter,
  ShiftBox,
  VBox,
}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Horizontal material — inline glue (\hskip), a shifted box (\raise, a superscript) — begins a paragraph when it
  * is met in vertical mode, exactly as a character does and as TeX's engine does. It does not strand on the
  * vertical list, and the paragraph it opens honours the running indent state. A bare \hbox stays a vertical-list
  * item and is the deliberate exception.
  */
class LeaveVmodeTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    override infix def add(box: Box): Unit = super.add(box)

  private def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    t.document = new CapturingDocument(t)
    (t, proc)

  private def texts(box: Box): List[String] = box match
    case v: VBox     => v.boxes.toList.flatMap(texts)
    case h: HBox     => h.boxes.toList.flatMap(texts)
    case s: ShiftBox => texts(s.box)
    case c: CharBox  => List(c.text)
    case _           => Nil

  private def verticalList(t: HeadlessTypesetter): List[Box] =
    t.mode.asInstanceOf[Builder].list.toList

  "a superscript that leads a paragraph opens it rather than stranding on the vertical list" in {
    val (t, proc) = fixture()

    proc.process("\\textsup{1}word after")
    t.paragraph()

    val items = verticalList(t)

    // the script did not land as a bare box on the vertical list
    items.collect { case s: ShiftBox => s } shouldBe empty

    // one paragraph line holds both the raised "1" and the following words
    val lines = items.collect { case h: HBox => h }
    lines.length shouldBe 1
    lines.head.boxes.exists(_.isInstanceOf[ShiftBox]) shouldBe true
    texts(lines.head) should contain allOf ("1", "word", "after")
  }

  "\\hskip leaves vertical mode, like \\vskip ends a paragraph" in {
    val (t, proc) = fixture()

    proc.process("\\hskip 5 word")
    t.paragraph()

    val lines = verticalList(t).collect { case h: HBox => h }
    lines.length shouldBe 1
    texts(lines.head) should contain("word")
  }

  private def hasIndentBox(line: HBox): Boolean =
    line.boxes.exists { case h: HSpaceBox => h.indent; case _ => false }

  "the paragraph a superscript opens is indented when the indent state is on" in {
    val (t, proc) = fixture()
    t.indentParagraph = true

    proc.process("\\textsup{1}word")
    t.paragraph()

    val firstLine = verticalList(t).collect { case h: HBox => h }.head
    hasIndentBox(firstLine) shouldBe true
  }

  "the paragraph a superscript opens is flush when the indent state is off" in {
    val (t, proc) = fixture()
    t.indentParagraph = false

    proc.process("\\textsup{1}word")
    t.paragraph()

    val firstLine = verticalList(t).collect { case h: HBox => h }.head
    hasIndentBox(firstLine) shouldBe false
  }
