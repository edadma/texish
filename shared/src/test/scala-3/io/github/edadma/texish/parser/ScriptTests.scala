package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, CharBox, DocumentMode, HBox, HeadlessTypesetter, ShiftBox, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** \textsub and \textsup set their body in a smaller version of the current font and shift the box below or
  * above the baseline. The size is derived from the current font, so a script adapts to the body size. Checked
  * on the stub, where the surrounding font is lmroman 14.
  */
class ScriptTests extends AnyFreeSpec with Matchers:

  private val baseSize = 14.0

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def render(src: String): Seq[Box] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\set raggedbottom {1}\n" + src + "\n"))
    t.end()
    doc.shipped.toSeq

  private def shifts(b: Box): List[ShiftBox] = b match
    case s: ShiftBox => s :: shifts(s.box)
    case h: HBox     => h.boxes.toList.flatMap(shifts)
    case v: VBox     => v.boxes.toList.flatMap(shifts)
    case _           => Nil

  private def chars(b: Box): List[CharBox] = b match
    case c: CharBox  => List(c)
    case s: ShiftBox => chars(s.box)
    case h: HBox     => h.boxes.toList.flatMap(chars)
    case v: VBox     => v.boxes.toList.flatMap(chars)
    case _           => Nil

  "\\textsub lowers a smaller box below the baseline" in {
    val sb = render("H\\textsub{2}O").flatMap(shifts).head
    sb.shift should be > 0.0                              // positive shift = lowered
    chars(sb.box).map(_.text).mkString shouldBe "2"
    chars(sb.box).head.font.size should be < baseSize     // smaller than the surrounding font
  }

  "\\textsup raises a smaller box above the baseline" in {
    val sb = render("x\\textsup{2}").flatMap(shifts).head
    sb.shift should be < 0.0                              // negative shift = raised
    chars(sb.box).head.font.size should be < baseSize
  }

  "the surrounding text returns to the base font after the script" in {
    val cs = render("H\\textsub{2}O").flatMap(chars)
    cs.find(_.text == "H").get.font.size shouldBe baseSize
    cs.find(_.text == "O").get.font.size shouldBe baseSize
  }
