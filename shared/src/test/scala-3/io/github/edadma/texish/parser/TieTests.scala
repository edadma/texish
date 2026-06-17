package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, CharBox, DocumentMode, HBox, HeadlessTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The non-breaking tie ~ (an active character) puts an unbreakable space between two words, so a reference
  * like "Figure~1" never splits across a line. It is checked on the fixed-metric stub.
  */
class TieTests extends AnyFreeSpec with Matchers:

  // U+00A0 NO-BREAK SPACE, the character the active ~ emits.
  private val nbsp: Char = 0x00a0.toChar

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

  private def texts(b: Box): String = b match
    case c: CharBox => c.text
    case h: HBox    => h.boxes.toList.map(texts).mkString
    case v: VBox    => v.boxes.toList.map(texts).mkString
    case _          => ""

  private def allText(boxes: Seq[Box]): String = boxes.map(texts).mkString

  "~ ties two words with a non-breaking space" in {
    // the active ~ emits U+00A0, an unbreakable space riding the text path between the two words
    allText(render("Figure~1")).exists(_ == nbsp) shouldBe true
  }
