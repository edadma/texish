package io.github.edadma.texish.parser

import io.github.edadma.texish.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** \texttt sets its argument in the monospaced face and reverts to the body font afterwards. Because no
  * ligatures are enabled on the mono font, the smart-quote and dash representations stay off inside it, so
  * code-like text such as file names is set literally.
  */
class TextttTests extends AnyFreeSpec with Matchers:

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

  private def chars(b: Box): List[CharBox] = b match
    case c: CharBox => List(c)
    case h: HBox    => h.boxes.toList.flatMap(chars)
    case v: VBox    => v.boxes.toList.flatMap(chars)
    case _          => Nil

  private def typefaceOf(boxes: Seq[Box], mark: String): String =
    boxes.toList.flatMap(chars).collectFirst { case c if c.text.contains(mark) => c.font.typeface }.get

  private def allText(boxes: Seq[Box]): String = boxes.toList.flatMap(chars).map(_.text).mkString

  "\\texttt sets its content in the mono typeface and reverts after" in {
    val boxes = render("\\texttt{X} Y")
    typefaceOf(boxes, "X") shouldBe "mono"
    typefaceOf(boxes, "Y") shouldBe "lmroman"
  }

  "the dash representations stay off inside \\texttt" in {
    allText(render("\\texttt{a--b}")) should not include `EN DASH`
  }
