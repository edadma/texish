package io.github.edadma.texish.parser

import io.github.edadma.texish.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** \mono sets its argument in the typewriter member of the current super-family — the mono *role* of the
  * `lmroman` family, not a separate typeface — and reverts to the body font afterwards. Because the mono cut
  * carries no ligatures, the smart-quote and dash representations stay off inside it, so code-like text such as
  * file names is set literally.
  */
class FontRoleTests extends AnyFreeSpec with Matchers:

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

  private def fontOf(boxes: Seq[Box], mark: String): Font =
    boxes.toList.flatMap(chars).collectFirst { case c if c.text.contains(mark) => c.font }.get

  private def allText(boxes: Seq[Box]): String = boxes.toList.flatMap(chars).map(_.text).mkString

  "\\mono sets its content in the mono role and reverts after" in {
    val boxes = render("\\mono{X} Y")
    fontOf(boxes, "X").style should contain("mono")
    fontOf(boxes, "Y").style should not contain "mono"
  }

  "\\mono keeps the surrounding weight — bold code stays bold" in {
    val boxes = render("\\bold{a\\mono{X}}")
    fontOf(boxes, "X").style should contain allOf ("mono", "bold")
  }

  "\\sans and \\serif select the sans and roman roles" in {
    fontOf(render("\\sans{X} Y"), "X").style should contain("sans")
    // \serif inside a sans run returns to the roman member, dropping the sans role
    fontOf(render("\\sans{a\\serif{X}}"), "X").style should not contain "sans"
  }

  "the dash representations stay off inside \\mono" in {
    allText(render("\\mono{a--b}")) should not include `EN DASH`
  }
