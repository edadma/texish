package io.github.edadma.texish.parser

import io.github.edadma.texish.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The font-shape and -series declarations — \itshape, \bfseries, \scshape and the resets \upshape, \mdseries,
  * \normalfont — flip the current font for the rest of the enclosing group and take no argument, unlike the
  * argument-wrapping \italic / \bold / \smallcaps. Font state is snapshotted on every group open and restored on
  * close, so a declaration reverts at the closing brace — or at an environment's \end, which closes a group too.
  * That is what lets an environment set its whole body in one shape (a run-in bold heading over an italic body,
  * the plain theorem style) without wrapping the body in a command.
  */
class FontDeclarationTests extends AnyFreeSpec with Matchers:

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

  private def styleOf(boxes: Seq[Box], mark: String): Set[String] =
    boxes.toList.flatMap(chars).collectFirst { case c if c.text.contains(mark) => c.font.style }.get

  "\\itshape sets italic for the rest of the group and reverts at the close" in {
    val boxes = render("{\\itshape X} Y")
    styleOf(boxes, "X") should contain("italic")
    styleOf(boxes, "Y") should not contain "italic"
  }

  "\\bfseries sets bold for the rest of the group and reverts at the close" in {
    val boxes = render("{\\bfseries X} Y")
    styleOf(boxes, "X") should contain("bold")
    styleOf(boxes, "Y") should not contain "bold"
  }

  "\\upshape clears italic for the remainder of the group" in {
    val boxes = render("{\\itshape A \\upshape B}")
    styleOf(boxes, "A") should contain("italic")
    styleOf(boxes, "B") should not contain "italic"
  }

  "\\mdseries clears the bold weight for the remainder of the group" in {
    val boxes = render("{\\bfseries A \\mdseries B}")
    styleOf(boxes, "A") should contain("bold")
    styleOf(boxes, "B") should not contain "bold"
  }

  "\\normalfont returns to the plain face" in {
    val boxes = render("{\\bfseries A \\normalfont B}")
    styleOf(boxes, "A") should contain("bold")
    styleOf(boxes, "B") should not contain "bold"
  }

  "an environment's body picks up a declaration and reverts at \\end" in {
    val boxes = render("\\newenvironment e {\\bfseries}{}\\begin{e}A \\end{e}B")
    styleOf(boxes, "A") should contain("bold")
    styleOf(boxes, "B") should not contain "bold"
  }

  "the plain theorem pattern — bold run-in head over an italic body, both reverting at \\end" in {
    val boxes = render("\\newenvironment thm {\\bfseries H \\mdseries\\itshape}{}\\begin{thm}A \\end{thm}B")
    styleOf(boxes, "H") should (contain("bold") and not contain "italic")
    styleOf(boxes, "A") should (contain("italic") and not contain "bold")
    styleOf(boxes, "B") should (not contain "bold" and not contain "italic")
  }

  "the argument-wrapping \\italic now actually sets italic (Font.equals regression)" in {
    val boxes = render("\\italic{X} Y")
    styleOf(boxes, "X") should contain("italic")
    styleOf(boxes, "Y") should not contain "italic"
  }

  "\\slshape sets the slanted shape for the rest of the group and reverts" in {
    val boxes = render("{\\slshape X} Y")
    styleOf(boxes, "X") should contain("slanted")
    styleOf(boxes, "Y") should not contain "slanted"
  }

  "the argument-wrapping \\slanted sets the slanted shape" in {
    val boxes = render("\\slanted{X} Y")
    styleOf(boxes, "X") should contain("slanted")
    styleOf(boxes, "Y") should not contain "slanted"
  }

  "the slope axis is exclusive — italic replaces slanted" in {
    val boxes = render("{\\slshape A \\itshape B}")
    styleOf(boxes, "A") should (contain("slanted") and not contain "italic")
    styleOf(boxes, "B") should (contain("italic") and not contain "slanted")
  }

  "the family-role axis is exclusive — the sans role replaces the mono role" in {
    val boxes = render("{\\ttfamily A \\sffamily B}")
    styleOf(boxes, "A") should (contain("mono") and not contain "sans")
    styleOf(boxes, "B") should (contain("sans") and not contain "mono")
  }

  "a family-role switch keeps the current weight (bold sans)" in {
    val boxes = render("{\\bfseries\\sffamily X}")
    styleOf(boxes, "X") should contain allOf ("bold", "sans")
  }

  "\\rmfamily clears the role back to roman" in {
    val boxes = render("{\\ttfamily A \\rmfamily B}")
    styleOf(boxes, "A") should contain("mono")
    styleOf(boxes, "B") should not contain "mono"
  }

  "a role+shape combination with no cut falls back to the nearest face without error" in {
    // small-caps mono has no file; it must substitute upright mono rather than throwing
    val boxes = render("{\\ttfamily\\scshape X}")
    styleOf(boxes, "X") should contain("mono")
  }

  "JetBrains Mono is available as a dedicated code face, distinct from the mono role" in {
    val boxes = render("{\\font jetbrains 10 regular X} \\texttt{Y}")
    boxes.toList.flatMap(chars).collectFirst { case c if c.text.contains("X") => c }.get.font.typeface shouldBe
      "jetbrains"
    // inline \texttt stays the lmroman mono role, not JetBrains
    styleOf(boxes, "Y") should contain("mono")
  }

  "\\fontsize changes only the size, keeping the current shape" in {
    val boxes = render("{\\bfseries\\fontsize{20} X} Y")
    val x     = boxes.toList.flatMap(chars).collectFirst { case c if c.text.contains("X") => c }.get
    x.font.size shouldBe (20.0 +- 1e-9)
    x.font.style should contain("bold")
    val y = boxes.toList.flatMap(chars).collectFirst { case c if c.text.contains("Y") => c }.get
    y.font.size shouldBe (14.0 +- 1e-9) // back to the default size after the group closes
  }
