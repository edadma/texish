package io.github.edadma.texish.parser

import io.github.edadma.texish.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The `base` package (packages/base.texish) holds the class-independent conveniences — the size ladder, vertical
  * skips, single-line alignment, inline markup, the TeX logos and the alignment environments — that used to live in
  * `document`. They must work on their own under \use{base}, without the full document class; `document` in turn
  * builds on `base` (\use{base}), so DocumentFeaturesTests is the companion regression that the layering did not
  * break the class.
  */
class BasePackageTests extends AnyFreeSpec with Matchers:

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
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\use{base}\n" + src + "\n"))
    t.end()
    doc.shipped.toSeq

  private def chars(b: Box): List[CharBox] = b match
    case c: CharBox => List(c)
    case h: HBox    => h.boxes.toList.flatMap(chars)
    case v: VBox    => v.boxes.toList.flatMap(chars)
    case _          => Nil

  private def allChars(boxes: Seq[Box]): List[CharBox] = boxes.toList.flatMap(chars)
  private def text(boxes: Seq[Box]): String            = allChars(boxes).map(_.text).mkString
  private def fontOf(boxes: Seq[Box], mark: String): Font =
    allChars(boxes).collectFirst { case c if c.text.contains(mark) => c.font }.get

  "base sets a 10pt body font so text has a family to set in" in {
    fontOf(render("X"), "X").size shouldBe (10.0 +- 1e-9)
    fontOf(render("X"), "X").typeface shouldBe "lmroman"
  }

  "the size ladder changes the type size for the rest of the group and reverts" in {
    val boxes = render("{\\large X} Y")
    fontOf(boxes, "X").size shouldBe (12.0 +- 1e-9)
    fontOf(boxes, "Y").size shouldBe (10.0 +- 1e-9)
  }

  "the size scale follows article's 10pt option" in {
    fontOf(render("{\\tiny X}"), "X").size shouldBe (5.0 +- 1e-9)
    fontOf(render("{\\Large X}"), "X").size shouldBe (14.0 +- 1e-9)
    fontOf(render("{\\Huge X}"), "X").size shouldBe (25.0 +- 1e-9)
  }

  "inline markup sets the shape and reverts to the body font afterwards" in {
    fontOf(render("\\italic{X} Y"), "X").style should contain("italic")
    fontOf(render("\\bold{X} Y"), "X").style should contain("bold")
    fontOf(render("\\smallcaps{X} Y"), "X").style should contain("smallcaps")
    fontOf(render("\\italic{X} Y"), "Y").style should not contain "italic"
  }

  "\\centerline sets its content and the TeX logo expands" in {
    text(render("\\centerline{Hi}")) should include("Hi")
    // the logo's lowered E sits in a shifted box the char walker does not descend into; the T and X on the
    // baseline are enough to show the macro expanded and set letters
    text(render("\\TeX")) should (include("T") and include("X"))
  }

  "the center environment sets its body" in {
    text(render("\\begin{center}Middle\\end{center}")) should include("Middle")
  }
