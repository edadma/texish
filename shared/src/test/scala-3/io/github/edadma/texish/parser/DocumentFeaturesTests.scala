package io.github.edadma.texish.parser

import io.github.edadma.texish.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The LaTeX-flavoured conveniences the `document` package (packages/document.texish) layers on the engine:
  * size-changing declarations, the \textXX inline markup, text symbols, starred and deeper section headings,
  * the description list, alignment environments and \appendix. Each leans on an engine primitive added for it
  * (\fontsize, \slanted, the small-caps body cut, the macro star flag), so a regression in either the package
  * or the primitive it rides on is caught here rather than only in a rendered PDF.
  */
class DocumentFeaturesTests extends AnyFreeSpec with Matchers:

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
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\use{document}\n" + src + "\n"))
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

  "size declarations change the type size for the rest of the group and revert" in {
    val boxes = render("{\\large X} Y")
    fontOf(boxes, "X").size shouldBe (12.0 +- 1e-9)
    fontOf(boxes, "Y").size shouldBe (10.0 +- 1e-9)
  }

  "the size scale follows article's 10pt option" in {
    fontOf(render("{\\tiny X}"), "X").size shouldBe (5.0 +- 1e-9)
    fontOf(render("{\\footnotesize X}"), "X").size shouldBe (8.0 +- 1e-9)
    fontOf(render("{\\Large X}"), "X").size shouldBe (14.0 +- 1e-9)
    fontOf(render("{\\Huge X}"), "X").size shouldBe (25.0 +- 1e-9)
  }

  "\\textsl sets the slanted shape, \\textsc small caps, \\textsf the sans family" in {
    fontOf(render("\\textsl{X} Y"), "X").style should contain("slanted")
    fontOf(render("\\textsc{X} Y"), "X").style should contain("smallcaps")
    fontOf(render("\\textsf{X} Y"), "X").typeface shouldBe "noto"
  }

  "inline markup reverts to the body font afterwards" in {
    val boxes = render("\\textsl{X} Y")
    fontOf(boxes, "Y").style should not contain "slanted"
    fontOf(boxes, "Y").typeface shouldBe "lmroman"
  }

  "text symbols emit their Unicode character" in {
    text(render("\\S")) should include("§")
    text(render("\\P")) should include("¶")
    text(render("\\dag\\ddag")) should (include("†") and include("‡"))
    text(render("\\copyright\\pounds")) should (include("©") and include("£"))
    text(render("\\textemdash\\textendash")) should (include("—") and include("–"))
  }

  "a numbered section prints its number; the starred form does not" in {
    text(render("\\section{Intro}")) should include("1")
    text(render("\\section*{Intro}")) should not include "1"
  }

  "subsubsection numbers within section.subsection.subsubsection" in {
    val boxes = render("\\section{A}\\subsection{B}\\subsubsection{C}")
    text(boxes) should include("1.1.1")
  }

  "\\appendix switches the section number to a letter" in {
    val boxes = render("\\section{One}\\appendix\\section{Data}")
    text(boxes) should (include("1") and include("A"))
  }

  "a description item uses its bracketed label, set bold, as the marker" in {
    val boxes = render("\\begin{description}\\item[Term] body\\end{description}")
    text(boxes) should include("Term")
    fontOf(boxes, "T").style should contain("bold")
  }

  "a plain \\item with no label still uses the list marker" in {
    val boxes = render("\\begin{itemize}\\item one\\end{itemize}")
    text(boxes) should include("•")
  }
