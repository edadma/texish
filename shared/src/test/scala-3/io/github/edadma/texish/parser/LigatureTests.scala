package io.github.edadma.texish.parser

import io.github.edadma.texish.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** TeX-style input representations turn ASCII source into proper typography: --- becomes an em dash, -- an en
  * dash, the paired `` and '' become curly double quotes, and ... an ellipsis. The engine has always known the
  * mapping; the body font (Latin Modern Roman) now enables it, so these fire in ordinary running text. The
  * arrow representations stay off in text — an ordinary text font has no glyphs for them.
  */
class LigatureTests extends AnyFreeSpec with Matchers:

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

  "the body font enables the text representations and excludes the math arrows" in {
    val ligs = Ligatures.TEXT_REPRESENTATIONS
    ligs should contain(`EM DASH`)
    ligs should contain(`EN DASH`)
    ligs should contain(`LEFT DOUBLE QUOTATION MARK`)
    ligs should contain(`HORIZONTAL ELLIPSIS`)
    ligs should not contain `RIGHTWARDS ARROW` // arrows are math glyphs, off in text
  }

  "--- sets an em dash and -- an en dash" in {
    allText(render("a---b")) should include(`EM DASH`)
    allText(render("a--b")) should include(`EN DASH`)
  }

  "paired `` and '' set curly double quotes" in {
    val out = allText(render("``hi''"))
    out should include(`LEFT DOUBLE QUOTATION MARK`)
    out should include(`RIGHT DOUBLE QUOTATION MARK`)
  }

  "... sets a horizontal ellipsis" in {
    allText(render("wait...")) should include(`HORIZONTAL ELLIPSIS`)
  }
