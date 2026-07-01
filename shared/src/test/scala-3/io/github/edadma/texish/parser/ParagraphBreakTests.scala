package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, CharBox, DocumentMode, HBox, HeadlessTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The \par primitive ends the current paragraph — the explicit form of the blank line that ordinarily
  * breaks one. A document is run through the engine over the metrics-only HeadlessTypesetter and the set
  * lines are read back, so a paragraph break shows up as one word landing on its own line. */
class ParagraphBreakTests extends AnyFreeSpec with Matchers:

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

  // Every set line, in order, as the text of the character boxes it holds — one entry per paragraph line.
  private def lines(src: String): List[String] =
    def collect(b: Box): List[HBox] = b match
      case h: HBox if h.boxes.exists(_.isInstanceOf[CharBox]) => List(h)
      case h: HBox                                            => h.boxes.toList.flatMap(collect)
      case v: VBox                                            => v.boxes.toList.flatMap(collect)
      case _                                                  => Nil
    def text(b: Box): String = b match
      case c: CharBox => c.text
      case h: HBox    => h.boxes.toList.map(text).mkString
      case _          => ""
    render(src).toList.flatMap(collect).map(text)

  "\\par splits a run into two paragraphs, each on its own line" in {
    val out = lines("alpha\\par beta")
    out.length shouldBe 2
    out.head should include("alpha")
    out(1) should include("beta")
  }

  "\\par produces the same break as a blank line" in {
    lines("alpha\\par beta") shouldBe lines("alpha\n\nbeta")
  }

  "without \\par the two words stay on one line" in {
    lines("alpha beta").length shouldBe 1
  }

  "\\par with no open paragraph is a no-op, not an error" in {
    val out = lines("\\par gamma")
    out.length shouldBe 1
    out.head should include("gamma")
  }
