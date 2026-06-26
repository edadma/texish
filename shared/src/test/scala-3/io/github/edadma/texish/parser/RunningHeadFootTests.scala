package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, CharBox, DocumentMode, HBox, HeadlessTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** A running header or footer runs its macro at shipout to build the head/foot box, and any `\font`, `\set` or
  * colour change it makes must stay local to that box. A `\def footline {\font … \the\pageno}` once left the body
  * font — and the baselineskip that `\font` derives from it — changed for everything typeset after the page
  * shipped, so the first page to carry a footer (e.g. the page after a number-suppressed cover) came out in the
  * body font, and the next page in the footer's smaller font and tighter leading.
  */
class RunningHeadFootTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def render(src: String): Seq[VBox] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(src + "\n"))
    t.end()
    doc.shipped.toSeq

  private def charSizes(b: Box): Seq[Double] = b match
    case c: CharBox => Seq(c.font.size)
    case h: HBox    => h.boxes.toList.flatMap(charSizes)
    case v: VBox    => v.boxes.toList.flatMap(charSizes)
    case _          => Nil

  "a footer's font change does not leak into the body of the next page" in {
    val pages = render(
      "\\font lmroman 12 regular\n" +
        "\\def footline {\\font lmroman 30 regular \\the\\pageno}\n" +
        "Apple.\n\\eject\nBanana.\n",
    )

    pages.length should be >= 2
    // The footer drawn for page one ran a \font 30; page two's body, set afterwards, must still be the 12pt body
    // font (the footer box itself is drawn separately and is not part of the captured page content).
    val page2 = charSizes(pages(1))
    page2 should not be empty
    all(page2) shouldBe 12.0
  }
