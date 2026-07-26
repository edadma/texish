package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, CharBox, Color, DocumentMode, HBox, HeadlessTypesetter, InsertBox, ShiftBox, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** A footnote resets the pen before setting its body, so that a marker falling inside a coloured span (the red
  * words of Jesus, say) does not tint the note at the foot. What it must reset *to* is the pen at document level
  * — not literal black, which is unreadable on a page a previewer has inverted to a light ink, and wrong for a
  * document that simply chose another body colour.
  */
class FootnoteColorTests extends AnyFreeSpec with Matchers:

  private val Ink = Color("#e9ecef")

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def fixture(ink: Color): (HeadlessTypesetter, CapturingDocument, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    t.currentColor = ink
    val doc = new CapturingDocument(t)
    t.document = doc
    (t, doc, proc)

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  // Every text run inside a box, paired with the colour it will be painted in.
  private def inks(box: Box): List[(String, Color)] = box match
    case v: VBox     => v.boxes.toList.flatMap(inks)
    case h: HBox     => h.boxes.toList.flatMap(inks)
    case s: ShiftBox => inks(s.box)
    case i: InsertBox => inks(i.content)
    case c: CharBox  => List(c.text -> c.color)
    case _           => Nil

  private def noteInks(t: HeadlessTypesetter): List[(String, Color)] =
    t.mode
      .asInstanceOf[io.github.edadma.texish.Builder]
      .list
      .toList
      .collect { case i: InsertBox => i }
      .flatMap(inks)

  "a footnote body is set in the document's ink, not literal black" in quietly {
    val (t, _, proc) = fixture(Ink)

    proc.process("alpha \\footnote{down low} beta")
    t.paragraph()

    val body = noteInks(t)
    body should not be empty
    body.map(_._2).distinct shouldBe List(Ink)
  }

  // \textcolor is the local form — it opens a group and tints only its body — so this is the "red words of
  // Jesus" case: the marker sits inside the coloured span, the note at the foot must not.
  "a footnote marker inside a coloured span still sets the note in the document's ink" in quietly {
    val (t, _, proc) = fixture(Ink)

    proc.process("alpha \\textcolor{red}{tinted \\footnote{down low}} beta")
    t.paragraph()

    val body = noteInks(t)
    body should not be empty
    body.map(_._2).distinct shouldBe List(Ink)
  }

  // The case that decides between resetting to a stored default and reading the pen at document level: here the
  // ink is the document's own choice, which no application-supplied default would know about.
  "a footnote follows an ink the document chose for itself" in quietly {
    val (t, _, proc) = fixture(Color("black"))

    proc.process("\\color{darkblue}\nalpha \\footnote{down low} beta")
    t.paragraph()

    val body = noteInks(t)
    body should not be empty
    body.map(_._2).distinct shouldBe List(Color("darkblue"))
  }

  // The two mechanisms together: a local span nested inside the document's own ink. The note takes the outer
  // ink, which is neither the application's default nor the span's colour.
  "a footnote inside a span nested in a document ink takes the document ink" in quietly {
    val (t, _, proc) = fixture(Color("black"))

    proc.process("\\color{darkblue}\nalpha \\textcolor{red}{tinted \\footnote{down low}} beta")
    t.paragraph()

    val body = noteInks(t)
    body should not be empty
    body.map(_._2).distinct shouldBe List(Color("darkblue"))
  }

  "the text after a footnote keeps the colour it had before the note" in quietly {
    val (t, _, proc) = fixture(Ink)

    proc.process("alpha \\footnote{down low} beta")

    t.currentColor shouldBe Ink
  }

  // The running foot is built at shipout by the page decorator, from whatever pen is current then and inside its
  // own group. It shares the page with the note, so a document that inverts the page needs both in the ink. The
  // decorator is invoked directly here: DocumentMode.add sees the page body, and the header and footer are only
  // composed onto it afterwards, so a capturing document never sees them.
  "a running foot is set in the document's ink" in quietly {
    val (t, _, proc) = fixture(Ink)

    proc.process("\\def footline {\\the\\pageno}\n")
    val (_, foot) = t.pageDecorator()

    val folio = inks(foot).filter(_._1.exists(_.isDigit))
    folio should not be empty
    folio.map(_._2).distinct shouldBe List(Ink)
  }
