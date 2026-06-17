package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, CharBox, Color, DocumentMode, HBox, HeadlessTypesetter, LinkBox, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** \href and \url build a LinkBox that, on a backend with link annotations, brackets its content's drawing
  * with begin/end markers carrying the URL. The URL is read verbatim, so a // (otherwise a line comment) and
  * other specials survive intact. The recording typesetter captures the begin/draw/end order.
  */
class LinkTests extends AnyFreeSpec with Matchers:

  /** A stub that records the link begin/end markers and the text drawn between them, in order. */
  private class RecordingTypesetter extends HeadlessTypesetter:
    val events                                                    = new ArrayBuffer[String]
    override def beginLink(uri: String): Unit                     = events += s"begin:$uri"
    override def endLink(): Unit                                  = events += "end"
    override def drawString(s: String, x: Double, y: Double): Unit = if s.nonEmpty then events += s"draw:$s"

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def render(t: HeadlessTypesetter, src: String): Seq[Box] =
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\set raggedbottom {1}\n" + src + "\n"))
    t.end()
    doc.shipped.toSeq

  private def links(b: Box): List[LinkBox] = b match
    case l: LinkBox => l :: links(l.box)
    case h: HBox    => h.boxes.toList.flatMap(links)
    case v: VBox    => v.boxes.toList.flatMap(links)
    case _          => Nil

  private def chars(b: Box): List[CharBox] = b match
    case c: CharBox => List(c)
    case l: LinkBox => chars(l.box)
    case h: HBox    => h.boxes.toList.flatMap(chars)
    case v: VBox    => v.boxes.toList.flatMap(chars)
    case _          => Nil

  private def firstLink(src: String): LinkBox = render(new RecordingTypesetter, src).flatMap(links).head

  "\\href wraps its display text in a LinkBox carrying the URL, with the // intact" in {
    val link = firstLink("\\href{https://example.org/a}{site}")
    link.uri shouldBe "https://example.org/a"
  }

  "\\href colours its display text blue" in {
    val link = firstLink("\\href{https://example.org}{site}")
    chars(link).foreach(_.color shouldBe Color("blue"))
  }

  "\\url typesets the address itself, in the mono face, as a link to itself" in {
    val link = firstLink("\\url{https://example.org/path}")
    link.uri shouldBe "https://example.org/path"
    val cs = chars(link)
    cs.map(_.text).mkString shouldBe "https://example.org/path"
    cs.foreach(_.font.typeface shouldBe "mono")
  }

  "a URL keeps a tilde and percent verbatim rather than treating them as active or comment" in {
    val link = firstLink("\\url{https://example.org/~bob/100%off}")
    link.uri shouldBe "https://example.org/~bob/100%off"
  }

  "the LinkBox has the same metrics as the body it wraps" in {
    val link = firstLink("\\href{https://example.org}{site}")
    link.width shouldBe link.box.width
    link.ascent shouldBe link.box.ascent
    link.descent shouldBe link.box.descent
  }

  "drawing a LinkBox brackets the body's drawing with the link markers" in {
    val t     = new RecordingTypesetter
    val boxes = render(t, "\\href{https://example.org}{Hi}")
    val link  = boxes.flatMap(links).head
    t.events.clear()
    link.draw(t, 0, 0)
    t.events.head shouldBe "begin:https://example.org"
    t.events.last shouldBe "end"
    t.events should contain("draw:Hi")
  }
