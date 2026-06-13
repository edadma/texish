package io.github.edadma.typesetter.texish

import io.github.edadma.typesetter.StubTypesetter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Running headers and footers: a document-defined headline/footline macro is evaluated at shipout time for every
  * page, with pageno holding the shipping page's number. The header is drawn headsep above the body, the footer
  * footskip below it.
  */
class HeaderFooterTests extends AnyFreeSpec with Matchers:

  private class RecordingTypesetter extends StubTypesetter:
    val drawn = new ArrayBuffer[(String, Double, Double)]
    override def drawString(text: String, x: Double, y: Double): Unit = drawn += ((text, x, y))

  private def fixture(): (RecordingTypesetter, Processor) =
    val t       = new RecordingTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  "the headline macro is evaluated at shipout with the shipping page's number" in quietly {
    val (t, proc) = fixture()

    proc.process("\\def headline {heading \\the\\pageno \\hfil}\nbody\n\n\\vfill\\eject more")
    t.end()

    val texts = t.drawn.map(_._1)
    texts should contain("1")
    texts should contain("2")
    // both header instances say "heading"
    texts.count(_ == "heading") shouldBe 2
  }

  "the header is drawn above the body, the footer below it" in quietly {
    val (t, proc) = fixture()
    val voffset   = 72.0 // engine default: 1in margins, 9in body

    proc.process("\\def headline {heading \\hfil}\\def footline {\\hfil footing}\nbody\n\n\\vfill")
    t.end()

    val header = t.drawn.find(_._1 == "heading").get
    val body   = t.drawn.find(_._1 == "body").get
    val footer = t.drawn.find(_._1 == "footing").get

    header._3 should be < voffset
    body._3 should be >= voffset
    footer._3 should be > voffset + 9 * 72
  }

  "without headline or footline macros nothing extra is drawn" in quietly {
    val (t, proc) = fixture()

    proc.process("body")
    t.end()

    t.drawn.map(_._1) shouldBe ArrayBuffer("body")
  }

  "a newline pending at shipout does not leak a space into the header" in quietly {
    val (t, proc) = fixture()

    // the page ships at an \eject preceded by a single newline: the pending newline would have produced an
    // interword space at the next text, and must not put one at the front of the header instead
    proc.process("\\def headline {heading words \\hfil}\nbody\n\\vfill\\eject")
    t.end()

    val texts = t.drawn.map(_._1)
    texts should contain("heading")
    texts should contain("words")
    texts.count(_.startsWith(" ")) shouldBe 0
  }

  "shipout during a paragraph contribution evaluates the header without disturbing the body" in quietly {
    val (t, proc) = fixture()
    t.set("vsize", 60.0)

    // a long paragraph overflows the page while its lines are being contributed: the page ships (and the
    // header hbox is built on a temporary mode) mid-contribution, and every body word must still come out
    // exactly once
    val words = (1 to 200).map(i => s"word$i").mkString(" ")

    proc.process(s"\\def headline {heading \\hfil}\n$words")
    t.end()

    val pages = t.getDocument.printedPages.length

    pages should be > 1
    t.drawn.map(_._1).count(_ == "heading") shouldBe pages
    for i <- 1 to 200 do t.drawn.map(_._1).count(_ == s"word$i") shouldBe 1
  }
