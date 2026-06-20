package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, CharBox, DocumentMode, FloatBox, HBox, HeadlessTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** \topinsert typesets its body into a block that floats to the top of whatever page it lands on: the block rides
  * the vertical list as a zero-size FloatBox that migrates out of its line, the page builder counts its height
  * against the page, and shipout lifts it above the body — the top-placement mirror of \footnote.
  */
class FloatTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def fixture(): (HeadlessTypesetter, CapturingDocument, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    (t, doc, proc)

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  private def texts(box: Box): List[String] = box match
    case v: VBox    => v.boxes.toList.flatMap(texts)
    case h: HBox    => h.boxes.toList.flatMap(texts)
    case c: CharBox => List(c.text)
    case _          => Nil

  "\\topinsert migrates the float out of the line to the vertical list" in {
    val (t, _, proc) = fixture()

    proc.process("alpha \\topinsert{boxed chart} beta gamma")
    t.paragraph()

    val items = t.mode.asInstanceOf[Builder].list

    // the float sits at the top level of the vertical list, not inside any line
    items.count(_.isInstanceOf[FloatBox]) shouldBe 1
    for line <- items.collect { case h: HBox => h } do
      line.boxes.exists(_.isInstanceOf[FloatBox]) shouldBe false

    // and it carries the typeset body
    val float = items.collectFirst { case f: FloatBox => f }.get.content
    texts(float) should (contain("boxed") and contain("chart"))
  }

  "a float heads the page its reference is on, ahead of the body text" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)

    proc.process("one \\topinsert{TOPFIG} two\n\n\\vfill\\eject after\n\n")
    t.end()

    doc.shipped.length shouldBe 2

    val page0 = texts(doc.shipped(0))
    page0.head shouldBe "TOPFIG"      // the float is the first thing on the page
    page0 should contain("one")       // the body it floated above is still there
    texts(doc.shipped(1)) should not contain "TOPFIG"
  }

  "several floats stack at the top in contribution order" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)

    proc.process("body \\topinsert{FIRST} more \\topinsert{SECOND} text\n\n")
    t.end()

    val page0 = texts(doc.shipped(0))
    page0.indexOf("FIRST") should be < page0.indexOf("SECOND")
    page0.indexOf("SECOND") should be < page0.indexOf("body")
  }

  "floated pages still build to exactly vsize, so float height counts against the page" in {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)
    t.set("vsize", 200.0)
    t.set("hsize", 200.0)

    val source =
      (1 to 8).map(i => s"\\topinsert{plot$i} word$i tail$i tag$i more$i stuff$i extra$i padding$i").mkString(" ")
    val out    = new java.io.ByteArrayOutputStream

    Console.withOut(out) {
      proc.process(source)
      t.end()
    }

    out.toString shouldBe empty
    doc.shipped.length should be > 1
    for page <- doc.shipped do page.height shouldBe 200.0 +- 1e-9

    // every float landed on some page exactly once
    for i <- 1 to 8 do doc.shipped.count(p => texts(p) contains s"plot$i") shouldBe 1
  }

  "a float that cannot share its reference page is carried to head a later one" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)
    t.set("vsize", 50.0)
    t.set("hsize", 200.0)

    // the page fills with paragraph lines, then a float is asked for near the bottom; counting its height
    // overflows the page, so the break lands before it and the float heads the next page
    proc.process("l1\n\nl2\n\nl3\n\n\\topinsert{LATE} after\n\n")
    t.end()

    doc.shipped.length should be > 1
    val onLate = doc.shipped.indexWhere(p => texts(p) contains "LATE")
    onLate should be > 0
    texts(doc.shipped(onLate)).head shouldBe "LATE"
  }

  "\\botinsert sinks the float to the foot of its page, below the body" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)

    proc.process("one \\botinsert{BOTFIG} two\n\n\\vfill\\eject after\n\n")
    t.end()

    doc.shipped.length shouldBe 2

    val page0 = texts(doc.shipped(0))
    page0.last shouldBe "BOTFIG"                    // the float is the last thing on the page
    page0.indexOf("one") should be < page0.indexOf("BOTFIG")
    texts(doc.shipped(1)) should not contain "BOTFIG"
  }

  "footnotes sit above bottom floats on the same page" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)

    proc.process("ref \\footnote{NOTE} text \\botinsert{BOTFIG} end\n\n")
    t.end()

    val page0 = texts(doc.shipped(0))
    page0.indexOf("NOTE") should be < page0.indexOf("BOTFIG")
    page0.last shouldBe "BOTFIG"
  }

  "\\midinsert keeps its block inline when the page can still hold it" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)

    proc.process("alpha\n\n\\midinsert{HEREFIG}\n\nbeta\n\n")
    t.end()

    // a here-float that fits is left where it sits, in document order — not lifted to the top of the page
    val page = texts(doc.shipped(0))
    page.indexOf("alpha") should be < page.indexOf("HEREFIG")
    page.indexOf("HEREFIG") should be < page.indexOf("beta")
  }

  "\\midinsert that cannot fit here floats to the top of a later page" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)
    t.set("vsize", 80.0)
    t.set("hsize", 200.0)

    // the page fills with lines, then a here-float too tall for the room left is asked for; it falls back to its
    // next preference (top), counts its height, overflows, and so heads the next page like a \topinsert
    proc.process("l1\n\nl2\n\nl3\n\n\\midinsert{LATEFIG\n\nrow2\n\nrow3\n\nrow4} after\n\n")
    t.end()

    doc.shipped.length should be > 1
    val onLate = doc.shipped.indexWhere(p => texts(p) contains "LATEFIG")
    onLate should be > 0
    texts(doc.shipped(onLate)).head shouldBe "LATEFIG"
  }

  "a placement spec overrides the command's default edge" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)

    proc.process("one \\topinsert[b]{OVERFIG} two\n\n\\vfill\\eject after\n\n")
    t.end()

    // [b] turns this \topinsert into a bottom float
    val page0 = texts(doc.shipped(0))
    page0.last shouldBe "OVERFIG"
    page0.indexOf("one") should be < page0.indexOf("OVERFIG")
  }

  "deferred floats keep their contribution order across pages" in quietly {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)
    t.set("vsize", 120.0)
    t.set("hsize", 200.0)

    val source = (1 to 6).map(i => s"\\topinsert{F$i} w$i x$i y$i z$i p$i q$i").mkString(" ") + "\n\n"
    proc.process(source)
    t.end()

    // the vertical list is the deferral queue: floats spilling across pages stay in contribution order
    val labels = doc.shipped.toList.flatMap(texts).filter(_.startsWith("F"))
    labels shouldBe (1 to 6).map(i => s"F$i").toList
  }

  "top and bottom floats both build the page to exactly vsize" in {
    val (t, doc, proc) = fixture()
    t.set("raggedbottom", 1.0)
    t.set("vsize", 200.0)
    t.set("hsize", 200.0)

    val source =
      (1 to 6)
        .map(i => s"\\topinsert{top$i} \\botinsert{bot$i} word$i tail$i more$i stuff$i extra$i padding$i")
        .mkString(" ")
    val out    = new java.io.ByteArrayOutputStream

    Console.withOut(out) {
      proc.process(source)
      t.end()
    }

    out.toString shouldBe empty
    for page <- doc.shipped do page.height shouldBe 200.0 +- 1e-9
    for i <- 1 to 6 do
      doc.shipped.count(p => texts(p) contains s"top$i") shouldBe 1
      doc.shipped.count(p => texts(p) contains s"bot$i") shouldBe 1
  }
