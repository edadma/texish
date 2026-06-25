package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, CharBox, DocumentMode, FloatBox, HBox, HeadlessTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream

/** The `float` package (packages/float.texish), exercised through the full parser. It layers a document-level
  * vocabulary over the engine's float and wrap primitives: `\figure` / `\table` detach to a page edge over
  * `\topinsert`, `\wrapfigure` anchors a box that text flows around over `\wrapbox`, and `\caption` numbers either
  * one on its own per-kind counter. These check the numbering is independent per kind and that a `\wrapfigure`
  * actually narrows the lines that follow it.
  */
class FloatPackageTests extends AnyFreeSpec with Matchers:

  private def floats(src: String): List[FloatBox] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new ByteArrayOutputStream) {
      proc.process(s"\\use{float}$src")
      t.paragraph()
    }
    t.mode.asInstanceOf[Builder].list.collect { case f: FloatBox => f }.toList

  private def texts(box: Box): List[String] = box match
    case v: VBox    => v.boxes.toList.flatMap(texts)
    case h: HBox    => h.boxes.toList.flatMap(texts)
    case c: CharBox => List(c.text)
    case _          => Nil

  private def caption(f: FloatBox): String = texts(f.content).mkString(" ")

  // The leading glue before the first character on a line — for a left wrap this is the figure width plus the gutter.
  private def leadingWidth(line: HBox): Double =
    line.boxes.takeWhile(!_.isInstanceOf[CharBox]).map(_.width).sum

  private def lines(src: String): List[HBox] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new ByteArrayOutputStream) {
      proc.process(s"\\use{float}$src")
      t.paragraph()
    }
    t.mode.asInstanceOf[Builder].list.collect { case h: HBox => h }.toList

  "figures number in their own sequence, independently of tables" in {
    val fs = floats("\\figure{\\caption{First}}\\figure{\\caption{Second}}\\table{\\caption{A grid}}")
    fs.length shouldBe 3
    caption(fs(0)) should (include("Figure") and include("1") and include("First"))
    caption(fs(1)) should (include("Figure") and include("2") and include("Second"))
    // the table starts its own count at 1 rather than continuing the figures' 3 — a shared counter would print "3"
    caption(fs(2)) should (include("Table") and include("1") and include("grid"))
    caption(fs(2)) should not include "3"
  }

  "a bare caption outside any float still numbers as a figure" in {
    val fs = floats("\\figure{\\caption{Solo}}")
    caption(fs.head) should (include("Figure") and include("1"))
  }

  "a left wrapfigure narrows the lines that follow it by the figure width plus the gutter" in {
    // a 1.5in (108pt) figure taller than the text, at the default 12pt wrapsep gutter, indents every following line
    val body = "\\wrapfigure{l}{1.5in}{\\picture width:1.5in height:3in {}}\n\n" +
      "\\noindent " + ("The quick brown fox jumps over the lazy dog. " * 10)
    val ls   = lines(body)
    ls.length should be > 3
    all(ls.map(leadingWidth)) should (be >= 118.0 and be <= 122.0)
  }

  "a minipage used as a wrapfigure payload narrows the text by the box width plus the gutter" in {
    // a minipage is a rigid box, the natural payload for a rectangular wrap: a 120pt [t] minipage anchored left
    // indents the lines beside it by its own width plus the 12pt wrapsep gutter, exactly as a picture of that width
    // would — the block composes as an ordinary wrap payload. It is short, so only the lines it spans are narrowed
    // and the rest return flush to the margin.
    val body = "\\wrapfigure{l}{2in}{\\beginminipage[t]{120pt}A block of text set in its own box.\\endminipage}\n\n" +
      "\\noindent " + ("The quick brown fox jumps over the lazy dog. " * 10)
    val ws   = lines(body).map(leadingWidth)
    ws.length should be > 3
    ws.count(_ >= 130.0) should be >= 1           // the lines beside the box are indented
    ws.max should (be >= 130.0 and be <= 134.0)   // by the box width (120) plus the wrapsep gutter (12)
    ws.last should be < 5.0                        // and text below the box runs flush again
  }

  "a right wrapfigure keeps the text flush left but forces more lines" in {
    val plain   = lines("\\noindent " + ("The quick brown fox jumps over the lazy dog. " * 10))
    val wrapped = lines(
      "\\wrapfigure{r}{1.5in}{\\picture width:1.5in height:3in {}}\n\n" +
        "\\noindent " + ("The quick brown fox jumps over the lazy dog. " * 10),
    )
    all(wrapped.map(leadingWidth)) should be < 1.0
    wrapped.length should be > plain.length
  }

  // The caption of a wrapped figure rides inside the OverlayBox the wrap anchors, not the page's vertical list, so
  // its contents entry only reaches the list of figures if the shipout walk descends into the OverlayBox.
  "a wrapped figure's caption reaches the list of figures with a folio" in {
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    t.document = new DocumentMode(t)
    Console.withOut(new ByteArrayOutputStream) {
      proc.process("\\use{float}\\wrapfigure{l}{1in}{a \\caption{Wrapped fig}}\n\nbody text on the page\n\n")
      t.end()
    }
    // commit promotes the pass's pending entries to the resolved side a later \listoffigures would read
    t.references.commit()
    val lof = t.references.list("lof")
    lof.map(_.title) should contain("Wrapped fig")
    lof.find(_.title == "Wrapped fig").get.page should be > 0
  }
