package io.github.edadma.texish.parser

import io.github.edadma.texish.{
  Box,
  Builder,
  CharBox,
  DocumentMode,
  Glue,
  HBox,
  HeadlessTypesetter,
  OverlayBox,
  Penalty,
  Typesetter,
  VBox,
  VerticalBox,
  VerticalMode,
}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Cross-page robustness for wrapped figures. A cutout narrows the lines a figure flows beside; these check that a
  * page break never lands inside that band (the figure would otherwise strand its text on the next page), that a
  * cutout shipped with its page stops narrowing later pages, and that a wrap pushed wholesale to a later page is
  * rebased into that page's coordinate frame so it narrows the right lines there.
  */
class WrapPageBreakTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def fixture(vsize: Double): (HeadlessTypesetter, Processor, CapturingDocument) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    t.set("vsize", vsize)
    t.set("topskip", Glue(0)) // first baseline at the box ascent, so page heights are predictable
    (t, proc, doc)

  // every line (HBox) on a shipped page, descending the page's vertical structure but not into an OverlayBox — a
  // figure's own caption lines ride inside the overlay and are not part of the body the page wraps
  private def linesOf(box: Box): List[HBox] = box match
    case h: HBox        => List(h)
    case v: VerticalBox => v.boxes.toList.flatMap(linesOf)
    case _              => Nil

  private def overlays(box: Box): Int = box match
    case _: OverlayBox  => 1
    case v: VerticalBox => v.boxes.toList.map(overlays).sum
    case h: HBox        => h.boxes.toList.map(overlays).sum
    case _              => 0

  // the glue and indent before the first character on a line: for a left wrap of width w this is about w plus the
  // wrapsep gutter, and for an unwrapped line it is at most the paragraph indent
  private def leadingWidth(line: HBox): Double =
    line.boxes.takeWhile(!_.isInstanceOf[CharBox]).map(_.width).sum

  private val Narrowed = 80.0 // a left wrap of 108pt + 12pt gutter leads by ~120; an indent is at most ~20

  private def runPages(vsize: Double, src: String): List[VBox] =
    val (t, proc, doc) = fixture(vsize)
    Console.withOut(new java.io.ByteArrayOutputStream) {
      proc.process(src)
      t.end()
    }
    doc.shipped.toList

  // a long single paragraph that breaks into many lines and spans several pages
  private def filler(reps: Int): String = ("The quick brown fox jumps over the lazy dog. " * reps)

  "a forbidden break is placed between the lines a figure makes the text wrap" in {
    // With a tall left cutout, the interior interline breaks the page builder would otherwise be free to take are
    // forbidden, so the list carries more penalties at or above the inhibit threshold than the same paragraph
    // without a cutout (which has only its club and widow penalties at the first and last breaks).
    def inhibits(src: String): Int =
      val t       = new HeadlessTypesetter
      t.set("vsize", 1.0e9) // nothing ships, so the whole paragraph's penalties stay in the page list
      val handler = new TypesetterHandler(t)
      val proc    = new Processor(handler)
      registerTypesettingPrimitives(proc, handler)
      Console.withOut(new java.io.ByteArrayOutputStream) {
        proc.process(src)
        t.paragraph()
      }
      t.mode.asInstanceOf[Builder].list.count {
        case p: Penalty => p.penalty >= Penalty.Inhibit
        case _          => false
      }

    val plain   = inhibits("\\noindent " + filler(12))
    val wrapped = inhibits("\\cutout{72}{120}{l}\\noindent " + filler(12))
    wrapped should be > plain
  }

  "a cutout that shipped with its page does not narrow the next page" in {
    // a short left cutout narrows the first lines of page one, then the body overflows onto page two; the cutout
    // shipped with page one, so it must not still be narrowing the top of page two
    val pages = runPages(200, "\\cutout{100}{30}{l}\\noindent " + filler(20))
    pages.length should be > 1
    val firstPage = linesOf(pages.head)
    firstPage.exists(leadingWidth(_) >= Narrowed) shouldBe true // page one's opening lines are narrowed
    all(linesOf(pages(1)).map(leadingWidth)) should be < Narrowed // page two is flush
  }

  "a wrapped figure carried to a later page narrows every line beside it there" in {
    // filler fills the first page on its own; the wrapfigure is anchored after it, so the whole wrap lands on a
    // later page. Its band is resolved from the figure's position on that page — not a coordinate frozen on the
    // first page — so every line the figure spans is narrowed there, and the first page (which the figure never
    // reached) carries none. A 96pt figure at the ~16.8pt baseline spans six lines; the bug this guards against
    // narrowed only the first few, because re-resolving the band per line let a mid-build page break move the
    // figure out from under the later lines.
    val src =
      "\\noindent " + filler(20) + "\n\n" +
        "\\wrapbox{l}{108}{\\picture width:108 height:96 {}}\\noindent " + filler(10)
    val pages = runPages(200, src)
    pages.length should be > 1

    overlays(pages.head) shouldBe 0
    all(linesOf(pages.head).map(leadingWidth)) should be < Narrowed

    val figurePage = pages.indexWhere(overlays(_) > 0)
    figurePage should be > 0
    pages.map(overlays).sum shouldBe 1
    linesOf(pages(figurePage)).count(leadingWidth(_) >= Narrowed) should be >= 5
  }

  "clearwrap pads the galley past a wrapped figure's foot so following content clears it" in {
    // a tall figure (80pt) with only one short line of accompanying text: the overlay reserves no height, so without
    // clearwrap the galley would sit at ~one line and the next block would ride over the figure. \clearwrap pads
    // down to the figure's foot and drops the cutout, so the galley height is at least the figure height and the
    // following content starts below it.
    val t       = new HeadlessTypesetter
    t.set("vsize", 1.0e9)
    t.set("topskip", Glue(0)) // figure sits at galley top, so its foot is at exactly its height
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream) {
      proc.process("\\wrapbox{l}{100}{\\picture width:100 height:80 {}}\none short line\n\n\\clearwrap")
      t.paragraph()
    }
    val g = t.mode.asInstanceOf[VerticalMode]
    g.naturalHeight should be >= 80.0 // padded down to the figure's foot
    g.cutouts shouldBe empty          // the wrap is finished
  }

  "a figure taller than the page is allowed to break rather than hang the page builder" in {
    // a cutout taller than the whole page would, if its interior breaks were forbidden, leave a single long
    // paragraph with nowhere legal to break; the over-tall figure is exempt, so the paragraph still paginates
    val pages = runPages(200, "\\cutout{72}{100000}{l}\\noindent " + filler(20))
    pages.length should be > 1
  }
