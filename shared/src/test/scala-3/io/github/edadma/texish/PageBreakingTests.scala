package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Page breaking follows TeX's legal-breakpoint rules: a break is allowed at a glue whose predecessor is
  * non-discardable or at a penalty below Penalty.Inhibit; a penalty at or below Penalty.Force ships the page
  * immediately; the break item and any discardables after it are dropped at the top of the new page.
  */
class PageBreakingTests extends AnyFreeSpec with Matchers:

  private class Line(val ascent: Double, val descent: Double = 0) extends ContentBox:
    val width: Double    = 10
    val xAdvance: Double = 10
    def draw(t: Typesetter, x: Double, y: Double): Unit = ()

  private class Rule(val ascent: Double) extends NoGlueBox:
    val descent: Double  = 0
    val width: Double    = 10
    val xAdvance: Double = 10
    def draw(t: Typesetter, x: Double, y: Double): Unit = ()

  private class CapturingDocument(t: Typesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      page += 1

  private def setup(vsize: Double): (CapturingDocument, PageMode) =
    val t = new HeadlessTypesetter
    t.set("vsize", vsize)
    val doc = new CapturingDocument(t)
    t.document = doc
    (doc, new PageMode(t))

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  private def lines(box: VBox): Int = box.boxes.count(_.isInstanceOf[Line])

  "overflow breaks at the latest glue with a non-discardable predecessor" in quietly {
    val (doc, pm) = setup(100)

    pm add Line(40)
    pm add Glue(10, 5, 5)
    pm add Line(40)
    pm add Glue(10, 5, 5)
    pm add Line(40) // 140 > 100: break at the second glue

    doc.shipped.length shouldBe 1
    // the page is glue-set to vsize: 40 + (19 set glue + 1 lineskip interline) + 40. The interline glue sits
    // between the explicit glue and the second line — \vskip adds to the leading, it does not replace it.
    doc.shipped(0).height shouldBe 100.0 +- 1e-9
    doc.shipped(0).boxes.map(_.height) shouldBe Seq(40.0, 19.0, 1.0, 40.0)
    // the break glue was discarded; only the third line carried over
    pm.length shouldBe 1
    pm.size shouldBe 40.0 +- 1e-9
  }

  "a glue between two lines adds the interline glue rather than replacing it" in quietly {
    // TeX's \prevdepth passes through glue, so an explicit \vskip between two lines adds to the leading rather
    // than replacing it: the interline glue is still inserted before the second line. The earlier code read the
    // last list item, so the \vskip suppressed the interline and jammed the lines together.
    val (doc, pm) = setup(100)
    pm add Line(40)
    pm add Glue(10, 5, 5) // an explicit \vskip between the two lines
    pm add Line(40)
    pm add Glue(10, 5, 5)
    pm add Line(40) // overflow ships the first two lines
    // the shipped page is Line, set-glue, interline, Line — four items, not three; the \vskip did not eat the interline
    doc.shipped(0).boxes.length shouldBe 4
  }

  "the new page never starts with discardables" in quietly {
    val (doc, pm) = setup(100)

    pm add Line(40)
    pm add Glue(10, 5, 5)
    pm add Glue(60, 5, 5) // overflow with nothing but glue after the only legal breakpoint

    doc.shipped.length shouldBe 1
    lines(doc.shipped(0)) shouldBe 1
    pm.isEmpty shouldBe true
  }

  "glue preceded by glue is not a breakpoint" in quietly {
    val (doc, pm) = setup(100)

    pm add Line(90)
    pm add Glue(5, 0, 0)
    pm add Glue(20, 0, 0) // overflows; only the first glue is legal

    doc.shipped.length shouldBe 1
    lines(doc.shipped(0)) shouldBe 1
  }

  "an inhibiting penalty holds the surrounding boxes together" in quietly {
    val (doc, pm) = setup(100)

    pm add Line(40)
    pm add Glue(10, 5, 5)
    pm add Line(40)
    pm add Penalty(Penalty.Inhibit)
    pm add Glue(10, 5, 5)
    pm add Line(40) // overflow: the glue after the penalty is illegal, so the break backs up to the first glue

    doc.shipped.length shouldBe 1
    lines(doc.shipped(0)) shouldBe 1
    // both protected lines moved to the next page together
    pm.list.count(_.isInstanceOf[Line]) shouldBe 2
  }

  "a penalty below Inhibit allows a break where none would otherwise exist" in quietly {
    val (doc, pm) = setup(50)

    pm add Rule(40)
    pm add Penalty(0)
    pm add Rule(40) // rules suppress interline glue: the penalty is the only legal breakpoint

    doc.shipped.length shouldBe 1
    doc.shipped(0).boxes.count(_.isInstanceOf[Rule]) shouldBe 1
    pm.length shouldBe 1
  }

  "with no legal breakpoint the material accumulates rather than breaking illegally" in quietly {
    val (doc, pm) = setup(50)

    pm add Rule(40)
    pm add Rule(40) // overfull, but two butted boxes cannot be broken apart

    doc.shipped shouldBe empty
    pm.length shouldBe 2
  }

  "a forcing penalty ships the page immediately" in quietly {
    val (doc, pm) = setup(100)

    pm add Line(40)
    pm add Penalty(Penalty.Force)

    doc.shipped.length shouldBe 1
    pm.isEmpty shouldBe true
  }

  "a forcing penalty on an empty page does nothing" in quietly {
    val (doc, pm) = setup(100)

    pm add Line(40)
    pm add Penalty(Penalty.Force)
    pm add Penalty(Penalty.Force)

    doc.shipped.length shouldBe 1
  }

  "material spanning several pages cascades into repeated breaks" in quietly {
    val (doc, pm) = setup(100)

    for i <- 1 to 7 do
      if i > 1 then pm add Glue(10, 5, 5)
      pm add Line(40)

    pm.newpage()

    doc.shipped.length shouldBe 4
    doc.shipped.map(lines) shouldBe Seq(2, 2, 2, 1)
  }

  "topskip pads the page top so first baselines align" in quietly {
    val (doc, pm) = setup(100)

    pm.t.set("topskip", Glue(20))
    pm add Line(8, 2)
    pm.newpage()
    pm add Line(15, 2)
    pm.newpage()

    // pad + ascent = topskip on both pages, whatever the first line's ascent
    doc.shipped(0).boxes.head shouldBe a[VSpaceBox]
    doc.shipped(0).boxes.head.height shouldBe 12.0 +- 1e-9
    doc.shipped(1).boxes.head.height shouldBe 5.0 +- 1e-9
  }

  "a first line taller than topskip gets no pad" in quietly {
    val (doc, pm) = setup(100)

    pm.t.set("topskip", Glue(20))
    pm add Line(40)
    pm.newpage()

    doc.shipped(0).boxes.head should not be a[VSpaceBox]
  }

  "raggedbottom ends a short page quietly" in {
    val (doc, pm) = setup(100)

    pm.t.set("raggedbottom", 1.0)

    val out = new java.io.ByteArrayOutputStream

    Console.withOut(out) {
      pm add Line(40)
      pm.newpage()
    }

    doc.shipped(0).height shouldBe 100.0 +- 1e-9
    out.toString shouldBe empty
  }

  "ending the document builds the final page only once" in {
    // done() must ship the final page exactly once; a regression that rebuilds or re-ships would ship it twice
    val t = new HeadlessTypesetter

    Console.withOut(new java.io.ByteArrayOutputStream) {
      t.add(new CharBox(t, "one"))
      t.end()
    }

    t.document.page shouldBe 1
  }

  "the final under-full page keeps its content at the top (flush-bottom default)" in {
    // raggedbottom defaults to 0, so a full page is justified — but the last page is under-full, and
    // ending the document must cap it with a fil (plain TeX's \bye) so its content stays at the top
    // rather than the tiny interparagraph glue being stretched down the whole page.
    val t = new HeadlessTypesetter
    t.set("vsize", 300)
    val doc = new CapturingDocument(t)
    t.document = doc

    Console.withOut(new java.io.ByteArrayOutputStream) {
      t.add(new CharBox(t, "one"))
      t.paragraph()
      t.add(new CharBox(t, "two"))
      t.end()
    }

    doc.shipped.length shouldBe 1
    val page = doc.shipped(0)
    page.height shouldBe 300.0 +- 1e-9
    // the slack is one big space at the very bottom, not spread between the two paragraphs
    page.boxes.last shouldBe a[VSpaceBox]
    page.boxes.last.height should be > 150.0
  }

  "the greedy fallback backs up to the last space instead of stranding a box" in {
    // A box that fits at a line's end, before a box that does not, must move to the next line with it —
    // the greedy fallback (forced here by tolerance 0, with no \emergencystretch second pass) backs up to the
    // last interword space rather than leaving the fitting box (an opening "(", say) stranded at the line end.
    class W(val width: Double) extends ContentBox:
      val xAdvance: Double = width
      val ascent: Double   = 8
      val descent: Double  = 2
      def draw(t: Typesetter, x: Double, y: Double): Unit = ()

    val t = new HeadlessTypesetter
    t.set("vsize", 5000); t.set("tolerance", 0.0); t.set("parindent", 0.0); t.set("hsize", 100.0)
    val open = new W(5)
    val wide = new W(60)
    val doc  = new CapturingDocument(t)
    t.document = doc

    Console.withOut(new java.io.ByteArrayOutputStream) {
      t.start // enter paragraph (horizontal) mode, as prose does
      t.add(new W(30)); t.glue(4, 2, 1); t.add(new W(30)); t.glue(4, 2, 1)
      t.add(open); t.add(wide)
      t.end()
    }

    def contents(line: Box): Seq[Box] = line match
      case h: HBox => h.boxes.toSeq
      case b       => Seq(b)
    val lines    = doc.shipped(0).boxes.filterNot(_.isInstanceOf[VSpaceBox])
    val openLine = lines.find(l => contents(l).contains(open))
    openLine.map(l => contents(l).contains(wide)) shouldBe Some(true)
  }

  "interline glue is inserted across a penalty, measured from the last box" in {
    val t = new HeadlessTypesetter
    t.set("baselineskip", Glue(20))
    t.set("lineskiplimit", 0.0)

    val vb = new VBoxBuilder(t)
    vb add Line(5, 5)
    vb add Penalty(150)
    vb add Line(5, 5)

    val box = vb.result.asInstanceOf[VBox]
    // box, penalty, then baselineskip glue (20 - descent 5 - ascent 5 = 10), then box
    box.boxes.map(_.height) shouldBe Seq(10.0, 0.0, 10.0, 10.0)
    box.boxes(1) shouldBe a[Penalty]
    box.boxes(2) shouldBe a[VSpaceBox]
  }
