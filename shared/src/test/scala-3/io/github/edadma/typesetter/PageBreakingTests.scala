package io.github.edadma.typesetter

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
    val t = new StubTypesetter
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
    // the page is glue-set to vsize: 40 + 20 + 40
    doc.shipped(0).height shouldBe 100.0 +- 1e-9
    doc.shipped(0).boxes.map(_.height) shouldBe Seq(40.0, 20.0, 40.0)
    // the break glue was discarded; only the third line carried over
    pm.length shouldBe 1
    pm.size shouldBe 40.0 +- 1e-9
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

  "ending the document builds the final page only once" in {
    // done() must ship the box result already built — rebuilding would double the work and, because glue
    // setting replaces the glue in place, mis-report the second pass as a glueless off-size box
    val t   = new StubTypesetter
    val out = new java.io.ByteArrayOutputStream

    Console.withOut(out) {
      t.add(new CharBox(t, "one"))
      t.end()
    }

    out.toString.linesIterator.count(_.startsWith("Warning")) shouldBe 1
  }

  "interline glue is inserted across a penalty, measured from the last box" in {
    val t = new StubTypesetter
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
