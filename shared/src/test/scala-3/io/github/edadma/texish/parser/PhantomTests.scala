package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, DocumentMode, HBox, HeadlessTypesetter, PhantomBox, Typesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** \phantom and its kin reserve an argument's size along selected axes without its full ink. The PhantomBox is
  * checked directly for which dimensions it reports and whether it draws, and the primitives are checked
  * end-to-end: \smash keeps the glyph's ink, \phantom drops it.
  */
class PhantomTests extends AnyFreeSpec with Matchers:

  // a stub inner box with distinct, recognizable dimensions, and a draw that records it was asked to draw
  private class Probe extends Box:
    var drawn            = false
    val ascent: Double   = 7.0
    val descent: Double  = 3.0
    val width: Double    = 10.0
    val xAdvance: Double = 10.0
    val isSpace: Boolean = false
    def draw(t: Typesetter, x: Double, y: Double): Unit = drawn = true

  "\\phantom keeps both axes and draws nothing" in {
    val ph = new PhantomBox(new Probe, keepWidth = true, keepHeight = true, visible = false)
    (ph.width, ph.ascent, ph.descent) shouldBe (10.0, 7.0, 3.0)
  }

  "\\hphantom keeps only the width" in {
    val ph = new PhantomBox(new Probe, keepWidth = true, keepHeight = false, visible = false)
    (ph.width, ph.ascent, ph.descent) shouldBe (10.0, 0.0, 0.0)
  }

  "\\vphantom keeps only the height and depth" in {
    val ph = new PhantomBox(new Probe, keepWidth = false, keepHeight = true, visible = false)
    (ph.width, ph.ascent, ph.descent) shouldBe (0.0, 7.0, 3.0)
  }

  "\\smash keeps the width, zeroes height and depth, and still draws its ink" in {
    val probe = new Probe
    val ph    = new PhantomBox(probe, keepWidth = true, keepHeight = false, visible = true)
    (ph.width, ph.ascent, ph.descent) shouldBe (10.0, 0.0, 0.0)
    ph.draw(new HeadlessTypesetter, 0, 0)
    probe.drawn shouldBe true
  }

  "a phantom draws nothing even though it reserves space" in {
    val probe = new Probe
    new PhantomBox(probe, keepWidth = true, keepHeight = true, visible = false).draw(new HeadlessTypesetter, 0, 0)
    probe.drawn shouldBe false
  }

  // ---- the primitives end-to-end ---------------------------------------------

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

  private def phantoms(b: Box): List[PhantomBox] = b match
    case p: PhantomBox => List(p)
    case h: HBox       => h.boxes.toList.flatMap(phantoms)
    case v: VBox       => v.boxes.toList.flatMap(phantoms)
    case _             => Nil

  private def allPhantoms(boxes: Seq[Box]): List[PhantomBox] = boxes.toList.flatMap(phantoms)

  "\\phantom reserves its argument's width in the running text" in {
    val ph = allPhantoms(render("\\phantom{Mg}"))
    ph should have size 1
    ph.head.width should be > 0.0
    ph.head.ascent should be > 0.0
  }

  "\\vphantom reserves height but no width" in {
    val ph = allPhantoms(render("\\vphantom{Mg}"))
    ph should have size 1
    ph.head.width shouldBe 0.0
    ph.head.ascent should be > 0.0
  }

  "\\smash reserves width but no height" in {
    val ph = allPhantoms(render("\\smash{Mg}"))
    ph should have size 1
    ph.head.width should be > 0.0
    ph.head.ascent shouldBe 0.0
    ph.head.descent shouldBe 0.0
  }
