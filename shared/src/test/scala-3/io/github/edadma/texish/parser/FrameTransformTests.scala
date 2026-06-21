package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, CharBox, Color, FrameBox, HeadlessTypesetter, RaiseBox, TransformBox, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The graphics-transform and framing boxes: \fbox/\framebox/\colorbox/\fcolorbox (FrameBox), \rotatebox/
  * \scalebox/\reflectbox/\resizebox (TransformBox), and \raisebox (RaiseBox). The HeadlessTypesetter gives every
  * character width 6 and metrics ascent 8 / descent 2, so a two-character body is 12 wide, 8 above and 2 below
  * the baseline — the reference content for the metric arithmetic below.
  */
class FrameTransformTests extends AnyFreeSpec with Matchers:

  private val tol = 1e-9

  /** Records every box handed to `add`, so an end-to-end \fbox{…} etc. can be checked: the box finally placed in
    * the outer list is the last add of the operation. */
  private class Capture extends HeadlessTypesetter:
    val added = ArrayBuffer[Box]()
    override infix def add(box: Box): Typesetter =
      added += box
      super.add(box)

  /** Records the drawing seam, to check the transform/fill calls a box issues at render time. Op names are bare
    * (no stringified Double) so equality is free of the JS/JVM `Double.toString` difference; the one numeric
    * check it needs — the scale factors — is kept separately. */
  private class Recording extends HeadlessTypesetter:
    val log                = ArrayBuffer[String]()
    var lastScale: (Double, Double) = (0, 0)
    override def gsave(): Unit                           = log += "gsave"
    override def grestore(): Unit                        = log += "grestore"
    override def translate(dx: Double, dy: Double): Unit = log += "translate"
    override def scale(sx: Double, sy: Double): Unit     = { lastScale = (sx, sy); log += "scale" }
    override def rotate(r: Double): Unit                 = log += "rotate"
    override def setColor(c: Color): Unit                = log += "setColor"
    override def fillRect(x: Double, y: Double, w: Double, h: Double): Unit = log += "fillRect"

  private def fixture(): (Capture, Processor) =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  private def inner(t: Typesetter): CharBox = new CharBox(t, "AB", t.currentFont, t.currentColor)

  "FrameBox metrics" - {
    "\\fbox grows the box by sep + rule on every side" in {
      val t = new HeadlessTypesetter
      val b = new FrameBox(inner(t), sep = 3, rule = 0.4, frameColor = Color("black"), bgColor = null)
      b.width shouldBe (12 + 2 * 3.4 +- tol) // 18.8
      b.ascent shouldBe (8 + 3.4 +- tol)     // 11.4
      b.descent shouldBe (2 + 3.4 +- tol)    // 5.4
    }

    "\\colorbox (no frame) grows the box by sep only" in {
      val t = new HeadlessTypesetter
      val b = new FrameBox(inner(t), sep = 3, rule = 0.4, frameColor = null, bgColor = Color("yellow"))
      b.width shouldBe (12 + 6.0 +- tol)
      b.ascent shouldBe (8 + 3.0 +- tol)
      b.descent shouldBe (2 + 3.0 +- tol)
    }
  }

  "TransformBox metrics" - {
    "\\rotatebox{90} swaps width and height about the baseline" in {
      val t = new HeadlessTypesetter
      val c = math.cos(math.Pi / 2)
      val s = math.sin(math.Pi / 2)
      val b = new TransformBox(inner(t), c, s, -s, c, _ => ())
      b.width shouldBe (10.0 +- 1e-6)   // original height 8 + 2
      b.ascent shouldBe (12.0 +- 1e-6)  // original width
      b.descent shouldBe (0.0 +- 1e-6)
    }

    "\\scalebox{2} scales every metric uniformly" in {
      val t = new HeadlessTypesetter
      val b = new TransformBox(inner(t), 2, 0, 0, 2, _ => ())
      b.width shouldBe (24.0 +- tol)
      b.ascent shouldBe (16.0 +- tol)
      b.descent shouldBe (4.0 +- tol)
    }

    "\\scalebox{2}[3] scales the axes independently" in {
      val t = new HeadlessTypesetter
      val b = new TransformBox(inner(t), 2, 0, 0, 3, _ => ())
      b.width shouldBe (24.0 +- tol)
      b.ascent shouldBe (24.0 +- tol)
      b.descent shouldBe (6.0 +- tol)
    }

    "\\reflectbox keeps the metrics, mirroring only the drawing" in {
      val t = new HeadlessTypesetter
      val b = new TransformBox(inner(t), -1, 0, 0, 1, _ => ())
      b.width shouldBe (12.0 +- tol)
      b.ascent shouldBe (8.0 +- tol)
      b.descent shouldBe (2.0 +- tol)
    }
  }

  "RaiseBox adjusts height and depth to the lifted position" in {
    val t = new HeadlessTypesetter
    val b = new RaiseBox(inner(t), lift = 5)
    b.ascent shouldBe (13.0 +- tol) // 8 + 5
    b.descent shouldBe (0.0 +- tol) // max(0, 2 - 5)
    b.width shouldBe (12.0 +- tol)
  }

  "end to end" - {
    "\\fbox places a FrameBox with frame padding" in {
      val (t, proc) = fixture()
      proc.process("\\fbox{AB}")
      val fb = t.added.last.asInstanceOf[FrameBox]
      fb.width shouldBe (18.8 +- tol)
    }

    "\\framebox is an alias of \\fbox" in {
      val (t, proc) = fixture()
      proc.process("\\framebox{AB}")
      t.added.last shouldBe a[FrameBox]
    }

    "\\colorbox reads a colour name then the body" in {
      val (t, proc) = fixture()
      proc.process("\\colorbox{yellow}{AB}")
      val fb = t.added.last.asInstanceOf[FrameBox]
      fb.width shouldBe (18.0 +- tol) // sep only, no rule
    }

    "\\fcolorbox reads frame and background colours" in {
      val (t, proc) = fixture()
      proc.process("\\fcolorbox{black}{yellow}{AB}")
      t.added.last shouldBe a[FrameBox]
    }

    "\\rotatebox produces a TransformBox sized to the rotation" in {
      val (t, proc) = fixture()
      proc.process("\\rotatebox{90}{AB}")
      val tb = t.added.last.asInstanceOf[TransformBox]
      tb.width shouldBe (10.0 +- 1e-6)
      tb.ascent shouldBe (12.0 +- 1e-6)
    }

    "\\scalebox with an optional vertical factor" in {
      val (t, proc) = fixture()
      proc.process("\\scalebox{2}[3]{AB}")
      val tb = t.added.last.asInstanceOf[TransformBox]
      tb.width shouldBe (24.0 +- tol)
      tb.ascent shouldBe (24.0 +- tol)
    }

    "\\resizebox to a width with ! preserves the aspect ratio" in {
      val (t, proc) = fixture()
      proc.process("\\resizebox{24pt}{!}{AB}")
      val tb = t.added.last.asInstanceOf[TransformBox]
      tb.width shouldBe (24.0 +- tol)  // scaled ×2
      tb.ascent shouldBe (16.0 +- tol) // same factor on the height
    }

    "\\reflectbox mirrors with unchanged metrics" in {
      val (t, proc) = fixture()
      proc.process("\\reflectbox{AB}")
      val tb = t.added.last.asInstanceOf[TransformBox]
      tb.width shouldBe (12.0 +- tol)
    }

    "\\raisebox lifts the content and reshapes the box" in {
      val (t, proc) = fixture()
      proc.process("\\raisebox{5pt}{AB}")
      val rb = t.added.last.asInstanceOf[RaiseBox]
      rb.ascent shouldBe (13.0 +- tol)
      rb.descent shouldBe (0.0 +- tol)
    }
  }

  "draw seam" - {
    "a TransformBox brackets its drawing with gsave/translate/scale/grestore" in {
      val t = new Recording
      val b = new TransformBox(inner(t), 2, 0, 0, 2, _.scale(2, 2))
      b.draw(t, 0, 0)
      t.log.head shouldBe "gsave"
      t.log.last shouldBe "grestore"
      t.log should contain("scale")
      t.lastScale shouldBe (2.0, 2.0)
    }

    "an \\fbox draws four rule edges around the content" in {
      val t = new HeadlessTypesetter
      val rec = new Recording
      val b   = new FrameBox(inner(t), sep = 3, rule = 0.4, frameColor = Color("black"), bgColor = null)
      b.draw(rec, 0, 0)
      rec.log.count(_ == "fillRect") shouldBe 4 // top, bottom, left, right rules
    }
  }
