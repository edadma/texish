package io.github.edadma.texish

import scala.collection.mutable.ArrayBuffer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Phase 2 of the picture-graphics layer: the [[PictureMode]] display-list collector and the [[PictureBox]]
  * that replays it. These are backend-independent, so they run on the headless [[StubTypesetter]] — op
  * collection is checked against the recorded list directly, and replay is checked through a typesetter that
  * logs every seam call instead of rasterizing. Pixel-level rendering is exercised separately on the Cairo and
  * Graphics2D backends.
  */
class PictureTests extends AnyFreeSpec with Matchers:

  /** A recorded seam call. Structured (not stringified) so equality is exact and free of the platform-specific
    * `Double.toString` formatting that differs between the JVM and Scala.js. */
  private enum Seam:
    case GSave, GRestore, NewPath, ClosePath, Stroke
    case SetColor(r: Int, g: Int, b: Int)
    case Translate(x: Double, y: Double)
    case Scale(x: Double, y: Double)
    case MoveTo(x: Double, y: Double)
    case LineTo(x: Double, y: Double)
    case Fill(preserve: Boolean)
    case FillRect(x: Double, y: Double, w: Double, h: Double)

  /** Records the picture seam (plus colour and the leaf fillRect a placed [[RuleBox]] draws with) so a
    * [[PictureBox]] replay can be asserted against an exact call sequence. */
  private class RecordingTypesetter extends StubTypesetter:
    val log = ArrayBuffer[Seam]()

    override def setColor(color: Color): Unit            = log += Seam.SetColor(color.redInt, color.greenInt, color.blueInt)
    override def gsave(): Unit                           = log += Seam.GSave
    override def grestore(): Unit                        = log += Seam.GRestore
    override def translate(dx: Double, dy: Double): Unit = log += Seam.Translate(dx, dy)
    override def scale(sx: Double, sy: Double): Unit     = log += Seam.Scale(sx, sy)
    override def newPath(): Unit                         = log += Seam.NewPath
    override def moveTo(x: Double, y: Double): Unit      = log += Seam.MoveTo(x, y)
    override def lineTo(x: Double, y: Double): Unit      = log += Seam.LineTo(x, y)
    override def closePath(): Unit                       = log += Seam.ClosePath
    override def fillPath(preserve: Boolean): Unit       = log += Seam.Fill(preserve)
    override def strokePath(): Unit                      = log += Seam.Stroke
    override def fillRect(x: Double, y: Double, w: Double, h: Double): Unit = log += Seam.FillRect(x, y, w, h)

  private def mode(w: Double = 100, h: Double = 50): PictureMode = new PictureMode(new StubTypesetter, w, h)

  "PictureMode collects a display list" - {

    "a rectangle lowers to a fresh path, four sides, a close, and one paint" in {
      val m = mode()
      m.setFill(Color("red"))
      m.setStroke(Color("blue"))
      m.rect(0, 0, 10, 20)

      m.displayList shouldBe Vector(
        PictureOp.NewPath,
        PictureOp.MoveTo(0, 0),
        PictureOp.LineTo(10, 0),
        PictureOp.LineTo(10, 20),
        PictureOp.LineTo(0, 20),
        PictureOp.Close,
        PictureOp.Paint(Some(Color("red")), Some(Color("blue"))),
      )
    }

    "the active fill and stroke colours are baked into each paint as it is issued" in {
      val m = mode()
      m.setFill(Color("red"))
      m.rect(0, 0, 1, 1) // stroke still off
      m.setStroke(Color("green"))
      m.noFill()
      m.rect(0, 0, 1, 1)

      val paints = m.displayList.collect { case p: PictureOp.Paint => p }
      paints shouldBe Vector(
        PictureOp.Paint(Some(Color("red")), None),
        PictureOp.Paint(None, Some(Color("green"))),
      )
    }

    "the current point tracks the last segment and drives relative resolution" in {
      val m = mode()
      m.moveTo(5, 5)
      m.currentPoint shouldBe (5.0, 5.0)
      m.rel(2, 3, relative = true) shouldBe (7.0, 8.0)
      m.rel(2, 3, relative = false) shouldBe (2.0, 3.0)
      m.lineTo(20, 30)
      m.currentPoint shouldBe (20.0, 30.0)
    }

    "a group brackets its body with gsave/grestore and restores the colour state" in {
      val m = mode()
      m.setFill(Color("red"))
      m.groupBegin()
      m.setFill(Color("green"))
      m.rect(0, 0, 1, 1) // filled green inside the group
      m.groupEnd()
      m.rect(0, 0, 1, 1) // filled red again outside

      val ops = m.displayList
      ops.head shouldBe PictureOp.GSave
      ops should contain(PictureOp.GRestore)
      // the fill set inside the group does not leak past it: the second rect paints red again
      val paints = ops.collect { case p: PictureOp.Paint => p }
      paints shouldBe Vector(
        PictureOp.Paint(Some(Color("green")), None),
        PictureOp.Paint(Some(Color("red")), None),
      )
    }

    "circle lowers to an arc path" in {
      val m = mode()
      m.setStroke(Color("black"))
      m.circle(10, 10, 5)

      m.displayList shouldBe Vector(
        PictureOp.NewPath,
        PictureOp.Arc(10, 10, 5, 0, 2 * math.Pi, false),
        PictureOp.Close,
        PictureOp.Paint(None, Some(Color("black"))),
      )
    }

    "result is a PictureBox of the declared size, sitting on the baseline" in {
      val m   = mode(120, 60)
      val box = m.result
      box shouldBe a[PictureBox]
      val pb = box.asInstanceOf[PictureBox]
      pb.width shouldBe 120.0
      pb.ascent shouldBe 60.0
      pb.descent shouldBe 0.0
    }
  }

  "PictureBox replays the display list" - {

    "it installs the y-up frame at the draw origin, then paints" in {
      val t = new RecordingTypesetter
      val m = new PictureMode(t, 100, 50)
      m.setStroke(Color("blue"))
      m.setLineWidth(2)
      m.line(0, 0, 10, 10)
      val pb = m.result.asInstanceOf[PictureBox]

      pb.draw(t, 200, 300)

      t.log.take(3) shouldBe ArrayBuffer(Seam.GSave, Seam.Translate(200, 300), Seam.Scale(1, -1))
      t.log.last shouldBe Seam.GRestore
      t.log should contain(Seam.SetColor(0, 0, 255)) // blue
      t.log should contain(Seam.Stroke)
    }

    "a fill-and-stroke paint fills with preserve then strokes" in {
      val t = new RecordingTypesetter
      val m = new PictureMode(t, 100, 50)
      m.setFill(Color("red"))
      m.setStroke(Color("blue"))
      m.rect(0, 0, 10, 10)
      m.result.asInstanceOf[PictureBox].draw(t, 0, 0)

      val painting = t.log.dropWhile(_ != Seam.Fill(true))
      painting.take(4) shouldBe ArrayBuffer(Seam.Fill(true), Seam.SetColor(0, 0, 255), Seam.Stroke, Seam.GRestore) // blue
    }

    "a placed box draws upright with the anchor handle on the point" in {
      val t = new RecordingTypesetter
      val m = new PictureMode(t, 100, 50)
      // a 20-wide rule, 10 above and 4 below its baseline, placed centred on (30,40)
      val rule = new RuleBox(t, 20, 10, 4, Color("black"))
      m.place(rule, Anchor.Center, 30, 40)
      m.result.asInstanceOf[PictureBox].draw(t, 0, 0)

      // the place op un-flips: translate to the point, scale(1,-1), then the rule draws at its anchor offset.
      val placed = t.log.dropWhile(_ != Seam.Translate(30, 40))
      placed.take(2) shouldBe ArrayBuffer(Seam.Translate(30, 40), Seam.Scale(1, -1))
      // Center anchor → bx = -width/2 = -10, by = (ascent-descent)/2 = 3; RuleBox fills (bx, by-ascent, w, h).
      t.log should contain(Seam.FillRect(-10, -7, 20, 14))
    }
  }
