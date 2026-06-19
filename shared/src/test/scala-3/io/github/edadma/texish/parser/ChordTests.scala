package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, HeadlessTypesetter, PictureBox, PictureOp, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The `chords` package (packages/chords.texish) drawn through the full parser onto the picture layer. A chord box
  * lowers to a grid, an optional nut or fret label, an optional barre, a mark or dot per string, and the name, so
  * these check the parts that carry the chord's meaning: one filled dot per fretted string, one open ring per open
  * string, nothing for a muted string, a barre drawn as a full-width bar that also absorbs the dots on its own
  * fret. The full-width-bar check in particular guards the value plumbing — an earlier digit-in-name bug collapsed
  * the bar to a zero-length dot.
  */
class ChordTests extends AnyFreeSpec with Matchers:

  private val DotRadius  = 4.4 // chorddot
  private val OpenRadius = 2.6 // open-string ring
  private val GridWidth  = 65.0 // 5 * chordcw
  private val BarreWidth = 8.8 // chorddot * 2

  private class Capture extends HeadlessTypesetter:
    val pictures = ArrayBuffer[PictureBox]()
    override infix def add(box: Box): Typesetter =
      box match
        case pb: PictureBox => pictures += pb
        case _              =>
      super.add(box)

  private def ops(body: String): Vector[PictureOp] =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(s"\\use{chords}$body"))
    t.pictures should have size 1
    t.pictures.head.displayList

  private def arcsOfRadius(list: Vector[PictureOp], r: Double): Int =
    list.count { case PictureOp.Arc(_, _, rr, _, _, _) => math.abs(rr - r) < 0.001; case _ => false }

  "an open chord draws one dot per fretted string and one ring per open string" in {
    // C major = x 3 2 0 1 0: three fretted strings (frets 3, 2, 1), two open (strings 4 and 6), one muted (string 1)
    val list = ops("\\chord{C}{x 3 2 0 1 0}{. 3 2 . 1 .}")
    arcsOfRadius(list, DotRadius) shouldBe 3
    arcsOfRadius(list, OpenRadius) shouldBe 2
  }

  "a muted string contributes neither a dot nor a ring" in {
    // all six strings muted: no dots, no open rings at all
    val list = ops("\\chord{X}{x x x x x x}{. . . . . .}")
    arcsOfRadius(list, DotRadius) shouldBe 0
    arcsOfRadius(list, OpenRadius) shouldBe 0
  }

  "a barre chord draws a full-width bar across its fret" in {
    // F = barre across all six strings at the first fret. The bar is the stroke issued at the barre line width;
    // its two endpoints must span the whole grid (the digit-in-name regression collapsed them to one point).
    val list = ops("\\chord[1 1 6]{F}{1 3 3 2 1 1}{1 3 4 2 1 1}")
    val barreAt = list.indexWhere {
      case PictureOp.SetLineWidth(w) => math.abs(w - BarreWidth) < 0.001
      case _                         => false
    }
    barreAt should be >= 0
    val after = list.drop(barreAt)
    val move  = after.collectFirst { case m: PictureOp.MoveTo => m }.get
    val lineE = after.collectFirst { case l: PictureOp.LineTo => l }.get
    math.abs(lineE.x - move.x) shouldBe GridWidth +- 0.001
    move.y shouldBe lineE.y +- 0.001
  }

  "a barre absorbs the dots on its own fret" in {
    // F frets its lowest and highest strings at the first fret too, but those sit under the barre — only the three
    // notes above the barre (frets 3, 3, 2) get their own dots, not six.
    val list = ops("\\chord[1 1 6]{F}{1 3 3 2 1 1}{1 3 4 2 1 1}")
    arcsOfRadius(list, DotRadius) shouldBe 3
  }
