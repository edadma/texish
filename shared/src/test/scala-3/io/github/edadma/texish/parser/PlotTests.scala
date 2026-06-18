package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, PictureBox, PictureOp, HeadlessTypesetter, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The `plot` package (packages/plot.texish) drawn through the full parser onto the picture layer. These check
  * that the data-to-device transform maps series points to the expected picture coordinates and that the frame
  * (axes) is drawn, so a regression in the package — or in the engine pieces it leans on (\round, the comparison
  * primitives, calc resolution inside coordinate groups) — is caught here rather than only in a rendered PDF.
  */
class PlotTests extends AnyFreeSpec with Matchers:

  private class Capture extends HeadlessTypesetter:
    val pictures = ArrayBuffer[PictureBox]()
    override infix def add(box: Box): Typesetter =
      box match
        case pb: PictureBox => pictures += pb
        case _              =>
      super.add(box)

  private def run(src: String): Vector[PictureOp] =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.process(src)
    t.pictures should have size 1
    t.pictures.head.displayList

  // area 234 x 162, left margin 46, bottom margin 42; xrange 0..10, yrange 0..100.
  // So Sx = 23.4, Sy = 1.62, origin (46,42): (0,0) -> (46,42) and (10,100) -> (280,204).
  private val preamble = "\\use{plot}\\xrange{0}{10}\\yrange{0}{100}\\set plotgrid {0}"

  "a line series maps its data points through the transform" in {
    val ops = run(s"$preamble \\plot{ \\lineplot{royalblue}{0 0  10 100} }")
    ops should contain(PictureOp.MoveTo(46, 42))
    ops should contain(PictureOp.LineTo(280, 204))
  }

  "the frame draws the two axes at the area edges" in {
    val ops = run(s"$preamble \\plot{ \\lineplot{black}{0 0  10 100} }")
    // x axis: (46,42) -> (280,42);  y axis: (46,42) -> (46,204)
    ops should contain(PictureOp.MoveTo(46, 42))
    ops should contain(PictureOp.LineTo(280, 42))
    ops should contain(PictureOp.LineTo(46, 204))
  }

  "scatter places a filled marker circle at each data point" in {
    val ops = run(s"$preamble \\plot{ \\scatter{crimson}{5 50} }")
    // (5,50) -> (46 + 5*23.4, 42 + 50*1.62) = (163, 123), marker radius 2.4
    ops should contain(PictureOp.Arc(163, 123, 2.4, 0, 2 * math.Pi, false))
  }

  "a forced tick step is honoured" in {
    // xstep 5 over 0..10 -> ticks at x = 0,5,10 -> device x = 46,163,280 each drawn as a tick mark
    val ops = run(s"$preamble \\xstep{5} \\plot{ \\lineplot{black}{0 0  10 100} }")
    val tickXs = ops.collect { case PictureOp.MoveTo(x, y) if y == 42 => x }
    tickXs should contain(163.0)
  }
