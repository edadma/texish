package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, Color, PictureBox, PictureOp, HeadlessTypesetter, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The `plot` package (packages/plot.texish) drawn through the full parser onto the picture layer. These check
  * that the data-to-device transform maps series points to the expected picture coordinates and that the frame
  * (axes, zero line) and the auto colour cycle / auto-ranging behave, so a regression in the package — or in the
  * engine pieces it leans on (\round, the comparison primitives, numeric coercion, calc resolution inside
  * coordinate groups) — is caught here rather than only in a rendered PDF.
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
    val ops = run(s"$preamble \\plot{ \\lineplot[royalblue]{0 0  10 100} }")
    ops should contain(PictureOp.MoveTo(46, 42))
    ops should contain(PictureOp.LineTo(280, 204))
  }

  "the frame draws the two axes at the area edges" in {
    val ops = run(s"$preamble \\plot{ \\lineplot[black]{0 0  10 100} }")
    // x axis: (46,42) -> (280,42);  y axis: (46,42) -> (46,204)
    ops should contain(PictureOp.MoveTo(46, 42))
    ops should contain(PictureOp.LineTo(280, 42))
    ops should contain(PictureOp.LineTo(46, 204))
  }

  "scatter places a filled marker circle at each data point" in {
    val ops = run(s"$preamble \\plot{ \\scatter[crimson]{5 50} }")
    // (5,50) -> (46 + 5*23.4, 42 + 50*1.62) = (163, 123), marker radius 2.6
    ops should contain(PictureOp.Arc(163, 123, 2.6, 0, 2 * math.Pi, false))
  }

  "a forced tick step is honoured" in {
    // xstep 5 over 0..10 -> ticks at x = 0,5,10 -> device x = 46,163,280 each drawn as a tick mark
    val ops = run(s"$preamble \\xstep{5} \\plot{ \\lineplot[black]{0 0  10 100} }")
    val tickXs = ops.collect { case PictureOp.MoveTo(x, y) if y == 42 => x }
    tickXs should contain(163.0)
  }

  "auto colour cycles through the palette across series" in {
    // No explicit colour: first series takes palette[0] = royalblue, second palette[1] = crimson.
    val ops    = run(s"$preamble \\plot{ \\lineplot{0 0  10 100}  \\lineplot{0 50  10 50} }")
    val strokes = ops.collect { case PictureOp.Paint(None, Some(c)) => c }
    strokes should contain(Color("royalblue"))
    strokes should contain(Color("crimson"))
  }

  "an explicit colour overrides the cycle" in {
    val ops     = run(s"$preamble \\plot{ \\lineplot[seagreen]{0 0  10 100} }")
    val strokes = ops.collect { case PictureOp.Paint(None, Some(c)) => c }
    strokes should contain(Color("seagreen"))
  }

  "a square marker shape lowers to a rectangle, not an arc" in {
    val ops = run(s"$preamble \\set plotmarkshape {square} \\plot{ \\scatter[crimson]{5 50} }")
    ops.collect { case a: PictureOp.Arc => a } shouldBe empty
    // square of side 2*2.6 centred on (163,123): corner (160.4, 120.4)
    ops should contain(PictureOp.MoveTo(160.4, 120.4))
  }

  "autorange derives the ranges from the data" in {
    // data x in [0,10], y all >= 0 so the y baseline is pinned to 0; (10,0) -> (280,42).
    val ops = run("\\use{plot}\\set plotgrid {0}\\autorange{0 0  10 0}\\plot{ \\lineplot[black]{0 0  10 0} }")
    ops should contain(PictureOp.MoveTo(46, 42))
    ops should contain(PictureOp.LineTo(280, 42))
  }

  "a zero axis is drawn when the y range straddles zero" in {
    // yrange -50..50: y=0 maps to 42 + 50/100*162 = 123, a full-width line distinct from the data.
    val ops = run("\\use{plot}\\xrange{0}{10}\\yrange{-50}{50}\\set plotgrid {0}\\plot{ \\lineplot[black]{0 -50  10 50} }")
    ops should contain(PictureOp.MoveTo(46, 123))
    ops should contain(PictureOp.LineTo(280, 123))
  }

  "a labelled series and \\legend render without error" in {
    val ops = run(s"$preamble \\plot{ \\lineplot[royalblue][Observed]{0 0  10 100} \\scatter[crimson][Sampled]{5 50} \\legend }")
    // the legend marker swatch adds a second arc beyond the one data marker
    ops.collect { case a: PictureOp.Arc => a }.size should be >= 2
  }

  "reference lines map through the transform" in {
    // \hline{50} -> device y = 42 + 50*1.62 = 123, full width
    val ops = run(s"$preamble \\plot{ \\hline{50} }")
    ops should contain(PictureOp.MoveTo(46, 123))
    ops should contain(PictureOp.LineTo(280, 123))
  }

  "the series body is clipped to the data area by default" in {
    run(s"$preamble \\plot{ \\lineplot[black]{0 0  10 100} }") should contain(PictureOp.Clip)
  }

  "clipping can be turned off" in {
    run(s"$preamble \\set plotclip {0} \\plot{ \\lineplot[black]{0 0  10 100} }") should not contain PictureOp.Clip
  }

  "areaplot fills a region closed to the baseline" in {
    val ops = run(s"$preamble \\plot{ \\areaplot[royalblue]{0 0  10 100} }")
    ops should contain(PictureOp.MoveTo(46, 42))          // start at (xmin, baseline 0)
    ops should contain(PictureOp.Close)
    ops.collect { case PictureOp.Paint(Some(c), None) => c } should contain(Color("royalblue"))
  }

  "stepplot draws a staircase: horizontal then vertical" in {
    val ops = run(s"$preamble \\plot{ \\stepplot[black]{0 0  10 100} }")
    ops should contain(PictureOp.MoveTo(46, 42))
    ops should contain(PictureOp.LineTo(280, 42))   // horizontal to new x at the old y
    ops should contain(PictureOp.LineTo(280, 204))  // then vertical to the new y
  }

  "a categorical x axis places one tick per named category" in {
    // \xcategories sets xrange 0.5..3.5; Sx = 234/3 = 78; categories at x=1,2,3 -> 85,163,241
    val ops = run("\\use{plot}\\set plotgrid {0}\\yrange{0}{160}\\xcategories{A B C}\\plot{ \\bars[teal]{1 95  2 110  3 80} }")
    val catTickXs = ops.collect { case PictureOp.MoveTo(x, y) if y == 42 => x }
    catTickXs should contain allOf (85.0, 163.0, 241.0)
  }

  "bar value labels render without error when enabled" in {
    noException should be thrownBy run(s"$preamble \\set plotvalues {1} \\plot{ \\bars[teal]{1 50  2 80} }")
  }

  "error bars draw a capped whisker at each point" in {
    val ops    = run(s"$preamble \\plot{ \\errorbars[black]{5 50 10} }")
    val moveXs = ops.collect { case PictureOp.MoveTo(x, _) => x }
    moveXs should contain(163.0) // whisker at x=5 -> device 163
    moveXs should contain(160.0) // a cap starts at 163 - ploterrcap(3)
  }

  "a trend line fits the least-squares slope and intercept" in {
    // y = 5x exactly: slope 5, intercept 0; the fit line (0,0)->(10,50) maps to (46,42)-(280,123)
    val ops = run(s"$preamble \\plot{ \\trendline[black]{0 0  1 5  2 10  3 15} }")
    ops should contain(PictureOp.MoveTo(46, 42))
    ops should contain(PictureOp.LineTo(280, 123))
  }

  "minor ticks add shorter marks between the majors" in {
    // minor y tick length = plotticklen*0.6 = 2.4, so a minor y tick ends at x = 46 - 2.4 = 43.6
    val ops = run(s"$preamble \\set plotyminor {2} \\plot{ \\lineplot[black]{0 0  10 100} }")
    ops.collect { case PictureOp.LineTo(x, _) => x } should contain(43.6)
  }

  "without minor subdivisions there are no minor ticks" in {
    val ops = run(s"$preamble \\plot{ \\lineplot[black]{0 0  10 100} }")
    ops.collect { case PictureOp.LineTo(x, _) => x } should not contain 43.6
  }

  "tick-label formatting renders without error" in {
    noException should be thrownBy run(s"$preamble \\ytickformat{}{k} \\plot{ \\lineplot[black]{0 0  10 100} }")
  }
