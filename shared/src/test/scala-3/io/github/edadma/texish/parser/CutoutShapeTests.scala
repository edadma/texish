package io.github.edadma.texish.parser

import io.github.edadma.texish.{Builder, CharBox, CutoutShape, HBox, HeadlessTypesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream

/** Shaped cutouts: a wrapped figure's text can follow a silhouette rather than its bounding box. The profile maps a
  * depth below the figure's top to the inset text keeps there, so a disc narrows the lines at its middle and relaxes
  * toward the margin at its top and foot. These check the analytic profiles directly and that a `\cutshape` actually
  * curves the running text — while a rectangular shape stays bit-identical to the original `\cutout`.
  */
class CutoutShapeTests extends AnyFreeSpec with Matchers:

  "the ellipse profile is widest at the middle and tapers to the half-width at the ends" in {
    val p = CutoutShape.profile(CutoutShape.Ellipse, extent = 100, height = 100, gutter = 0)
    p(50) shouldBe (100.0 +- 1e-6) // the vertical middle hugs the full extent
    p(0) shouldBe (50.0 +- 1e-6)   // the top edge clears at the disc's half-width
    p(100) shouldBe (50.0 +- 1e-6) // and the foot does too
    p(50) should be > p(10)        // it bulges through the middle
  }

  "a triangledown profile is wide at the top and tapers to the gutter at the foot" in {
    val p = CutoutShape.profile(CutoutShape.TriangleDown, extent = 80, height = 100, gutter = 5)
    p(0) shouldBe (85.0 +- 1e-6)   // full extent plus gutter at the top
    p(50) shouldBe (45.0 +- 1e-6)  // half-way down, half the extent remains
    p(100) shouldBe (5.0 +- 1e-6)  // only the gutter at the point
  }

  "the rectangle profile is the constant extent-plus-gutter at every depth" in {
    val p = CutoutShape.profile(CutoutShape.Rectangle, extent = 70, height = 100, gutter = 12)
    p(0) shouldBe (82.0 +- 1e-6)
    p(100) shouldBe (82.0 +- 1e-6)
  }

  "a shape name is parsed case-insensitively and falls back to a rectangle" in {
    CutoutShape.named("ellipse") shouldBe CutoutShape.Ellipse
    CutoutShape.named("CIRCLE") shouldBe CutoutShape.Ellipse
    CutoutShape.named("triangledown") shouldBe CutoutShape.TriangleDown
    CutoutShape.named("squircle") shouldBe CutoutShape.Rectangle
  }

  private def leadingWidth(line: HBox): Double =
    line.boxes.takeWhile(!_.isInstanceOf[CharBox]).map(_.width).sum

  private def lineWidths(src: String): List[Double] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new ByteArrayOutputStream) {
      proc.process(src)
      t.paragraph()
    }
    t.mode.asInstanceOf[Builder].list.collect { case h: HBox => leadingWidth(h) }.toList

  "a left ellipse cutout curves the text: widest at the disc's middle, flush below it" in {
    val filler = "The quick brown fox jumps over the lazy dog. " * 40
    val widths = lineWidths("\\cutshape{ellipse}{120}{120}{l}\\noindent " + filler)
    widths.length should be > 12
    val widest = widths.max
    widest should be >= 119.0      // a line at the disc's vertical middle hugs the full extent
    widths.head should be < widest // the first line, near the top of the disc, is narrower
    widths.last should be < 5.0    // text below the disc returns flush to the margin
  }

  "a rect cutshape narrows the text identically to a plain rectangular cutout" in {
    val filler = "\\noindent " + ("The quick brown fox jumps over the lazy dog. " * 20)
    val shaped = lineWidths("\\cutshape{rect}{100}{100}{l}" + filler)
    val plain  = lineWidths("\\cutout{100}{100}{l}" + filler)
    shaped shouldBe plain
  }
