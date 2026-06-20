package io.github.edadma.texish

import io.github.edadma.texish.parser.{Processor, TypesetterHandler, registerTypesettingPrimitives}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The fragment-baseline metrics the in-browser math API uses to align an inline formula on the surrounding
  * text's baseline. The geometry is computed in the shared layout, so it is exercised here on the JVM through
  * the cropping SVG backend: render a one-line formula, read back where its math baseline falls within the
  * cropped box, and check it against what the formula's shape demands. */
class FragmentBaselineTests extends AnyFreeSpec with Matchers:

  private def metricsOf(src: String): FragmentMetrics =
    val t = new SvgTypesetter
    t.cropToContent = true
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.setBaseDir(".")
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(src))
    t.end()
    t.fragmentMetrics.getOrElse(fail(s"no fragment metrics for $src"))

  /** Ink below the baseline — the depth the inline element must hang under the text baseline. */
  private def depth(m: FragmentMetrics): Double = m.height - m.baseline

  "a cropped fragment reports a baseline strictly inside its box" in {
    val m = metricsOf("$x^2$")
    m.width should be > 0.0
    m.height should be > 0.0
    m.baseline should be > 0.0
    m.baseline should be < m.height
  }

  "a formula with no descender sits essentially on the baseline" in {
    // x has no descender and the superscript rides above the baseline, so almost no ink falls below it: the
    // depth is just the crop margin, not a real descent.
    val m = metricsOf("$x^2$")
    depth(m) should be < 2.5 // ≈ the 1pt crop margin
  }

  "a subscript drives the baseline up, leaving real depth below it" in {
    // x_2's subscript hangs below the baseline, so the cropped box extends well past it — the inline element
    // must be dropped by that much to keep the x on the text baseline.
    val sup = metricsOf("$x^2$")
    val sub = metricsOf("$x_2$")
    depth(sub) should be > depth(sup) + 1.0
  }

  "a fraction straddles the axis, with substantial ink both above and below the baseline" in {
    val m = metricsOf("$\\frac{a}{b}$")
    m.baseline should be > 3.0    // numerator and the bar sit well above the baseline
    depth(m) should be > 3.0      // the denominator hangs well below it
  }
