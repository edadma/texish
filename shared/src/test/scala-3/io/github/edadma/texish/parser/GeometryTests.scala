package io.github.edadma.texish.parser

import io.github.edadma.texish.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The `\geometry` primitive resolves the page frame from texish name:value options and sets the underlying
  * variables — the sheet (paperwidth/paperheight), the text size (hsize/vsize), and the top-left offset
  * (hoffset/voffset). The far margin is always `paper − offset − size`, so the cases below check the frame closes
  * the way LaTeX's geometry package would. Lengths are in points (72 per inch); the A-series sizes come out in
  * millimetre-derived points.
  */
class GeometryTests extends AnyFreeSpec with Matchers:

  /** Run a fragment and read back the resolved page variables. */
  private def geom(src: String): Map[String, Double] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.process(src)
    Seq("paperwidth", "paperheight", "hsize", "vsize", "hoffset", "voffset", "headsep")
      .map(n => n -> t.getNumber(n))
      .toMap

  private val in = 72.0
  private val mm = (72.0 / 2.54) / 10

  "an equal margin sets all four margins and the text fills the rest" in {
    val g = geom("\\geometry margin:2in")
    g("hoffset") shouldBe (2 * in +- 1e-9)
    g("voffset") shouldBe (2 * in +- 1e-9)
    g("hsize") shouldBe (8.5 * in - 4 * in +- 1e-9)  // letter width minus 2in each side
    g("vsize") shouldBe (11 * in - 4 * in +- 1e-9)
  }

  "a named paper size sets the sheet" in {
    val g = geom("\\geometry paper:a4")
    g("paperwidth") shouldBe (210 * mm +- 1e-6)
    g("paperheight") shouldBe (297 * mm +- 1e-6)
  }

  "paper and margin compose, in any unit" in {
    val g = geom("\\geometry paper:a4 margin:2cm")
    g("hoffset") shouldBe (20 * mm +- 1e-6)
    g("hsize") shouldBe (210 * mm - 40 * mm +- 1e-6)
    g("vsize") shouldBe (297 * mm - 40 * mm +- 1e-6)
  }

  "two edge margins make the text the remainder" in {
    val g = geom("\\geometry left:1.5in right:1in")
    g("hoffset") shouldBe (1.5 * in +- 1e-9)
    g("hsize") shouldBe (8.5 * in - 1.5 * in - 1 * in +- 1e-9)
    g("voffset") shouldBe (1 * in +- 1e-9)  // vertical axis untouched, keeps the default 1in
  }

  "a text width with centering splits the slack evenly" in {
    val g = geom("\\geometry textwidth:6in centering:on")
    g("hsize") shouldBe (6 * in +- 1e-9)
    g("hoffset") shouldBe ((8.5 * in - 6 * in) / 2 +- 1e-9)
  }

  "landscape swaps the sheet so width exceeds height" in {
    val g = geom("\\geometry paper:a4 landscape:on")
    g("paperwidth") shouldBe (297 * mm +- 1e-6)
    g("paperheight") shouldBe (210 * mm +- 1e-6)
    g("paperwidth") should be > g("paperheight")
  }

  "headsep passes straight through" in {
    geom("\\geometry headsep:18pt")("headsep") shouldBe (18.0 +- 1e-9)
  }

  "the settings survive a group (they are global, like a LaTeX preamble)" in {
    geom("{\\geometry margin:2in}")("hsize") shouldBe (8.5 * in - 4 * in +- 1e-9)
  }

  "over-constrained margins that leave no text width are an error" in {
    a[ParserException] should be thrownBy geom("\\geometry margin:5in")
  }
