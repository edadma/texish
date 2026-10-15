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

  "every print-on-demand trim size resolves to its inch dimensions" in {
    val trims = Seq(
      "5x8"       -> (5.0, 8.0),
      "5.06x7.81" -> (5.06, 7.81),
      "5.25x8"    -> (5.25, 8.0),
      "5.5x8.5"   -> (5.5, 8.5),
      "6x9"       -> (6.0, 9.0),
      "6.14x9.21" -> (6.14, 9.21),
      "6.69x9.61" -> (6.69, 9.61),
      "7x10"      -> (7.0, 10.0),
      "7.44x9.69" -> (7.44, 9.69),
      "7.5x9.25"  -> (7.5, 9.25),
      "8x10"      -> (8.0, 10.0),
      "8.25x6"    -> (8.25, 6.0),
      "8.25x8.25" -> (8.25, 8.25),
      "8.5x8.5"   -> (8.5, 8.5),
      "8.27x11.69" -> (8.27, 11.69),
      "8.5x11"    -> (8.5, 11.0),
    )
    for (name, (w, h)) <- trims do
      val g = geom(s"\\geometry paper:$name margin:0.5in")
      withClue(name) {
        g("paperwidth") shouldBe (w * in +- 1e-6)
        g("paperheight") shouldBe (h * in +- 1e-6)
      }
  }

  "the trade names are aliases for trim sizes" in {
    val aliases = Seq(
      "us-trade"     -> (6.0, 9.0),
      "digest"       -> (5.5, 8.5),
      "royal"        -> (6.14, 9.21),
      "crown-quarto" -> (7.44, 9.69),
      "executive"    -> (7.0, 10.0),
    )
    for (name, (w, h)) <- aliases do
      val g = geom(s"\\geometry paper:$name margin:0.5in")
      withClue(name) {
        g("paperwidth") shouldBe (w * in +- 1e-6)
        g("paperheight") shouldBe (h * in +- 1e-6)
      }
  }

  "a width-by-height sheet takes an optional unit" in {
    val g = geom("\\geometry paper:148x210mm margin:10mm")
    g("paperwidth") shouldBe (148 * mm +- 1e-6)
    g("paperheight") shouldBe (210 * mm +- 1e-6)
    val c = geom("\\geometry paper:15x20cm margin:1cm")
    c("paperwidth") shouldBe (150 * mm +- 1e-6)
    c("paperheight") shouldBe (200 * mm +- 1e-6)
  }

  "landscape swaps a trim size" in {
    val g = geom("\\geometry paper:6x9 landscape:on margin:0.5in")
    g("paperwidth") shouldBe (9 * in +- 1e-6)
    g("paperheight") shouldBe (6 * in +- 1e-6)
  }

  "an unknown or malformed paper name is an error that lists what is accepted" in {
    for bad <- Seq("quarto", "6x", "x9", "6x9furlong", "0x9") do
      val e = the[TexishException] thrownBy geom(s"\\geometry paper:$bad")
      withClue(bad) {
        e.getMessage should include("unknown paper size")
        e.getMessage should include("us-trade")
        e.getMessage should include("<width>x<height>")
      }
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
    a[TexishException] should be thrownBy geom("\\geometry margin:5in")
  }
