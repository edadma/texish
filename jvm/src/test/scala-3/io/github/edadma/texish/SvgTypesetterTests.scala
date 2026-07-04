package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The SVG backend renders each page to a self-contained `<svg>` document, filling text and glyphs as path
  * outlines (no embedded font). These tests drive the typesetter the way the document machinery does — adding
  * boxes and ejecting pages — and check the structural shape of the emitted markup, plus the picture seam and
  * glyph seam directly.
  */
class SvgTypesetterTests extends AnyFreeSpec with Matchers:

  /** Run a body that adds boxes, swallowing the off-size page warnings the bare typesetter prints. */
  private def quiet[A](body: => A): A = Console.withOut(new java.io.ByteArrayOutputStream)(body)

  "a simple text page is a well-formed svg with a background and glyph paths" in {
    val t = new SvgTypesetter
    quiet {
      t.add(new CharBox(t, "Hello"))
      t.end()
    }

    val svgs = t.pageSvgs
    svgs.length shouldBe 1
    val svg = svgs.head

    svg should startWith("<svg")
    svg should include("xmlns=\"http://www.w3.org/2000/svg\"")
    svg should include("viewBox=\"0 0 ")
    svg should endWith("</svg>\n")
    // a painted background rect and at least one filled glyph outline
    svg should include("<rect")
    svg should include("<path d=\"M")
    // exactly one document, one closing tag — balanced
    countOf(svg, "<svg") shouldBe 1
    countOf(svg, "</svg>") shouldBe 1
  }

  "the page background honours backgroundColor" in {
    val t = new SvgTypesetter
    t.backgroundColor = Color("black")
    quiet {
      t.add(new CharBox(t, "x"))
      t.end()
    }

    val svg = t.pageSvgs.head
    // the first element is the background rect, filled with the page colour
    svg should include("<rect x=\"0\" y=\"0\"")
    svg should include("fill=\"#000000\"")
  }

  "each shipped page is its own svg document" in {
    val t = new SvgTypesetter
    quiet {
      t.add(new CharBox(t, "one"))
      t.newpage()
      t.add(new CharBox(t, "two"))
      t.end()
    }

    val svgs = t.pageSvgs
    svgs.length shouldBe 2
    svgs.foreach(_ should startWith("<svg"))
    assert(svgs(0) != svgs(1), "the two pages must not be identical")
  }

  "a rule is emitted as a filled polygon" in {
    val t = new SvgTypesetter
    quiet {
      t.add(new CharBox(t, "body"))
      t.add(RuleBox(t, 2 * t.in, 0.4, 0))
      t.end()
    }

    val svg = t.pageSvgs.head
    svg should include("<polygon points=\"")
  }

  "the glyph seam fills a single glyph's outline as a path" in {
    val t    = new SvgTypesetter
    t.init(200, 200)
    val face = t.loadFont("fonts/LatinModernRoman/lmroman10-regular.otf")
    val rf   = t.makeFont(face, 20.0)
    val gid  = t.glyphIndex(rf, 'A'.toInt)

    val page = t.createPageTarget.asInstanceOf[SvgPage]
    t.setColor(Color("blue"))
    t.drawGlyph(rf, gid, 50, 100)
    t.ejectPageTarget()

    page.svg should include("<path d=\"M")
    page.svg should include("fill=\"#0000ff\"")
  }

  "the picture seam strokes a built path and clips through a clipPath def" in {
    val t    = new SvgTypesetter
    t.init(200, 200)
    val page = t.createPageTarget.asInstanceOf[SvgPage]

    t.gsave()
    // a clip region, then a stroked line inside it
    t.newPath()
    t.moveTo(0, 0); t.lineTo(100, 0); t.lineTo(100, 100); t.closePath()
    t.clipPath()
    t.setColor(Color("red"))
    t.setLineWidth(2)
    t.newPath()
    t.moveTo(10, 10); t.lineTo(90, 90)
    t.strokePath()
    t.grestore()
    t.ejectPageTarget()

    val svg = page.svg
    svg should include("<clipPath id=\"clip0\">")
    svg should include("clip-path=\"url(#clip0)\"")
    svg should include("stroke=\"#ff0000\"")
    svg should include("stroke-width=\"2\"")
    // the clip group opened by clipPath is closed by grestore
    countOf(svg, "<g clip-path") shouldBe countOf(svg, "</g>")
  }

  "the picture seam bakes the CTM into emitted coordinates" in {
    // a unit square drawn after translate(50,50) scale(10,10) must land at device (50,50)-(60,60)
    val t    = new SvgTypesetter
    t.init(200, 200)
    val page = t.createPageTarget.asInstanceOf[SvgPage]

    t.gsave()
    t.translate(50, 50)
    t.scale(10, 10)
    t.newPath()
    t.moveTo(0, 0); t.lineTo(1, 0); t.lineTo(1, 1); t.lineTo(0, 1); t.closePath()
    t.setColor(Color("green"))
    t.fillPath()
    t.grestore()
    t.ejectPageTarget()

    // the moveTo(0,0) maps to (50,50); the lineTo(1,1) corner maps to (60,60)
    page.svg should include("M50 50")
    page.svg should include("L60 60")
  }

  "the stroke pen scales with the CTM like the raster backends' pens" in {
    // under scale(10,10) a 2pt pen must stroke 20 device units wide, and dash lengths scale the same way —
    // before the fix the path coordinates were transformed but the width and dashes were not, so SVG strokes
    // came out ten times thinner than PDF/PNG under \scale
    val t    = new SvgTypesetter
    t.init(200, 200)
    val page = t.createPageTarget.asInstanceOf[SvgPage]

    t.gsave()
    t.scale(10, 10)
    t.setColor(Color("black"))
    t.setLineWidth(2)
    t.setDash(Seq(3.0), 0)
    t.newPath()
    t.moveTo(1, 1); t.lineTo(15, 1)
    t.strokePath()
    t.grestore()
    t.ejectPageTarget()

    page.svg should include("stroke-width=\"20\"")
    page.svg should include("stroke-dasharray=\"30\"")
  }

  "drawLine's stroke width carries the CTM scale too" in {
    val t    = new SvgTypesetter
    t.init(200, 200)
    val page = t.createPageTarget.asInstanceOf[SvgPage]

    t.gsave()
    t.scale(3, 3)
    t.setLineWidth(1)
    t.drawLine(0, 0, 10, 10)
    t.grestore()
    t.ejectPageTarget()

    page.svg should include("stroke-width=\"3\"")
  }

  "path geometry freezes when a segment is added — a later transform moves only the pen" in {
    // Cairo and the canvas record segments in the space current when they are added; the SVG backend must
    // agree, or a path built before \scale and stroked after it draws in a different place per backend.
    val t    = new SvgTypesetter
    t.init(200, 200)
    val page = t.createPageTarget.asInstanceOf[SvgPage]

    t.gsave()
    t.setLineWidth(1)
    t.newPath()
    t.moveTo(10, 10); t.lineTo(20, 10)
    t.scale(2, 2) // set after the segments were added
    t.strokePath()
    t.grestore()
    t.ejectPageTarget()

    page.svg should include("M10 10")      // the geometry stayed where it was built…
    page.svg should include("stroke-width=\"2\"") // …while the pen took the new transform
    (page.svg should not).include("M20 20")
  }

  "an unbalanced grestore is ignored instead of closing clip groups it does not own" in {
    val t    = new SvgTypesetter
    t.init(200, 200)
    val page = t.createPageTarget.asInstanceOf[SvgPage]

    t.gsave()
    t.newPath()
    t.moveTo(0, 0); t.lineTo(50, 0); t.lineTo(50, 50); t.closePath()
    t.clipPath()
    t.grestore()
    noException should be thrownBy t.grestore() // one more than was saved
    t.ejectPageTarget()

    countOf(page.svg, "<g clip-path") shouldBe countOf(page.svg, "</g>")
  }

  "an underline leaves the pen width it found" in {
    val t    = new SvgTypesetter
    t.init(200, 200)
    val page = t.createPageTarget.asInstanceOf[SvgPage]

    new UnderlineBox(t, RuleBox(t, 20, 2, 0)).draw(t, 10, 50)
    t.setColor(Color("black"))
    t.newPath()
    t.moveTo(0, 100); t.lineTo(100, 100)
    t.strokePath()
    t.ejectPageTarget()

    // the underline draws at its own 0.8, and the stroke after it is back at the default 1pt pen
    page.svg should include("stroke-width=\"0.8\"")
    page.svg should include("stroke-width=\"1\"")
  }

  private def countOf(s: String, sub: String): Int =
    var i = 0; var n = 0
    while { i = s.indexOf(sub, i); i >= 0 } do
      n += 1; i += sub.length
    n
