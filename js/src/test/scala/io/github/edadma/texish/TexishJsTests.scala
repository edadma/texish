package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The in-browser renderer running under Node. Unlike the shared tests (which use a fake-metrics typesetter),
  * these drive the real Scala.js path end to end: the embedded fonts are base64-decoded, parsed as real
  * OpenType/CFF in pure Scala, and their glyph outlines are filled into SVG. This is the first coverage of
  * real font parsing on the JS target. The DOM-returning entry points need a browser and are exercised
  * elsewhere; here we check the string output. */
class TexishJsTests extends AnyFreeSpec with Matchers:

  /** Swallow the bare typesetter's off-size page warnings while rendering. */
  private def quiet[A](body: => A): A = Console.withOut(new java.io.ByteArrayOutputStream)(body)

  "renderToString lays out text into a well-formed svg with filled glyph outlines" in {
    val svg = quiet(Texish.renderToString("Hello, world."))
    svg should startWith("<svg")
    svg should include("xmlns=\"http://www.w3.org/2000/svg\"")
    svg should endWith("</svg>\n")
    // real glyph outlines from the embedded Latin Modern Roman, filled as paths
    svg should include("<path d=\"M")
  }

  "the embedded math font renders a formula by glyph index" in {
    val svg = quiet(Texish.renderToString("$E = mc^2$"))
    // math draws glyphs from Latin Modern Math's outlines — paths, not <text>
    svg should include("<path d=\"M")
    svg should not include "<text"
  }

  "an embedded font decodes to a valid OpenType font" in {
    // lmroman is a CFF/.otf; parsing it proves the base64 embed + decode + sfnt reader work on JS
    val font = new io.github.edadma.texish.opentype.OtfFont(
      EmbeddedFonts.bytes("fonts/LatinModernRoman/lmroman10-regular.otf"),
    )
    font.unitsPerEm should be > 0
    font.numGlyphs should be > 0
    font.isCff shouldBe true
    // 'A' resolves through cmap and has a non-empty outline
    val gid = font.glyphIndex('A'.toInt)
    gid should be > 0
    font.outline(gid) should not be empty
  }

  "renderAllToStrings returns one svg per page" in {
    val pages = quiet(Texish.renderAllToStrings("Hello, world."))
    pages.length should be >= 1
    pages.toArray.foreach(_ should startWith("<svg"))
  }

  "a font outside the embedded set fails with a clear error" in {
    val ex = intercept[RuntimeException](EmbeddedFonts.bytes("fonts/NotoSans/NotoSans-Regular.ttf"))
    ex.getMessage should include("not embedded")
  }

  "a document using an embedded package resolves it with no filesystem" in {
    // \use{document} reads no file on disk here — its source is embedded in the build and resolved as a
    // fallback. It in turn pulls in the logos package, so the whole module graph must come from the embed.
    val src =
      """\use{document}
        |\section{Hello}
        |Body text with a logo: \TeX.
        |""".stripMargin
    val svg = quiet(Texish.renderToString(src))
    svg should startWith("<svg")
    svg should include("<path d=\"M")
  }

  "the standard packages are all embedded" in {
    for name <- Seq("document", "book", "logos", "counters", "theorem") do
      EmbeddedPackages.sources.keySet should contain(name)
  }

  "math glyphs that have a codepoint resolve back to one, so they draw with the hinted fillText path" in {
    // Math mode selects every glyph by index; the canvas backend draws a glyph with fillText when it has a
    // codepoint and only outline-fills the ones that do not. The embedded math font is the SMaFL build, whose
    // cmap gives the size-variant and assembly glyphs private-use codepoints too — so a private-use codepoint
    // resolves to a glyph, and that glyph round-trips back, proving the hinted path reaches the stretchy glyphs.
    val math = new io.github.edadma.texish.opentype.OtfFont(
      EmbeddedFonts.bytes("fonts/LatinModernMath/LatinModernMath-SMaFL.otf"),
    )
    for cp <- Seq('x'.toInt, 'a'.toInt, '2'.toInt, '='.toInt, 0x221a /* √ */, 0x00b1 /* ± */) do
      val gid = math.glyphIndex(cp)
      gid should be > 0
      math.codepointForGlyph(gid) should not be empty
    val variantGid = math.glyphIndex(0xe000) // first SMaFL private-use assignment
    variantGid should be > 0
    math.codepointForGlyph(variantGid) shouldBe Some(0xe000)
  }

  "a fragment is tight-cropped, not a full page" in {
    // The in-browser renderer crops each page to its ink, so a short line is sized to the content (in points)
    // rather than emitted as a full 612x792 page shrunk to fit.
    val svg = quiet(Texish.renderToString("Hi"))
    val w   = "width=\"([0-9.]+)pt\"".r.findFirstMatchIn(svg).map(_.group(1).toDouble)
    w.isDefined shouldBe true
    w.get should be < 200.0 // far smaller than a full page width (612pt)
  }
