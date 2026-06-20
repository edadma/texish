package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.OtfFont
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The pure-Scala SFNT reader [[OtfFont]] is the font engine the SVG/JS backend stands on, so it parses real
  * bundled fonts here rather than synthetic bytes. It exercises both outline flavours the engine ships — a CFF
  * `.otf` (Latin Modern Roman, the body face) and a TrueType `.ttf` (Noto Serif) — checking the structural
  * tables a layout needs: the character map, advance widths, and the design-grid resolution.
  */
class OtfFontTests extends AnyFreeSpec with Matchers:

  private def load(path: String): OtfFont = new OtfFont(Files.readAllBytes(Paths.get(path)))

  private val lmroman = load("fonts/LatinModernRoman/lmroman10-regular.otf")
  private val noto    = load("fonts/NotoSerif/NotoSerif-Regular.ttf")

  "a CFF .otf reports CFF outlines and a sane design grid" in {
    lmroman.isCff shouldBe true
    lmroman.unitsPerEm shouldBe 1000 // PostScript/CFF convention
    lmroman.numGlyphs should be > 100
  }

  "a TrueType .ttf reports glyf outlines" in {
    noto.isCff shouldBe false
    noto.unitsPerEm should be > 0
    noto.numGlyphs should be > 100
  }

  "the cmap maps a present codepoint to a non-notdef glyph" in {
    lmroman.glyphIndex('A'.toInt) should not be 0
    lmroman.glyphIndex('a'.toInt) should not be 0
    lmroman.glyphIndex(' '.toInt) should not be 0
    noto.glyphIndex('A'.toInt)    should not be 0
  }

  "the cmap maps an absent codepoint to glyph 0 (.notdef)" in {
    lmroman.glyphIndex(0x10fffe) shouldBe 0 // a Unicode noncharacter, in no font
  }

  "distinct letters map to distinct glyphs, and the lookup is stable" in {
    val a = lmroman.glyphIndex('A'.toInt)
    val b = lmroman.glyphIndex('B'.toInt)
    a should not be b
    lmroman.glyphIndex('A'.toInt) shouldBe a // repeatable
  }

  "advance widths are positive and ordered the way the letters' shapes are" in {
    val i = lmroman.advanceWidthEm(lmroman.glyphIndex('i'.toInt))
    val m = lmroman.advanceWidthEm(lmroman.glyphIndex('m'.toInt))
    i should be > 0.0
    m should be > 0.0
    m should be > i // 'm' is far wider than 'i'
    i should be < 2.0 // an advance is a small multiple of the em
  }

  "the space glyph has a positive advance even though it has no outline" in {
    lmroman.advanceWidthEm(lmroman.glyphIndex(' '.toInt)) should be > 0.0
  }

  "the MATH font's table is reachable through the reader's table bytes" in {
    val lmmath = load("fonts/LatinModernMath/LatinModernMath-Regular.otf")
    lmmath.tableBytes("MATH") should not be empty
    lmmath.isCff shouldBe true
  }

  // The reader's advances are what the SVG/JS layout will lay text out with, so they must agree with the
  // metrics the existing AWT backend lays the SAME font out with — otherwise build-time SVG and JVM PDF of one
  // document would break lines differently. Compare raw hmtx advances (scaled to the point size) against AWT's
  // single-character advance for a span of letters, within a small tolerance.
  "advances match the Graphics2D backend's AWT metrics for the same font" in {
    val size = 12.0
    val t    = new Graphics2DTypesetter(dpi = 72)
    t.selectFont("lmroman", size, Set("regular"))
    val rf = t.currentFont.renderFont.asInstanceOf[t.RenderFont]

    for ch <- "iManglemw.,".toCharArray do
      val mine = lmroman.advanceWidthEm(lmroman.glyphIndex(ch.toInt)) * size
      val awt  = t.charWidth(rf, ch)
      withClue(s"char '$ch': reader=$mine awt=$awt — ") {
        math.abs(mine - awt) should be < 0.05 // sub-tenth-of-a-point agreement
      }
  }
