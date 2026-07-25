package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{Gpos, Gsub, IndicShaper, OtfFont, Telugu}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The Telugu shaping pipeline, checked against the bundled Noto Serif Telugu face: cluster segmentation
  * ([[Telugu]]) feeding the font's GSUB features ([[IndicShaper]]) feeding GPOS placement ([[Gpos]]). Every
  * expected glyph count and order was confirmed against the same font with `hb-shape --shaper=ot`.
  *
  * Telugu leans on the font more than the other Indic scripts texish sets: a consonant and its vowel sign
  * usually fuse into a single glyph, and a virama-joined consonant becomes a subscript beneath the base
  * rather than a half-form beside it. The parsers are pure shared code; this test reads the real font.
  */
class GsubTeluguFontTests extends AnyFreeSpec with Matchers:

  private val font =
    new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifTelugu/NotoSerifTelugu-Regular.ttf")))
  private val shaper = IndicShaper.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")).get
  private val gpos   = Gpos.from(font.tableBytes("GPOS"), font.tableBytes("GDEF"), font.unitsPerEm).get

  private def g(cp: Int): Int                 = font.glyphIndex(cp)
  private def shape(word: String): Array[Int] = shaper.shape(word.toArray.map(_.toInt), g)

  private val Ka = 0x0c15
  private val Ra = 0x0c30

  "the font is recognised as a Telugu shaper" in {
    IndicShaper.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")) shouldBe defined
    shaper.script shouldBe Telugu
    Gsub.fromIndic(font.tableBytes("GSUB"), font.tableBytes("GDEF"), Telugu.scriptTags).get.boundToRequestedScript shouldBe true
  }

  "the Devanagari face is not mistaken for a Telugu run, and vice versa" in {
    val deva = new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifDevanagari/NotoSerifDevanagari-Regular.ttf")))
    IndicShaper.from(deva.tableBytes("GSUB"), deva.tableBytes("GDEF")).get.handles("తెలుగు") shouldBe false
    shaper.handles("తెలుగు") shouldBe true
    shaper.handles("hello") shouldBe false
  }

  "a consonant and its vowel sign fuse into a single glyph" in {
    // కి (ka, i-sign) is one glyph, not a base plus a mark — the font carries a drawn ka-i.
    val glyphs = shape("కి")
    glyphs.length shouldBe 1
    glyphs.head should not equal g(Ka)     // it is not the bare ka
    glyphs.head should not equal g(0x0c3f) // nor the bare i-sign
  }

  "the aa sign also fuses, and to a different glyph than the i sign" in {
    val ki = shape("కి")
    val ka = shape("కా")
    ka.length shouldBe 1
    ka.head should not equal ki.head // the two vowels give different drawn forms
  }

  "a virama-joined consonant becomes a subscript under the base, which stays first" in {
    // క్క (ka, virama, ka) → [ka, ka.postscript]: the first ka remains the base and the second subjoins.
    val glyphs = shape("క్క")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(Ka)             // the base is the FIRST consonant
    glyphs.last should not equal g(Ka)     // the second is a subscript form, not another plain ka
    glyphs.last should not equal g(0x0c4d) // and not the bare virama
  }

  "a joined ra subjoins beneath the base as a zero-width mark" in {
    // క్ర (ka, virama, ra) → [ka, ra.subscript], the ra placed under the base by GPOS.
    val glyphs = shape("క్ర")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(Ka)
    glyphs.last should not equal g(Ra)
  }

  "a word-initial ra with a virama is not a reph: the ra stays the base" in {
    // ర్క (ra, virama, ka) → [ra, ka.postscript]. In Devanagari the ra would rise as a reph over the ka; in
    // Telugu it stays in place and the ka subjoins, so the run opens with the ra itself.
    val glyphs = shape("ర్క")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(Ra)
    glyphs.last shouldBe shape("క్క").last // the same subscript ka that క్క produces
  }

  "the ksha conjunct collapses to a single ligature glyph" in {
    shape("క్ష").length shouldBe 1
  }

  "an anusvara stays a spacing glyph after its base" in {
    val glyphs = shape("కం")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(Ka)
    glyphs.last shouldBe g(0x0c02)
    gpos.position(glyphs).exists(_.isMark) shouldBe false // it advances, it does not attach
  }

  "పుస్తకం shapes into its three clusters" in {
    // పు | స్త | కం → [pu, sa, ta.subscript, ka, anusvara]: the pu fuses, the ta subjoins under the sa, and
    // the last cluster is a plain ka with its anusvara. Five glyphs, confirmed with hb-shape.
    val glyphs = shape("పుస్తకం")
    glyphs.length shouldBe 5
    glyphs(1) shouldBe g(0x0c38)     // సa, the base of the middle cluster
    glyphs(3) shouldBe g(Ka)
    glyphs(4) shouldBe g(0x0c02)     // the anusvara closes the run
    glyphs.head should not equal g(0x0c2a) // the pu is fused, not a bare pa
  }

  "a subjoined consonant is placed under its base by GPOS" in {
    // The ta subscript of స్త attaches rather than advancing on its own.
    val glyphs = shape("స్త")
    glyphs.length shouldBe 2
    val places = gpos.position(glyphs)
    places.count(_.isMark) shouldBe 1
    places.last.isMark shouldBe true
  }
