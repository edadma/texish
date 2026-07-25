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

  "a joined ra subjoins beneath the base, in the form the base calls for" in {
    // క్ర (ka, virama, ra) → [ka, ka-ra.subscript]: the below-base feature forms a generic subjoined ra, and
    // the above-base feature then swaps it for the variant drawn under a ka. That second step is reached
    // through a chaining-context lookup whose record names another chaining-context lookup — a nesting the
    // parser once refused to follow, leaving the generic form in place.
    val glyphs = shape("క్ర")
    glyphs.toSeq shouldBe Seq(23, 539)
    glyphs.head shouldBe g(Ka)
    glyphs.last should not equal g(Ra)
    shape("ప్ర").toSeq shouldBe Seq(43, 559) // and a different base calls for a different variant
  }

  "a vowel sign is drawn beside its base, ahead of the consonants subjoined beneath it" in {
    // ప్రేమ (pa virama ra ee | ma): the ee sign is typed after the whole conjunct but belongs to the pa, so
    // it moves back to the pa's side — close enough for the font to fuse the two into one drawn glyph — and
    // the subjoined ra follows. Without the move the sign is stranded after the ra and no fusion matches.
    shape("ప్రేమ").toSeq shouldBe Seq(313, 559, 47)
    shape("స్త్రే").toSeq shouldBe Seq(326, 594) // and it moves back across two subjoined consonants
  }

  "the vocalic r sign is the exception: it stays below, after the subjoined consonant" in {
    // సంస్కృతం (samskrutam): the ృ of the స్కృ cluster is drawn below the syllable, not on the base, so
    // unlike every other Telugu sign it keeps its place after the subjoined ka.
    val glyphs = shape("సంస్కృతం")
    glyphs.toSeq shouldBe Seq(57, 6, 57, 736, 535, 38, 6)
    glyphs(3) shouldBe shape("క్క").last // the subjoined ka comes first…
    glyphs(4) should not equal g(0x0c43) // …then the vocalic r in its drawn form
  }

  "the ai sign splits into the two parts the font draws" in {
    // హై (ha, ai) is one glyph in memory but the font carries the ai as an e sign plus a length mark; split
    // into the pair, both parts land on the ha and fuse into the single drawn ha-ai.
    shape("హై").toSeq shouldBe Seq(363)
    shape("హైదరాబాద్").toSeq shouldBe Seq(363, 40, 167, 163, 122)
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

  "పుస్తకం shapes into its three clusters, glyph for glyph as hb-shape does" in {
    // పు | స్త | కం → [pu, sa, ta.subscript, ka, anusvara]: the pu fuses, the ta subjoins under the sa, and
    // the last cluster is a plain ka with its anusvara. The exact sequence `hb-shape --shaper=ot` reports,
    // which this font builds partly through class-based context lookups — the formats texish once skipped.
    val glyphs = shape("పుస్తకం")
    glyphs.toSeq shouldBe Seq(250, 57, 485, 23, 6)
    glyphs(1) shouldBe g(0x0c38)           // స, the base of the middle cluster
    glyphs(3) shouldBe g(Ka)
    glyphs(4) shouldBe g(0x0c02)           // the anusvara closes the run
    glyphs.head should not equal g(0x0c2a) // the pu is fused, not a bare pa
  }

  "everyday words shape glyph for glyph as hb-shape does" in {
    // The sequences `hb-shape --shaper=ot` reports for this font, covering the whole Telugu path: fused
    // consonant-vowel glyphs, conjuncts of two and three consonants, subjoined ra and ya, the reordered vowel
    // signs, the anusvara, and a word-final virama.
    shape("తెలుగు").toSeq shouldBe Seq(272, 51, 63, 25, 63)
    shape("విద్య").toSeq shouldBe Seq(207, 40, 732)
    shape("స్త్రీ").toSeq shouldBe Seq(246, 594)
    shape("అమ్మ").toSeq shouldBe Seq(9, 47, 738)
    shape("కృష్ణ").toSeq shouldBe Seq(23, 65, 56, 484)
    shape("శ్రీ").toSeq shouldBe Seq(244, 570)
    shape("జ్ఞానం").toSeq shouldBe Seq(148, 479, 42, 6)
    shape("తెలంగాణ").toSeq shouldBe Seq(272, 51, 6, 143, 37)
    shape("ధన్యవాదాలు").toSeq shouldBe Seq(41, 42, 732, 171, 158, 51, 63)
    shape("స్వాతంత్ర్యం").toSeq shouldBe Seq(174, 731, 38, 6, 38, 554, 525, 6)
  }

  "a subjoined consonant is placed under its base by GPOS" in {
    // The ta subscript of స్త attaches rather than advancing on its own.
    val glyphs = shape("స్త")
    glyphs.length shouldBe 2
    val places = gpos.position(glyphs)
    places.count(_.isMark) shouldBe 1
    places.last.isMark shouldBe true
  }
