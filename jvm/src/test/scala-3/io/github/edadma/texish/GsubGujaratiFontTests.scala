package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{Gpos, Gsub, Gujarati, IndicShaper, OtfFont}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The Gujarati shaping pipeline, checked against the bundled Noto Serif Gujarati face: cluster segmentation
  * and pre-base reordering ([[Gujarati]]) feeding the font's GSUB features ([[IndicShaper]]) feeding GPOS mark
  * placement ([[Gpos]]). Every expected glyph sequence here is what `hb-shape --shaper=ot` reports for the
  * same font, so the test pins texish against the reference shaper rather than against itself. The parsers are
  * pure shared code; this test reads the real font from disk. */
class GsubGujaratiFontTests extends AnyFreeSpec with Matchers:

  private val font =
    new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifGujarati/NotoSerifGujarati-Regular.ttf")))
  private val shaper = IndicShaper.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")).get
  private val gpos   = Gpos.from(font.tableBytes("GPOS"), font.tableBytes("GDEF"), font.unitsPerEm).get

  private def g(cp: Int): Int                 = font.glyphIndex(cp)
  private def shape(word: String): Array[Int] = shaper.shape(word.toArray.map(_.toInt), g)

  "the font is recognised as a Gujarati shaper" in {
    IndicShaper.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")) shouldBe defined
    Gsub
      .fromIndic(font.tableBytes("GSUB"), font.tableBytes("GDEF"), Gujarati.scriptTags)
      .get
      .boundToRequestedScript shouldBe true
  }

  "the Gujarati face is not mistaken for a Devanagari shaper, nor the reverse" in {
    // Both scripts share the OpenType Indic model, and the two faces are siblings from the same family — so
    // the script tag is the only thing separating them, and IndicShaper picking the wrong front end would
    // shape the text with the wrong character knowledge rather than failing outright.
    shaper.script shouldBe Gujarati
    val devanagari =
      new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifDevanagari/NotoSerifDevanagari-Regular.ttf")))
    IndicShaper.from(devanagari.tableBytes("GSUB"), devanagari.tableBytes("GDEF")).get.script should not be Gujarati
  }

  "the i sign is reordered before its base consonant" in {
    // કિ (ka, i) shapes to two glyphs, and the base ka stays last — the i sign, typed after it, is drawn
    // first. Without reordering ka would lead the run.
    val glyphs = shape("કિ")
    glyphs.toSeq shouldBe Seq(100, 22)
    glyphs.last shouldBe g(0x0A95)         // ka
    glyphs.head should not equal g(0x0ABF) // the plain i sign became a width-matched variant
  }

  "a conjunct collapses to a single ligature glyph" in {
    // ક્ષ (ka, virama, ssa) is an akhand ligature: three characters become one glyph.
    shape("ક્ષ").toSeq shouldBe Seq(243)
  }

  "a below-base vowel sign attaches to its base through GPOS" in {
    // કુ (ka, below-base u) keeps two glyphs, and the u sign is a mark placed under the base.
    val glyphs = shape("કુ")
    glyphs.toSeq shouldBe Seq(22, 63)
    val places = gpos.position(glyphs)
    val marks  = glyphs.indices.filter(i => places(i).isMark)
    marks.size shouldBe 1
    places(marks.head).attach should be >= 0
  }

  "a post-base aa sign stays after its base and carries an advance" in {
    // કા (ka, aa) — the aa sign is a spacing glyph after the base, not a mark and not reordered.
    shape("કા").toSeq shouldBe Seq(22, 60)
    gpos.position(shape("કા")).exists(_.isMark) shouldBe false
  }

  "a word-initial reph is moved past the base and set as an above-mark" in {
    // ર્ક (ra virama ka): the reph is not drawn in place but as a mark over its base ka, so the shaped run is
    // [ka, reph] — the base first, the reph last, attached above the line by GPOS.
    val glyphs = shape("ર્ક")
    glyphs.toSeq shouldBe Seq(22, 148)
    val places = gpos.position(glyphs)
    places.last.isMark shouldBe true  // the reph is a mark…
    places.last.attach should be >= 0 // …attached to its base
    places.count(_.isMark) shouldBe 1
  }

  "a Gujarati reph follows a post-base aa sign, as Devanagari's does" in {
    // ર્મા (ra, virama, ma, aa) → [ma, aa, reph]: the reph closes the cluster, after the post-base vowel
    // sign, so Gujarati leaves rephBeforePostBase off. Bengali is the script that differs here.
    val glyphs = shape("ર્મા")
    glyphs.toSeq shouldBe Seq(47, 60, 148)
    glyphs(0) shouldBe g(0x0AAE)                   // ma
    glyphs(1) shouldBe g(0x0ABE)                   // the aa sign stays right after its base
    gpos.position(glyphs).last.isMark shouldBe true // the reph, above the syllable
  }

  "a reph over a bare base keeps the base spacing and the reph floating above" in {
    // કર્મ (ka | ra virama ma) → [ka, ma, reph]: two spacing bases and the reph mark.
    val glyphs = shape("કર્મ")
    glyphs.toSeq shouldBe Seq(22, 47, 148)
    val places = gpos.position(glyphs)
    places(0).isMark shouldBe false // ka
    places(1).isMark shouldBe false // ma
    places(2).isMark shouldBe true  // reph
  }

  "everyday words shape glyph for glyph as hb-shape does" in {
    // The sequences `hb-shape --shaper=ot` reports for this font. They exercise the conjunct and half-form
    // lookups, the reph, and the pre-base reordering together, which is where a front end that got the
    // character categories subtly wrong would diverge from the reference.
    shape("ગુજરાતી").toSeq shouldBe Seq(24, 63, 29, 49, 60, 38, 62)  // the language's own name
    shape("અમદાવાદ").toSeq shouldBe Seq(8, 47, 40, 60, 52, 60, 40)   // Amdavad
    shape("નમસ્તે").toSeq shouldBe Seq(42, 47, 199, 38, 68)          // namaste
    shape("પુસ્તક").toSeq shouldBe Seq(43, 63, 199, 38, 22)          // pustak, a book
    shape("વિદ્યા").toSeq shouldBe Seq(100, 52, 285, 60)             // vidya, knowledge
    shape("ધર્મ").toSeq shouldBe Seq(41, 47, 148)                    // dharma
  }

  "the words the bundled demo sets shape as hb-shape does" in {
    // scripts/gujarati-demo.script is the longest piece of real Gujarati in the tree and the corpus
    // renderer's exercise of this shaper. Pinning its distinctive words here means a regression shows up in
    // the ordinary suite rather than as a subtly wrong page nobody reads closely.
    shape("શ્રી").toSeq shouldBe Seq(232, 62)              // shri — a conjunct ligature with a post-base sign
    shape("પ્રેમ").toSeq shouldBe Seq(223, 68, 47)         // prem, love — a rakaar conjunct
    shape("સૂર્ય").toSeq shouldBe Seq(55, 64, 48, 148)     // surya, sun — a reph over a syllable with a sign
    shape("અર્થ").toSeq shouldBe Seq(8, 39, 148)           // arth, meaning — a reph after an independent vowel
    shape("સમૃદ્ધ").toSeq shouldBe Seq(55, 47, 65, 280)    // samruddh, rich — the ddha conjunct
    shape("લિપિમાં").toSeq shouldBe Seq(101, 50, 100, 43, 47, 60, 6) // lipima, in the script — two i signs
  }
