package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{Gpos, Gsub, Gurmukhi, IndicShaper, OtfFont}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The Gurmukhi shaping pipeline, checked against the bundled Noto Serif Gurmukhi face: cluster segmentation,
  * pre-base sihari reordering and subjoined below-base forms ([[Gurmukhi]]) feeding the font's GSUB features
  * ([[IndicShaper]]) feeding GPOS mark placement ([[Gpos]]). Every expected glyph count, order and mark was
  * confirmed against the same font with `hb-shape --shaper=ot`. The parsers are pure shared code; this test
  * reads the real font from disk. */
class GsubGurmukhiFontTests extends AnyFreeSpec with Matchers:

  private val font =
    new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifGurmukhi/NotoSerifGurmukhi-Regular.ttf")))
  private val shaper = IndicShaper.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")).get
  private val gpos   = Gpos.from(font.tableBytes("GPOS"), font.tableBytes("GDEF"), font.unitsPerEm).get

  private def g(cp: Int): Int                 = font.glyphIndex(cp)
  private def shape(word: String): Array[Int] = shaper.shape(word.toArray.map(_.toInt), g)

  private val Ka = 0x0A15
  private val Pa = 0x0A2A

  "the font is recognised as a Gurmukhi shaper" in {
    IndicShaper.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")) shouldBe defined
    shaper.script shouldBe Gurmukhi
    Gsub.fromIndic(font.tableBytes("GSUB"), font.tableBytes("GDEF"), Gurmukhi.scriptTags).get.boundToRequestedScript shouldBe true
  }

  "the Devanagari face is not mistaken for a Gurmukhi run, and vice versa" in {
    // Each Indic font binds to its own script: the Devanagari shaper handles Devanagari text, not Gurmukhi.
    val deva = new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifDevanagari/NotoSerifDevanagari-Regular.ttf")))
    IndicShaper.from(deva.tableBytes("GSUB"), deva.tableBytes("GDEF")).get.handles("ਪੰਜਾਬੀ") shouldBe false
    shaper.handles("ਪੰਜਾਬੀ") shouldBe true
    shaper.handles("hello") shouldBe false
  }

  "the sihari is reordered before its base consonant" in {
    // ਕਿ (ka, sihari) → [sihari, ka]: the sihari, typed after ka, is drawn first. Gurmukhi has no word-initial
    // form for it, so it stays the plain glyph at the front.
    val glyphs = shape("ਕਿ")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(0x0A3F) // the plain sihari leads
    glyphs.last shouldBe g(Ka)     // the base ka stays last
  }

  "a post-base bihari stays after the base" in {
    // ਕੀ (ka, bihari) → [ka, bihari]: the ii-sign is post-base and is not reordered.
    val glyphs = shape("ਕੀ")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(Ka)
    glyphs.last shouldBe g(0x0A40)
  }

  "a virama-joined ra takes a subjoined below-base form, not a reph" in {
    // ਪ੍ਰ (pa, virama, ra) → [pa, ra.below]: the ra fuses to a single below-base glyph under the base. It is
    // neither a leading reph nor the plain ra or virama — Gurmukhi has no reph at all.
    val glyphs = shape("ਪ੍ਰ")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(Pa)
    glyphs.last should not equal g(0x0A30) // the subjoined form, not the plain ra
    glyphs.last should not equal g(0x0A4D) // and not the bare virama
  }

  "a below-base u sign attaches to its base through GPOS" in {
    // ਗੁ (ga, below-base aunkar u) keeps two glyphs, and the u sign is a mark placed under the base.
    val glyphs = shape("ਗੁ")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(0x0A17) // ga
    val places = gpos.position(glyphs)
    val marks  = glyphs.indices.filter(i => places(i).isMark)
    marks.size shouldBe 1
    places(marks.head).attach should be >= 0
  }

  "a tippi is set as a mark over its base" in {
    // ਪੰ (pa, tippi) → [pa, tippi]: the nasal tippi stays after the base and is placed as a mark above it.
    val glyphs = shape("ਪੰ")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(Pa)
    val places = gpos.position(glyphs)
    places.count(_.isMark) shouldBe 1
    places.last.isMark shouldBe true
  }

  "everyday words shape glyph for glyph as hb-shape does" in {
    // The sequences `hb-shape --shaper=ot` reports for this font. The tests above check one behaviour each,
    // which cannot catch a lookup that is skipped wholesale — the glyphs stay plausible and only the exact
    // run gives it away. These pin the whole pipeline: subjoined forms, the reordered sihari, the addak, the
    // nasal marks and the word-position forms together.
    shape("ਪੰਜਾਬੀ").toSeq shouldBe Seq(38, 78, 25, 52, 40, 54)
    shape("ਸਤਿ").toSeq shouldBe Seq(49, 53, 33)
    shape("ਸ੍ਰੀ").toSeq shouldBe Seq(49, 112, 54)
    shape("ਦਿਨ").toSeq shouldBe Seq(53, 35, 37)
    shape("ਪ੍ਰੇਮ").toSeq shouldBe Seq(38, 112, 57, 42)
    shape("ਚਿੰਨ੍ਹ").toSeq shouldBe Seq(53, 23, 78, 37, 113)
    shape("ਇੱਕ").toSeq shouldBe Seq(10, 79, 18)
    shape("ਪੱਕਾ").toSeq shouldBe Seq(38, 79, 18, 52)
    shape("ਗੁਰਮੁਖੀ").toSeq shouldBe Seq(20, 55, 44, 42, 55, 19, 54)
    shape("ਵਿਦਿਆਰਥੀ").toSeq shouldBe Seq(53, 47, 53, 35, 9, 44, 34, 54)
    shape("ਧੰਨਵਾਦ").toSeq shouldBe Seq(36, 78, 37, 47, 52, 35)
    shape("ਸ੍ਵਰਗ").toSeq shouldBe Seq(49, 114, 44, 20)
  }

  "ਸਿੰਘ reorders the sihari and marks the tippi" in {
    // ਸ ਿ ੰ ਘ (sa, sihari, tippi | gha): the sihari of the first syllable leads the run, the tippi is a mark on
    // that syllable, and the gha opens a new cluster. Four glyphs, the sihari first, one mark.
    val glyphs = shape("ਸਿੰਘ")
    glyphs.length shouldBe 4
    glyphs.head shouldBe g(0x0A3F) // the reordered sihari leads
    glyphs should contain(g(0x0A18)) // the gha survives
    gpos.position(glyphs).count(_.isMark) shouldBe 1 // the tippi
  }
