package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{Bengali, Gpos, Gsub, IndicShaper, OtfFont}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The Bengali shaping pipeline, checked against the bundled Noto Serif Bengali face: cluster segmentation,
  * pre-base reordering, two-part vowel decomposition and word-position forms ([[Bengali]]) feeding the font's
  * GSUB features ([[IndicShaper]]) feeding GPOS mark placement ([[Gpos]]). Every expected glyph count, order
  * and mark was confirmed against the same font with `hb-shape --shaper=ot`. The parsers are pure shared code;
  * this test reads the real font from disk. */
class GsubBengaliFontTests extends AnyFreeSpec with Matchers:

  private val font =
    new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifBengali/NotoSerifBengali-Regular.ttf")))
  private val shaper = IndicShaper.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")).get
  private val gpos   = Gpos.from(font.tableBytes("GPOS"), font.tableBytes("GDEF"), font.unitsPerEm).get

  private def g(cp: Int): Int                 = font.glyphIndex(cp)
  private def shape(word: String): Array[Int] = shaper.shape(word.toArray.map(_.toInt), g)
  private def shapeCps(cps: Int*): Array[Int] = shaper.shape(cps.toArray, g)

  private val Ka = 0x0995

  "the font is recognised as a Bengali shaper" in {
    IndicShaper.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")) shouldBe defined
    shaper.script shouldBe Bengali
    Gsub.fromIndic(font.tableBytes("GSUB"), font.tableBytes("GDEF"), Bengali.scriptTags).get.boundToRequestedScript shouldBe true
  }

  "the Devanagari face is not mistaken for a Bengali run, and vice versa" in {
    // Each Indic font binds to its own script: the Devanagari shaper handles Devanagari text, not Bengali.
    val deva = new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifDevanagari/NotoSerifDevanagari-Regular.ttf")))
    IndicShaper.from(deva.tableBytes("GSUB"), deva.tableBytes("GDEF")).get.handles("বাংলা") shouldBe false
    shaper.handles("বাংলা") shouldBe true
    shaper.handles("hello") shouldBe false
  }

  "the i sign is reordered before its base consonant" in {
    // কি (ka, i-sign) → [i, ka]: the i-sign, typed after ka, is drawn first. The Bengali i-sign has no
    // word-initial form, so it stays the plain glyph at the front.
    val glyphs = shape("কি")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(0x09BF) // the plain i-sign leads
    glyphs.last shouldBe g(Ka)     // the base ka stays last
  }

  "the pre-base e sign is reordered and takes its word-initial form" in {
    // কে (ka, e-sign) → [e.init, ka]: the e-sign moves to the front and, being word-initial, takes its
    // distinct init form — so the leading glyph is neither the plain e-sign nor the base.
    val glyphs = shape("কে")
    glyphs.length shouldBe 2
    glyphs.last shouldBe g(Ka)
    glyphs.head should not equal g(0x09C7) // a word-initial variant, not the plain e-sign
    glyphs.head should not equal g(Ka)
  }

  "the two-part o sign decomposes into a front e part and a trailing aa part" in {
    // কো (ka, o-sign U+09CB) → [e.init, ka, aa.fina]: the single o-sign renders as an e-sign before the base
    // and an aa-sign after it. Three glyphs, the base in the middle, and neither vowel part is the plain sign.
    val glyphs = shape("কো")
    glyphs.length shouldBe 3
    glyphs(1) shouldBe g(Ka)
    glyphs.head should not equal g(Ka)
    glyphs.head should not equal g(0x09BE) // the front part is the e sign (its init form), not aa
    glyphs.last should not equal g(Ka)
    gpos.position(glyphs).exists(_.isMark) shouldBe false // all three parts are spacing glyphs
  }

  "a post-base aa sign takes its word-final form and stays after the base" in {
    // কা (ka, aa-sign) → [ka, aa.fina]: the aa-sign is post-base, is not reordered, and being word-final
    // takes its fina form.
    val glyphs = shape("কা")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(Ka)
    glyphs.last should not equal g(0x09BE) // the word-final variant, not the plain aa
  }

  "a conjunct collapses to a single ligature glyph" in {
    // ক্ত (ka, virama, ta) fuses to one conjunct glyph; প্র (pa, virama, ra) fuses its below-base ra in too.
    shape("ক্ত").length shouldBe 1
    shape("প্র").length shouldBe 1
  }

  "the post-base ya-phalaa is formed after the base" in {
    // ক্য (ka, virama, ya) → [ka, ya-phalaa]: the ya takes its post-base form (the pstf feature) and stays a
    // spacing glyph after the base, not a mark.
    val glyphs = shape("ক্য")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(Ka)
    gpos.position(glyphs).exists(_.isMark) shouldBe false
  }

  "a below-base u sign attaches to its base through GPOS" in {
    // কু (ka, below-base u) keeps two glyphs, and the u sign is a mark placed under the base.
    val glyphs = shape("কু")
    glyphs.length shouldBe 2
    val places = gpos.position(glyphs)
    val marks  = glyphs.indices.filter(i => places(i).isMark)
    marks.size shouldBe 1
    places(marks.head).attach should be >= 0
  }

  "a word-initial reph is moved past the base and set as an above-mark" in {
    // র্ক (ra, virama, ka): the reph is not drawn in place but as a mark over its base ka. The shaped run is
    // [ka, reph], the reph last and attached above the line by GPOS.
    val glyphs = shape("র্ক")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(Ka)
    val places = gpos.position(glyphs)
    places.last.isMark shouldBe true
    places.last.attach should be >= 0
    places.count(_.isMark) shouldBe 1
  }

  "a reph sits between the base and a post-base aa sign, which keeps its word-final form" in {
    // র্মা (ra, virama, ma, aa) → [ma, reph, aa.fina], confirmed with hb-shape: the Bengali reph is inserted
    // before the post-base vowel sign, not after it, and the aa — still the word's last glyph — takes its
    // fina form.
    val glyphs = shape("র্মা")
    glyphs.length shouldBe 3
    glyphs(0) shouldBe g(0x09AE)         // ma
    glyphs(1) shouldBe shape("র্ক").last // the reph glyph, the same one র্ক sets over its ka
    glyphs(2) should not equal g(0x09BE) // the aa in its fina form, not the plain sign
  }

  "কিন্তু segments into two clusters and reorders the first i sign" in {
    // কি | ন্তু (ka i | na virama ta u): the i-sign of the first syllable leads the run; the second syllable
    // fuses na+virama+ta and its below-base u into one glyph. Three glyphs, the i-sign first.
    val glyphs = shape("কিন্তু")
    glyphs.length shouldBe 3
    glyphs.head shouldBe g(0x09BF) // the reordered i-sign leads
    glyphs should contain(g(Ka))   // the base ka survives
  }

  "বাংলা keeps a medial aa plain and only the word-final aa takes its fina form" in {
    // বাংলা (ba aa anusvara | la aa) → [ba, aa, anusvara, la, aa.fina]: the first aa is not word-final, so it
    // stays the plain sign; only the last glyph, the second aa, takes the fina form. Proof that fina is a
    // word-position form, applied to the end of the run and not to every aa.
    val glyphs = shape("বাংলা")
    glyphs.length shouldBe 5
    glyphs(0) shouldBe g(0x09AC)     // ba
    glyphs(1) shouldBe g(0x09BE)     // the medial aa stays plain
    glyphs(2) shouldBe g(0x0982)     // anusvara
    glyphs(3) shouldBe g(0x09B2)     // la
    glyphs(4) should not equal g(0x09BE) // the final aa takes its fina form
  }

  "a consonant with a nukta composes into one glyph" in {
    // ya (U+09AF) + nukta (U+09BC) composes to the precomposed yya (U+09DF) through the nukt feature: one
    // character pair, one glyph.
    val glyphs = shapeCps(0x09AF, 0x09BC)
    glyphs.length shouldBe 1
    glyphs.head shouldBe g(0x09DF)
  }

  "everyday words shape glyph for glyph as hb-shape does" in {
    // Some of this font's akhand, pre-base and above-base behaviour is written as rule- and class-based
    // context lookups (types 5 and 6, formats 1 and 2), which the parser once skipped entirely. These are the
    // sequences `hb-shape --shaper=ot` reports.
    shape("বাংলা").toSeq shouldBe Seq(47, 59, 11, 52, 478)
    shape("কিন্তু").toSeq shouldBe Seq(60, 25, 573)
    shape("ছাত্র").toSeq shouldBe Seq(31, 59, 258)
    shape("জ্ঞান").toSeq shouldBe Seq(197, 59, 44)
    shape("কর্ম").toSeq shouldBe Seq(25, 49, 124)
  }
