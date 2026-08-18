package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{Gpos, Gsub, IndicShaper, Kannada, OtfFont}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The Kannada shaping pipeline, checked against the bundled Noto Serif Kannada face: cluster segmentation,
  * the nested vowel-sign decomposition and the move back across the subjoined consonants ([[Kannada]]) feeding
  * the font's GSUB features ([[IndicShaper]]) feeding GPOS mark placement ([[Gpos]]). Every expected glyph
  * sequence here is what `hb-shape --shaper=ot` reports for the same font, so the test pins texish against the
  * reference shaper rather than against itself. The glyph names in the comments are the font's own, which is
  * what makes these sequences readable — `ka_kannada.below` is the ottakshara, `reph_kannada` the arkavattu. */
class GsubKannadaFontTests extends AnyFreeSpec with Matchers:

  private val font =
    new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifKannada/NotoSerifKannada-Regular.ttf")))
  private val shaper = IndicShaper.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")).get
  private val gpos   = Gpos.from(font.tableBytes("GPOS"), font.tableBytes("GDEF"), font.unitsPerEm).get

  private def g(cp: Int): Int                 = font.glyphIndex(cp)
  private def shape(word: String): Array[Int] = shaper.shape(word.toArray.map(_.toInt), g)

  "the font is recognised as a Kannada shaper" in {
    shaper.script shouldBe Kannada
    Gsub
      .fromIndic(font.tableBytes("GSUB"), font.tableBytes("GDEF"), Kannada.scriptTags)
      .get
      .boundToRequestedScript shouldBe true
  }

  "the Telugu face is not mistaken for a Kannada shaper, nor the reverse" in {
    // The two scripts are close relatives and shape almost alike, so the script tag is the only thing keeping
    // them apart — and picking the wrong front end would shape the text with the wrong character knowledge
    // rather than failing outright.
    val telugu = new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifTelugu/NotoSerifTelugu-Regular.ttf")))
    IndicShaper.from(telugu.tableBytes("GSUB"), telugu.tableBytes("GDEF")).get.script should not be Kannada
  }

  "a consonant and its vowel sign fuse into one glyph" in {
    // ಕಿ (ka, i) is a single drawn form — ki_kannada — not a base and a mark, which is how this script and
    // Telugu differ from Devanagari.
    shape("ಕಿ").length shouldBe 1
    shape("ಕೆ").length shouldBe 1 // ಕೆ likewise: ke_kannada
  }

  "a virama-joined consonant subjoins beneath the base, which is the FIRST consonant" in {
    // ಕ್ಕ → [ka_kannada, ka_kannada.below]: the ottakshara hangs under the opening ka, so the base is the
    // consonant that opens the cluster rather than the one that closes it.
    val glyphs = shape("ಕ್ಕ")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(0x0c95) // the base ka is the plain glyph
    glyphs.last should not equal g(0x0c95)
  }

  "a vowel sign is moved back across the subjoined consonant to reach its base" in {
    // ಕ್ಕಾ → [ka_kannada.base, aaMatra_kannada, ka_kannada.below]. The aa sign is typed after the whole
    // conjunct but belongs to the base, so it must land beside it and ahead of the ottakshara — otherwise the
    // base and its sign never meet.
    val glyphs = shape("ಕ್ಕಾ")
    glyphs.length shouldBe 3
    glyphs(1) shouldBe g(0x0cbe) // the aa sign, between the base and the subjoined ka
  }

  "the signs drawn below the syllable stay after the subjoined consonant" in {
    // ಕ್ಕೃ → [ka_kannada, ka_kannada.below, rVocalicMatra_kannada]: the vocalic r sign hangs below the whole
    // syllable rather than on the base, so unlike the aa sign it is not moved back.
    val glyphs = shape("ಕ್ಕೃ")
    glyphs.length shouldBe 3
    glyphs.last shouldBe g(0x0cc3)
  }

  "a two-part vowel sign is split into the parts the font draws" in {
    // ಕೊ → [ke_kannada, uuMatra_kannada]: the o sign is written as an e sign, which fuses with the ka, plus a
    // uu sign. The composed codepoint has no glyph of its own.
    val glyphs = shape("ಕೊ")
    glyphs.length shouldBe 2
    glyphs.last shouldBe g(0x0cc2) // the uu part
  }

  "a vowel sign whose parts themselves decompose is split all the way down" in {
    // ಕೋ → [ke_kannada, uuMatra_kannada, length_kannada]. The oo sign is the o sign plus a length mark, and
    // that o sign is an e sign plus a uu sign — so one pass of the split would leave an o the font has no
    // glyph for. This is the case the nested decomposition exists for.
    val glyphs = shape("ಕೋ")
    glyphs.length shouldBe 3
    glyphs(1) shouldBe g(0x0cc2) // uu
    glyphs(2) shouldBe g(0x0cd5) // length mark
  }

  "a length mark trails the subjoined consonant while the sign it lengthens moves back" in {
    // ಕ್ಕೀ → [ki_kannada, ka_kannada.below, length_kannada]: the ii sign splits into an i — which fuses with
    // the base ahead of the ottakshara — and a length mark, which stays behind it.
    val glyphs = shape("ಕ್ಕೀ")
    glyphs.length shouldBe 3
    glyphs.last shouldBe g(0x0cd5)
  }

  "a syllable-opening ra becomes the arkavattu, set after the syllable" in {
    // ರ್ಕ → [ka_kannada, reph_kannada]: the ra is not drawn in place but lifted out and set after its base —
    // Kannada forms a reph where Telugu, its closest relative, does not.
    val glyphs = shape("ರ್ಕ")
    glyphs.length shouldBe 2
    glyphs.head shouldBe g(0x0c95) // ka
  }

  "the arkavattu is a SPACING glyph, unlike Devanagari's reph" in {
    // The two scripts place their reph differently, and it shows up in the metrics rather than the glyph
    // order. Devanagari's rides above its base with no advance of its own, so GPOS carries it as a mark;
    // Kannada's is written to the upper right and takes its own room on the line, so it is an ordinary
    // spacing glyph and nothing in the syllable is a mark at all. Getting this wrong would set the arkavattu
    // on top of whatever follows it.
    gpos.position(shape("ರ್ಕ")).exists(_.isMark) shouldBe false
  }

  "the arkavattu follows a post-base vowel sign, as Devanagari's reph does" in {
    // ರ್ಕಾ → [ka_kannada.base, aaMatra_kannada, reph_kannada]: it closes the cluster after the aa sign, so
    // rephBeforePostBase stays off. Bengali is the script that differs here.
    val glyphs = shape("ರ್ಕಾ")
    glyphs.length shouldBe 3
    glyphs(1) shouldBe g(0x0cbe) // the aa sign, still beside its base
  }

  "everyday words shape glyph for glyph as hb-shape does" in {
    // The sequences `hb-shape --shaper=ot` reports for this font, with its glyph names beside each.
    shape("ಕನ್ನಡ").toSeq shouldBe Seq(26, 45, 237, 38)        // [ka, na, na.below, Da] — the language's own name
    shape("ಬೆಂಗಳೂರು").toSeq shouldBe Seq(180, 7, 28, 156, 52, 67) // Bengaluru
    shape("ನಮಸ್ಕಾರ").toSeq shouldBe Seq(45, 50, 294, 64, 218, 52) // [na, ma, sa.base, aaMatra, ka.below, ra]
    shape("ಪುಸ್ತಕ").toSeq shouldBe Seq(148, 59, 233, 26)      // [pu, sa, ta.below, ka] — pustaka, a book
    shape("ಅರ್ಥ").toSeq shouldBe Seq(10, 42, 94)              // [a, tha, reph] — artha, meaning
    shape("ಧರ್ಮ").toSeq shouldBe Seq(44, 50, 94)              // [dha, ma, reph] — dharma
  }
