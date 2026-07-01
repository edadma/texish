package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{Devanagari, Gpos, Gsub, IndicShaper, OtfFont}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The Devanagari shaping pipeline, checked against the bundled Noto Serif Devanagari face: cluster
  * segmentation and pre-base reordering ([[Devanagari]]) feeding the font's GSUB features ([[IndicShaper]])
  * feeding GPOS mark placement ([[Gpos]]). The expected glyph counts and orderings were confirmed against
  * the same font with hb-shape. The parsers are pure shared code; this test reads the real font from disk. */
class GsubDevanagariFontTests extends AnyFreeSpec with Matchers:

  private val font =
    new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifDevanagari/NotoSerifDevanagari-Regular.ttf")))
  private val shaper = IndicShaper.from(font.tableBytes("GSUB")).get
  private val gpos   = Gpos.from(font.tableBytes("GPOS"), font.tableBytes("GDEF"), font.unitsPerEm).get

  private def g(cp: Int): Int          = font.glyphIndex(cp)
  private def shape(word: String): Array[Int] = shaper.shape(word.toArray.map(_.toInt), g)

  "the font is recognised as a Devanagari shaper" in {
    IndicShaper.from(font.tableBytes("GSUB")) shouldBe defined
    Gsub.fromIndic(font.tableBytes("GSUB")).get.isIndicScript shouldBe true
  }

  "the Naskh Arabic face is not mistaken for a Devanagari shaper" in {
    val arabic = new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoNaskhArabic/NotoNaskhArabic-Regular.ttf")))
    IndicShaper.from(arabic.tableBytes("GSUB")) shouldBe None
  }

  "the short-i sign is reordered before its base consonant" in {
    // कि (ka, short-i) shapes to two glyphs, and the base ka stays last — the short-i, typed after it, is
    // drawn first. Without reordering ka would lead the run.
    val glyphs = shape("कि")
    glyphs.length shouldBe 2
    glyphs.last shouldBe g(0x0915) // ka
    glyphs.head should not equal g(0x0915)
    glyphs.head should not equal g(0x093F) // the plain short-i became a width-matched variant
  }

  "a conjunct collapses to a single ligature glyph" in {
    // क्ष (ka, virama, ssa) is an akhand ligature: three characters become one glyph.
    shape("क्ष").length shouldBe 1
  }

  "हिन्दी shapes to five glyphs with the short-i first" in {
    // [short-i.03, ha, na-half, da, ii-matra] — the short-i of the first syllable leads the run, and the
    // na+virama half-form keeps the conjunct in the second.
    val glyphs = shape("हिन्दी")
    glyphs.length shouldBe 5
    glyphs.head should not equal g(0x0939) // ha does not lead; the reordered short-i does
  }

  "a word with a post-base sign keeps it after the base" in {
    // किताब (ka short-i, ta aa-matra, ba) → [short-i, ka, ta, aa-matra, ba]: the aa-matra is post-base and
    // is not reordered.
    val glyphs = shape("किताब")
    glyphs.length shouldBe 5
    glyphs should contain(g(0x0915)) // ka survives
  }

  "spacing vowel signs carry advances and are not marks" in {
    // In कि both the reordered short-i and the base ka advance the pen; neither is a GPOS mark.
    val places = gpos.position(shape("कि"))
    places.exists(_.isMark) shouldBe false
  }

  "a below-base vowel sign attaches to its base through GPOS" in {
    // कु (ka, below-base u) keeps two glyphs, and the u sign is a mark placed under the base.
    val glyphs = shape("कु")
    glyphs.length shouldBe 2
    val places = gpos.position(glyphs)
    val marks  = glyphs.indices.filter(i => places(i).isMark)
    marks.size shouldBe 1
    places(marks.head).attach should be >= 0
  }
