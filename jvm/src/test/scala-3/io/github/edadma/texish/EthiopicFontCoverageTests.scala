package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{ArabicShaping, IndicShaper, OtfFont}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Glyph coverage of the bundled Ethiopic face, read from disk. Ethiopic is the one non-Latin script texish
  * sets that needs no shaping at all — it is written left to right, one codepoint is one glyph, nothing
  * reorders and there are no dependent vowel signs to place, because the vowel is written into the letterform
  * itself. So there is no shaper to test and what matters instead is coverage: a syllabary of some 350 glyphs
  * has to carry every letter its languages use, and a gap shows up as a missing-glyph box rather than as
  * mis-shaped text. The tests below also pin the "no shaping" claim, since an Ethiopic face wrongly routed
  * down the Indic or Arabic path would be a silent regression. */
class EthiopicFontCoverageTests extends AnyFreeSpec with Matchers:

  private def font(path: String): OtfFont = new OtfFont(Files.readAllBytes(Paths.get(path)))
  private def covers(f: OtfFont, s: String): Boolean =
    s.codePoints().allMatch(cp => f.glyphIndex(cp) != 0)

  private val regular = font("fonts/NotoSerifEthiopic/NotoSerifEthiopic-Regular.ttf")
  private val bold    = font("fonts/NotoSerifEthiopic/NotoSerifEthiopic-Bold.ttf")

  "the face covers Amharic" in {
    covers(regular, "አማርኛ") shouldBe true   // the language's own name
    covers(regular, "ሰላም") shouldBe true    // selam, peace
    covers(regular, "ኢትዮጵያ") shouldBe true // Ityop'ya, Ethiopia
  }

  "the face covers Tigrinya and Ge'ez" in {
    // The two other languages the Ethiopic script carries in daily use. Tigrinya draws on letters Amharic
    // does not, which is why coverage of the whole block matters rather than of Amharic's subset.
    covers(regular, "ትግርኛ") shouldBe true // Tigrinya
    covers(regular, "ግዕዝ") shouldBe true  // Ge'ez
  }

  "the face covers the Ethiopic punctuation and digits" in {
    // Ethiopic has its own wordspace and full stop, and its own numerals; a document typing them literally
    // needs them present, since no ASCII shorthand produces them.
    regular.glyphIndex(0x1361) should not be 0 // ፡ wordspace
    regular.glyphIndex(0x1362) should not be 0 // ። full stop
    regular.glyphIndex(0x1369) should not be 0 // ፩ digit one
  }

  "the face reaches past the main block into the Supplement and Extended blocks" in {
    // Ethiopic spills out of U+1200–137F into three further blocks for the letters that write the smaller
    // languages of the region. This face carries them, so it is not an Amharic-only cut.
    regular.glyphIndex(0x1380) should not be 0 // ᎀ Supplement
    regular.glyphIndex(0x2D80) should not be 0 // ⶀ Extended
    regular.glyphIndex(0xab01) should not be 0 // ꬁ Extended-A
  }

  "the bold cut carries the same coverage" in {
    covers(bold, "አማርኛሰላም") shouldBe true
    covers(bold, "ትግርኛ") shouldBe true
  }

  "the face is not routed to a shaper — Ethiopic takes the plain path" in {
    // The design claim this face is bundled on: no cluster segmentation, no reordering, no joining. Its GSUB
    // carries no Indic script table, so no Indic shaper binds to it, and Ethiopic text is not cursive, so the
    // Arabic path does not claim it either. The text is set glyph by glyph like a roman run.
    IndicShaper.from(regular.tableBytes("GSUB"), regular.tableBytes("GDEF")) shouldBe None
    ArabicShaping.hasArabic("አማርኛ") shouldBe false
  }

  "the face carries Latin, so a mixed run needs no fallback" in {
    // Like the Indic faces it is built with a Latin companion, which is what lets an Ethiopic document set a
    // roman word or a page folio without dropping to the fallback face.
    covers(regular, "Ethiopia 2026") shouldBe true
  }
