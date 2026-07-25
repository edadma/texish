package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.OtfFont
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Glyph coverage of the bundled CJK faces, read from disk. CJK scripts need no shaping (they are not
  * cursive and do not reorder), so what matters is that each region's face carries the glyphs its language
  * uses — in particular that the Japanese face has the kanji and kana, and the Korean face has the Hangul
  * syllables the Chinese faces lack entirely. */
class CJKFontCoverageTests extends AnyFreeSpec with Matchers:

  private def font(path: String): OtfFont = new OtfFont(Files.readAllBytes(Paths.get(path)))
  private def covers(f: OtfFont, s: String): Boolean =
    s.codePoints().allMatch(cp => f.glyphIndex(cp) != 0)

  private val jp = font("fonts/NotoSerifCJK/NotoSerifJP-Regular.otf")
  private val kr = font("fonts/NotoSerifCJK/NotoSerifKR-Regular.otf")
  private val sc = font("fonts/NotoSerifCJK/NotoSerifSC-Regular.ttf")

  "the Japanese face covers kanji and both kana" in {
    covers(jp, "日本語") shouldBe true    // kanji
    covers(jp, "ひらがな") shouldBe true // hiragana
    covers(jp, "カタカナ") shouldBe true // katakana
  }

  "the Korean face covers Hangul syllables and Hanja" in {
    covers(kr, "한국어") shouldBe true       // Hangul
    covers(kr, "안녕하세요") shouldBe true // Hangul
    covers(kr, "大韓民國") shouldBe true    // Hanja (Han used in Korean)
  }

  "the Chinese face has no Hangul, which is why the Korean face is bundled" in {
    // Every Hangul syllable is .notdef in the SC face — a Korean run set in it would be missing-glyph boxes.
    sc.glyphIndex('한') shouldBe 0
    sc.glyphIndex('국') shouldBe 0
    covers(sc, "中文") shouldBe true // but it covers its own Han, of course
  }

  "the bold cuts carry the same coverage" in {
    val jpBold = font("fonts/NotoSerifCJK/NotoSerifJP-Bold.otf")
    val krBold = font("fonts/NotoSerifCJK/NotoSerifKR-Bold.otf")
    covers(jpBold, "日本語ひらがな") shouldBe true
    covers(krBold, "한국어") shouldBe true
  }
