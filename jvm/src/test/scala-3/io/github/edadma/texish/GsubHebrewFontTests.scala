package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{Gsub, HebrewShaping, OtfFont}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Pointed Hebrew against the two bundled Hebrew faces, which want opposite things of the engine.
  *
  * Ezra SIL, the scholarly face cut for Biblical Hebrew, draws a letter and the point inside it as one glyph
  * and gives no anchor for placing that point alone: its composition feature has to run, and the points have
  * to be put into drawing order first or the rule never matches. Noto Serif Hebrew combines nothing and
  * places every point from its own anchor, so it must be left on the plain path untouched. Every sequence
  * below was confirmed against the same font with `hb-shape --shaper=ot`, whose right-to-left output is read
  * back to front.
  */
class GsubHebrewFontTests extends AnyFreeSpec with Matchers:

  private val ezra = new OtfFont(Files.readAllBytes(Paths.get("fonts/EzraSIL/SILEOT.ttf")))
  private val noto =
    new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifHebrew/NotoSerifHebrew-Regular.ttf")))

  private def shaperFor(f: OtfFont) = Gsub.fromHebrew(f.tableBytes("GSUB"), f.tableBytes("GDEF"))

  /** Shape a word the way CharBox does, or null when the font combines nothing and the run stays plain. */
  private def shape(f: OtfFont, word: String): Array[Int] =
    val cps = word.toArray.map(_.toInt)
    shaperFor(f) match
      case Some(gs) => HebrewShaping.shape(cps, f.glyphIndex, gs.applyFeatureByTag(_, "ccmp"))
      case None     => null

  "a pointed word combines its letters with the points drawn inside them" in {
    // Each of these stores a vowel between a letter and its dagesh or shin dot, so each depends on the points
    // being put into drawing order before the font's rule is given the run.
    shape(ezra, "דָּבָר").toSeq shouldBe Seq(353, 262, 276, 262, 299)      // dalet+dagesh, then its qamats
    shape(ezra, "כָּל").toSeq shouldBe Seq(360, 262, 287)                  // kaf+dagesh
    shape(ezra, "שָׁלוֹם").toSeq shouldBe Seq(344, 262, 287, 380, 288)      // shin+shin dot, vav+holam
    shape(ezra, "בַּיִת").toSeq shouldBe Seq(351, 261, 284, 258, 301)       // bet+dagesh
    shape(ezra, "צַדִּיק").toSeq shouldBe Seq(297, 261, 353, 258, 284, 298) // dalet+dagesh mid-word
    shape(ezra, "בְּרֵאשִׁית").toSeq shouldBe Seq(351, 254, 299, 259, 275, 344, 258, 284, 301)
    shape(ezra, "מִשְׁפָּט").toSeq shouldBe Seq(289, 258, 344, 254, 366, 262, 283)
    shape(ezra, "הַשָּׁמַיִם").toSeq shouldBe Seq(279, 261, 346, 262, 289, 261, 284, 258, 288)
  }

  "a rule that reads the letters around it still fires" in {
    // Ezra combines a lamed with its holam only in context — the pair alone is left apart — so the feature
    // must be given the whole run rather than the pair, which is why the run is not shaped a pair at a time.
    shape(ezra, "אֱלֹהִים").toSeq shouldBe Seq(275, 255, 382, 279, 258, 284, 288)
    shape(ezra, "וַיֹּאמֶר").toSeq shouldBe Seq(280, 261, 358, 263, 275, 289, 260, 299)
  }

  "a point the face does not combine is left for the mark shaper" in {
    // Which pairs combine is the font's to say. Ezra draws a letter with its dagesh or shin dot as one shape
    // but leaves a plain vowel on its letter, so a word carrying only vowels combines nothing and stays on
    // the plain path entirely.
    shape(ezra, "אֶחָד") shouldBe null   // alef+segol, het+qamats: vowels only
    shape(ezra, "הָאָרֶץ") shouldBe null // likewise
    // …while a word with both keeps the uncombined pair apart: eleven characters, of which only the shin's
    // dagesh and shin dot combine, leaving nine glyphs rather than eight.
    "הַשָּׁמַיִם".length shouldBe 11
    shape(ezra, "הַשָּׁמַיִם").length shouldBe 9
  }

  "unpointed Hebrew is left exactly as it is" in {
    // Nothing to combine, so the run stays on the plain path in both faces.
    shape(ezra, "שלום") shouldBe null
    shape(ezra, "ישראל") shouldBe null
    shape(noto, "שלום") shouldBe null
  }

  "the face that combines nothing still has its points put in drawing order" in {
    // Noto Serif Hebrew combines no pair, placing every point from its own anchor, so it keeps its letters
    // and points as separate glyphs. It is still ordered: the letter's own point comes to its side, which is
    // the order hb-shape reports. Nothing on the page depends on it in this face — each point is anchored to
    // the letter, not to another point — but ordering both faces alike keeps one path for Hebrew, and a face
    // that stacked one point on another would need it.
    shape(noto, "שָׁלוֹם").toSeq shouldBe Seq(61, 97, 140, 38, 23, 141, 40)
    shape(noto, "בְּרֵאשִׁית").toSeq shouldBe Seq(14, 144, 129, 59, 137, 10, 61, 97, 136, 31, 67)
    shape(noto, "דָּבָר").toSeq shouldBe Seq(19, 144, 140, 14, 140, 59)
    shape(noto, "יִשְׂרָאֵל").toSeq shouldBe Seq(31, 136, 61, 147, 129, 59, 140, 10, 137, 38)
    // …and the letters themselves are untouched: only the points moved.
    shape(noto, "דָּבָר").count(_ == 19) shouldBe 1
  }

  "a word whose points are already in order is left on the plain path" in {
    // Nothing to combine and nothing to move, so the run is not taken off the path it was always drawn on.
    shape(noto, "הָאָרֶץ") shouldBe null
  }

  "the Arabic face is not mistaken for a Hebrew one" in {
    val arabic = new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoNaskhArabic/NotoNaskhArabic-Regular.ttf")))
    shaperFor(arabic) shouldBe None
  }
