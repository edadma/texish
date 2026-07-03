package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{ArabicShaping, Gpos, Gsub, OtfFont}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Arabic harakat (vowel-mark) positioning against the bundled Noto Naskh Arabic face. A short vowel is a
  * GPOS mark like a Hebrew point, but Arabic stacks marks: a vowel written over a shadda sits above the
  * shadda (mark-to-mark), not at the bare-consonant anchor a mark-to-base lookup would give it. These
  * checks run the full shape → position pipeline and pin where each mark attaches. The parsers are pure
  * shared code; this test reads the real font from disk.
  */
class GposArabicMarkTests extends AnyFreeSpec with Matchers:

  private val font = new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoNaskhArabic/NotoNaskhArabic-Regular.ttf")))
  private val gsub = Gsub.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")).get
  private val gpos = Gpos.from(font.tableBytes("GPOS"), font.tableBytes("GDEF"), font.unitsPerEm).get

  private def g(cp: Int): Int = font.glyphIndex(cp)

  private def placements(cps: Array[Int]) =
    val glyphs = gsub.shape(cps.map(g), ArabicShaping.resolveForms(cps))
    (glyphs, gpos.position(glyphs))

  private val Beh    = 0x0628
  private val Meem   = 0x0645
  private val Shadda = 0x0651
  private val Fatha  = 0x064E

  "a shadda over a bare consonant attaches to the consonant" in {
    // Meem carries no dots, so meem + shadda is a base and one mark.
    val (glyphs, places) = placements(Array(Meem, Shadda))
    glyphs.length shouldBe 2
    places(0).isMark shouldBe false // meem is the base
    places(1).isMark shouldBe true  // shadda is a mark
    places(1).attach shouldBe 0     // attaches to the meem
  }

  "a vowel over a shadda stacks on the shadda, not on the consonant" in {
    // Logical order meem, shadda, fatha. The shadda attaches to the meem; the fatha must stack on the
    // shadda (mark-to-mark) rather than land at the meem's bare-consonant anchor and collide with it.
    val (glyphs, places) = placements(Array(Meem, Shadda, Fatha))
    glyphs.length shouldBe 3
    places(1).attach shouldBe 0    // shadda on the meem
    places(2).isMark shouldBe true
    places(2).attach shouldBe 1    // fatha on the shadda — the mark-to-mark stacking
  }

  "a vowel above a letter whose dot is below attaches to the letter, not the dot" in {
    // Beh decomposes to a dotless skeleton (base) plus a below-dot; the fatha sits above. The below-dot and
    // the above-vowel do not stack, so the fatha falls through to the skeleton rather than the dot.
    val (glyphs, places) = placements(Array(Beh, Fatha))
    val fatha = places.last
    fatha.isMark shouldBe true
    fatha.attach shouldBe 0 // the skeleton, not the below-dot
    places(fatha.attach).isMark shouldBe false
  }
