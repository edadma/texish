package io.github.edadma.texish.opentype

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Font-free tests for the Tamil cluster front end: character categorization, cluster segmentation, base
  * identification and the reordering of the pre-base vowel signs. The example words are the same ones checked
  * against the bundled font with hb-shape.
  *
  * What sets Tamil apart from the other Indic scripts texish sets, and what these tests pin down, is that a
  * silenced consonant keeps its virama as a visible pulli instead of folding into a half-form. It therefore
  * stands as a letter of its own, and a pre-base vowel sign moves before the consonant it belongs to rather
  * than to the front of the whole cluster — the one place Tamil parts company with Devanagari and Bengali.
  */
class TamilTests extends AnyFreeSpec with Matchers:

  import Tamil.*
  import IndicCategory.*

  private def cps(s: String): Array[Int] = s.toArray.map(_.toInt)

  "categorization places the Tamil characters" in {
    category(0x0b95) shouldBe Consonant        // க ka
    category(0x0bb9) shouldBe Consonant        // ஹ ha
    category(0x0ba9) shouldBe Consonant        // ன na (the alveolar one)
    category(0x0bb4) shouldBe Consonant        // ழ zha
    category(0x0b85) shouldBe IndependentVowel // அ a
    category(0x0b94) shouldBe IndependentVowel // ஔ au
    category(0x0bcd) shouldBe Virama           // ் pulli
    category(0x0bbe) shouldBe Matra            // ா aa sign
    category(0x0bbf) shouldBe Matra            // ி i sign — post-base in Tamil, unlike Bengali's
    category(0x0bc1) shouldBe Matra            // ு u sign
    category(0x0bc6) shouldBe PreBaseMatra     // ெ e sign
    category(0x0bc7) shouldBe PreBaseMatra     // ே ee sign
    category(0x0bc8) shouldBe PreBaseMatra     // ை ai sign
    category(0x0bd7) shouldBe Matra            // ௗ au length mark
    category(0x0b83) shouldBe SyllableModifier // ஃ aytham
    category(0x0be7) shouldBe Other            // digit one
    category('A'.toInt) shouldBe Other         // outside the block
  }

  "Tamil has no nukta, so nothing in the block is one" in {
    (0x0b80 to 0x0bff).filter(category(_) == Nukta) shouldBe empty
  }

  "the unassigned codepoints in the consonant rows are not mistaken for letters" in {
    // Tamil writes one letter where Sanskrit writes a whole voiced/aspirated set, so its consonant rows are
    // full of holes. Each hole must fall through to Other or a stray byte would join a cluster.
    category(0x0b96) shouldBe Other // between க and ங
    category(0x0b9b) shouldBe Other // between ச and ஜ
    category(0x0ba0) shouldBe Other // between ட and ண
    category(0x0ba6) shouldBe Other // between த and ந
    category(0x0bab) shouldBe Other // between ப and ம
    category(0x0bc9) shouldBe Other // between the ai and o signs
  }

  "hasTamil sees letters and signs but not a bare digit" in {
    hasTamil("தமிழ்") shouldBe true
    hasTamil("hello") shouldBe false
    hasTamil("௧") shouldBe false // a Tamil digit alone needs no shaping
  }

  "தெரியும் segments into four clusters" in {
    // தெ | ரி | யு | ம் — each a consonant with its vowel sign, the last silenced by the pulli. These are
    // the cluster starts hb-shape reports (0, 2, 4, 6).
    clusters(cps("தெரியும்")).toList shouldBe List((0, 2), (2, 4), (4, 6), (6, 8))
  }

  "a pulli keeps the silenced consonant in the same cluster as the one it precedes" in {
    // க ் க is one cluster to the shaper even though Tamil reads it as two letters: the font's ligature
    // lookups need the whole sequence in front of them, which is how க்ஷ becomes one glyph.
    clusters(cps("க்க")).toList shouldBe List((0, 3))
    clusters(cps("க்ஷ")).toList shouldBe List((0, 3))
  }

  "an independent vowel and the aytham each start their own cluster" in {
    clusters(cps("அக")).toList shouldBe List((0, 1), (1, 2))  // அ | க
    clusters(cps("ஃபோ")).toList shouldBe List((0, 1), (1, 3)) // ஃ | போ
  }

  "the base is the last consonant, as in Devanagari and unlike Telugu" in {
    baseIndex(cps("க்க"), 0, 3) shouldBe 2
    baseIndex(cps("கே"), 0, 2) shouldBe 0
  }

  "a cluster of only an independent vowel reports its first character as the base" in {
    baseIndex(cps("அ"), 0, 1) shouldBe 0
  }

  "the three two-part vowel signs split into the parts the font draws" in {
    // Each is one codepoint in memory and two signs on the page: an e or ee sign before the base and an aa
    // sign or length mark after it, per Unicode canonical decomposition.
    decompose(0x0bca) shouldBe Some((0x0bc6, 0x0bbe)) // ொ o
    decompose(0x0bcb) shouldBe Some((0x0bc7, 0x0bbe)) // ோ oo
    decompose(0x0bcc) shouldBe Some((0x0bc6, 0x0bd7)) // ௌ au
    decompose(0x0bc8) shouldBe None                   // ை ai is a single sign in Tamil
    decompose(0x0bbe) shouldBe None
    decompose(0x0b95) shouldBe None
  }

  "the pre-base signs are e, ee and ai — the i sign is not among them" in {
    preBaseMatras shouldBe Set(0x0bc6, 0x0bc7, 0x0bc8)
    preBaseMatras should not contain 0x0bbf // ி is drawn above and right of its consonant, and stays there
    preBaseMatras should not contain 0x0bc0 // ீ likewise
  }

  "a pre-base sign on a lone consonant moves to the front" in {
    // கே — ka then the ee sign. With no pulli in the way the sign reaches the front of the cluster, which is
    // where the base is anyway (100 = ka, 200 = the ee sign, 900 = the pulli, absent here).
    reorderPreBaseMatra(cps("கே"), Array(100, 200), 200, 900).toList shouldBe List(200, 100)
  }

  "a pre-base sign moves before its own consonant, NOT before a silenced one" in {
    // க்கே — ka, pulli, ka, ee. hb-shape sets this as ka, pulli, ee, ka: the first ka is a complete letter
    // and the sign belongs to the second, so it stops at the pulli. This is the whole difference between
    // Tamil and Devanagari, where the same shape would put the sign at the very front.
    reorderPreBaseMatra(cps("க்கே"), Array(100, 900, 110, 200), 200, 900).toList shouldBe
      List(100, 900, 200, 110)
    preBaseMatraBeforeBase shouldBe true
  }

  "a sign whose pulli was swallowed by a ligature reaches the front after all" in {
    // க்ஷே — the akhn feature has already fused ka, pulli and ssa into the single ksha glyph (500), so no
    // pulli is left to stop the sign and it lands at the front, as hb-shape reports.
    reorderPreBaseMatra(cps("க்ஷே"), Array(500, 200), 200, 900).toList shouldBe List(200, 500)
  }

  "the sign stops at the LAST pulli when more than one consonant is silenced" in {
    reorderPreBaseMatra(cps("ங்க்கே"), Array(100, 900, 110, 900, 120, 200), 200, 900).toList shouldBe
      List(100, 900, 110, 900, 200, 120)
  }

  "a cluster with only post-base signs is left untouched" in {
    reorderPreBaseMatra(cps("கா"), Array(100, 250), 200, 900).toList shouldBe List(100, 250)
    reorderPreBaseMatra(cps("கி"), Array(100, 250), 200, 900).toList shouldBe List(100, 250)
  }

  "Tamil never forms a reph, even on a word-initial ra with a pulli" in {
    // ர்க — ra, pulli, ka. In Devanagari the ra would rise as a reph over the ka; in Tamil it keeps its
    // pulli and stays an ordinary letter, which is what hb-shape reports.
    startsWithReph(cps("ர்க")) shouldBe false
    clusters(cps("ர்க")).toList shouldBe List((0, 3))
  }

  "Tamil subjoins nothing, so no sign moves back across a conjunct" in {
    preSubjoinedMatras shouldBe empty
  }
