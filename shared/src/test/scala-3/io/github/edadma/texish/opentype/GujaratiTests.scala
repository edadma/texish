package io.github.edadma.texish.opentype

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Font-free tests for the Gujarati cluster front end: character categorization, cluster segmentation and base
  * identification. The example words are ordinary ones — ગુજરાતી (the language's own name) segments into four
  * clusters, and ક્ષ (ka, virama, ssa) stays one because the virama binds it. */
class GujaratiTests extends AnyFreeSpec with Matchers:

  import Gujarati.*
  import Gujarati.Category.*

  private def cps(s: String): Array[Int] = s.toArray.map(_.toInt)

  "categorization places the Gujarati characters" in {
    category(0x0A95) shouldBe Consonant        // ક
    category(0x0AB9) shouldBe Consonant        // હ
    category(0x0AF9) shouldBe Consonant        // ૹ ZHA, a letter for a borrowed sound
    category(0x0A85) shouldBe IndependentVowel // અ
    category(0x0AE0) shouldBe IndependentVowel // ૠ vocalic RR
    category(0x0ACD) shouldBe Virama           // ્
    category(0x0ABC) shouldBe Nukta            // ઼
    category(0x0ABF) shouldBe PreBaseMatra     // િ  (the reordering sign)
    category(0x0AC0) shouldBe Matra            // ી
    category(0x0AC1) shouldBe Matra            // ુ
    category(0x0ACB) shouldBe Matra            // ો
    category(0x0A82) shouldBe SyllableModifier // ં anusvara
    category(0x0AFB) shouldBe SyllableModifier // shadda, written over a consonant
    category(0x0ABD) shouldBe Other            // ઽ avagraha stands on its own
    category(0x0AE6) shouldBe Other            // ૦ digit
    category(0x0AF1) shouldBe Other            // ૱ rupee sign
    category('A'.toInt) shouldBe Other         // outside the block
  }

  "hasGujarati sees letters and signs but not a bare digit" in {
    hasGujarati("ગુજરાતી") shouldBe true
    hasGujarati("hello") shouldBe false
    hasGujarati("૦") shouldBe false // a Gujarati digit alone needs no shaping
  }

  "ગુજરાતી segments into four clusters" in {
    // ગ ુ | જ | ર ા | ત ી — each consonant that no virama holds to its predecessor opens a cluster, and the
    // vowel signs attach to the one in progress.
    clusters(cps("ગુજરાતી")).toList shouldBe List((0, 2), (2, 3), (3, 5), (5, 7))
  }

  "a virama keeps a conjunct in one cluster" in {
    clusters(cps("ક્ષ")).toList shouldBe List((0, 3)) // ક ્ ષ — ssa is held by the virama
  }

  "an independent vowel starts its own cluster" in {
    clusters(cps("અક")).toList shouldBe List((0, 1), (1, 2)) // અ | ક
  }

  "the base is the last consonant of the cluster" in {
    val g = cps("ગુજરાતી")
    baseIndex(g, 0, 2) shouldBe 0 // ગ in the first cluster
    baseIndex(g, 3, 5) shouldBe 3 // ર in the third
    val k = cps("ક્ષ")
    baseIndex(k, 0, 3) shouldBe 2 // ષ is the base of the conjunct
  }

  "the pre-base i moves to the front of its cluster's glyphs" in {
    // કિ — ka then the i sign. After the (empty here) basic pass the glyphs stand in memory order [ka, i];
    // the i sign is lifted to the front so it is drawn before its base, as Gujarati requires. The glyph ids
    // are stand-ins: 100 for ka, 200 for the i sign U+0ABF.
    val cluster = cps("કિ")
    reorderPreBaseMatra(cluster, Array(100, 200), 200, 900).toList shouldBe List(200, 100)
  }

  "the pre-base i moves ahead of a half-form and its base" in {
    // A conjunct cluster with an i sign: the sign jumps to the very front, before the half-form and the base
    // alike (300 = half-form, 400 = base consonant, 200 = the i sign U+0ABF).
    val cluster = cps("ન્દિ") // na virama da i-sign
    reorderPreBaseMatra(cluster, Array(300, 400, 200), 200, 900).toList shouldBe List(200, 300, 400)
  }

  "a cluster without an i sign is left untouched" in {
    val cluster = cps("કા") // ka aa-matra — the aa sign is post-base, no reordering
    reorderPreBaseMatra(cluster, Array(100, 250), 200, 900).toList shouldBe List(100, 250)
  }

  "a word-initial ra + virama before a base is a reph" in {
    startsWithReph(cps("ર્ક")) shouldBe true // ra virama ka — the ra is drawn as a reph over ka
  }

  "a ra + virama with no following consonant is not a reph" in {
    startsWithReph(cps("ર્")) shouldBe false // a dead ra, nothing to sit over
  }

  "a ra that does not open the cluster is not a reph" in {
    startsWithReph(cps("ક્ર")) shouldBe false // ka virama ra — the ra is below-base here, not a reph
    startsWithReph(cps("રા")) shouldBe false  // ra with a vowel sign — an ordinary base, no virama
  }

  "Gujarati has no two-part matras and no word-position forms" in {
    // Unlike Bengali's o/au and Telugu's ai, every Gujarati vowel sign is one codepoint the font maps on its
    // own — including ો and ૌ, which are drawn as an aa sign with a mark above it.
    decompose(0x0ACB) shouldBe None
    decompose(0x0ACC) shouldBe None
    initFeature shouldBe None
    finaFeature shouldBe None
  }
