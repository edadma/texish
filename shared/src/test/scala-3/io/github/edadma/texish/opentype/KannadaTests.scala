package io.github.edadma.texish.opentype

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Font-free tests for the Kannada cluster front end: character categorization, cluster segmentation, base
  * identification and the nested vowel-sign decomposition. Kannada builds downward like Telugu — the base is
  * the consonant that *opens* a conjunct, with everything after it subjoined beneath — but forms a reph like
  * Devanagari, and those two facts are what these tests pin. */
class KannadaTests extends AnyFreeSpec with Matchers:

  import Kannada.*
  import Kannada.Category.*

  private def cps(s: String): Array[Int] = s.toArray.map(_.toInt)

  "categorization places the Kannada characters" in {
    category(0x0c95) shouldBe Consonant        // ಕ ka
    category(0x0cb9) shouldBe Consonant        // ಹ ha
    category(0x0cdd) shouldBe Consonant        // ೝ nakaara pollu
    category(0x0c85) shouldBe IndependentVowel // ಅ a
    category(0x0ce0) shouldBe IndependentVowel // ೠ vocalic RR
    category(0x0ccd) shouldBe Virama           // ್
    category(0x0cbc) shouldBe Nukta            // ಼
    category(0x0cbe) shouldBe Matra            // ಾ aa
    category(0x0cbf) shouldBe Matra            // ಿ i — above-base, not pre-base
    category(0x0cd5) shouldBe Matra            // ೕ length mark
    category(0x0c82) shouldBe SyllableModifier // ಂ anusvara
    category(0x0c83) shouldBe SyllableModifier // ಃ visarga
    category(0x0cbd) shouldBe Other            // ಽ avagraha stands on its own
    category(0x0ce6) shouldBe Other            // ೦ digit
    category('A'.toInt) shouldBe Other         // outside the block
  }

  "Kannada has no pre-base vowel signs" in {
    // Even the i sign is written as a hook rising from the top of its consonant rather than to its left, so
    // unlike Devanagari and Bengali nothing is ever lifted to the front of a cluster.
    preBaseMatras shouldBe empty
  }

  "hasKannada sees letters and signs but not a bare digit" in {
    hasKannada("ಕನ್ನಡ") shouldBe true
    hasKannada("hello") shouldBe false
    hasKannada("೦") shouldBe false // a Kannada digit alone needs no shaping
  }

  "ಕನ್ನಡ segments into three clusters" in {
    // ಕ | ನ ್ ನ | ಡ — the virama binds the second na to the first, and the Da opens a cluster of its own.
    clusters(cps("ಕನ್ನಡ")).toList shouldBe List((0, 1), (1, 4), (4, 5))
  }

  "a virama keeps a conjunct in one cluster" in {
    clusters(cps("ಕ್ಕ")).toList shouldBe List((0, 3))
  }

  "an independent vowel starts its own cluster" in {
    clusters(cps("ಅಕ")).toList shouldBe List((0, 1), (1, 2)) // ಅ | ಕ
  }

  "the base is the FIRST consonant of the cluster, not the last" in {
    // This is the Telugu rule rather than the Devanagari one: a joined consonant hangs beneath the base, so
    // the syllable is built downward from the consonant that opens it.
    val k = cps("ಕ್ಕ")
    baseIndex(k, 0, 3) shouldBe 0
    val w = cps("ಕನ್ನಡ")
    baseIndex(w, 1, 4) shouldBe 1 // the first na, with the second subjoined under it
  }

  "the composed vowel signs decompose into the parts the font draws" in {
    decompose(0x0cc0) shouldBe Some((0x0cbf, 0x0cd5)) // ii = i + length
    decompose(0x0cc7) shouldBe Some((0x0cc6, 0x0cd5)) // ee = e + length
    decompose(0x0cc8) shouldBe Some((0x0cc6, 0x0cd6)) // ai = e + ai length
    decompose(0x0cca) shouldBe Some((0x0cc6, 0x0cc2)) // o  = e + uu
  }

  "the oo sign decomposes into a sign that decomposes again" in {
    // oo is the o sign plus a length mark, and the o sign is itself an e sign plus a uu sign. Declaring the
    // first step is enough — IndicShaper applies the split until nothing decomposes further — but it means a
    // single pass would leave an o the font has no glyph for.
    decompose(0x0ccb) shouldBe Some((0x0cca, 0x0cd5))
    decompose(0x0cca) shouldBe defined // …and the part itself splits
  }

  "the au sign is the one composite drawn with a glyph of its own" in {
    decompose(0x0ccc) shouldBe None
    decompose(0x0cbe) shouldBe None // and the simple signs never split
  }

  "only the signs drawn on the base move back across the subjoined consonants" in {
    // aa, i, u, uu, e and au are drawn on the base and must reach it; the vocalic r/rr and L/LL signs and the
    // two length marks hang below or after the whole syllable and stay where they were typed.
    preSubjoinedMatras should contain allOf (0x0cbe, 0x0cbf, 0x0cc1, 0x0cc2, 0x0cc6, 0x0ccc)
    preSubjoinedMatras should contain noneOf (0x0cc3, 0x0cc4, 0x0cd5, 0x0cd6, 0x0ce2, 0x0ce3)
  }

  "a syllable-opening ra + virama before a base is a reph" in {
    // Unlike Telugu, which leaves the ra an ordinary base, Kannada lifts it out as the arkavattu.
    startsWithReph(cps("ರ್ಕ")) shouldBe true
  }

  "a ra + virama with no following consonant is not a reph" in {
    startsWithReph(cps("ರ್")) shouldBe false // a dead ra, nothing to follow
  }

  "a ra that does not open the cluster is not a reph" in {
    startsWithReph(cps("ಕ್ರ")) shouldBe false // ka virama ra — the ra subjoins here
  }
