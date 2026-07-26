package io.github.edadma.texish.opentype

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Font-free tests for the Telugu cluster front end: character categorization, cluster segmentation and base
  * identification. The example words are the same ones checked against the bundled font with hb-shape —
  * పుస్తకం (book) segments into three clusters, and క్క holds a conjunct in one.
  *
  * Telugu differs from the other Indic scripts texish sets in two ways these tests pin down: the base of a
  * conjunct is its *first* consonant, the joined ones hanging beneath it as subscripts; and no vowel sign is
  * drawn before its consonant, but almost every one is drawn *on* the base and so is moved back across those
  * subscripts to reach it.
  */
class TeluguTests extends AnyFreeSpec with Matchers:

  import Telugu.*
  import IndicCategory.*

  private def cps(s: String): Array[Int] = s.toArray.map(_.toInt)

  "categorization places the Telugu characters" in {
    category(0x0c15) shouldBe Consonant        // క ka
    category(0x0c39) shouldBe Consonant        // హ ha
    category(0x0c58) shouldBe Consonant        // tsa
    category(0x0c05) shouldBe IndependentVowel // అ a
    category(0x0c14) shouldBe IndependentVowel // ఔ au
    category(0x0c60) shouldBe IndependentVowel // vocalic RR
    category(0x0c4d) shouldBe Virama           // ్ virama
    category(0x0c3c) shouldBe Nukta            // nukta
    category(0x0c3e) shouldBe Matra            // ా aa-sign
    category(0x0c3f) shouldBe Matra            // ి i-sign
    category(0x0c48) shouldBe Matra            // ై ai-sign
    category(0x0c02) shouldBe SyllableModifier // ం anusvara
    category(0x0c03) shouldBe SyllableModifier // ః visarga
    category(0x0c66) shouldBe Other            // digit zero
    category('A'.toInt) shouldBe Other         // outside the block
  }

  "the reserved codepoints inside the block are not mistaken for letters" in {
    category(0x0c0d) shouldBe Other // reserved between vocalic L and e
    category(0x0c29) shouldBe Other // reserved between na and pa
    category(0x0c45) shouldBe Other // reserved between vocalic rr and e signs
  }

  "hasTelugu sees letters and signs but not a bare digit" in {
    hasTelugu("తెలుగు") shouldBe true
    hasTelugu("hello") shouldBe false
    hasTelugu("౦") shouldBe false // a Telugu digit alone needs no shaping
  }

  "పుస్తకం segments into three clusters" in {
    // ప ు | స ్ త | క ం — the sa opens a cluster the ta is bound into by the virama, and the ka opens the
    // last, carrying the anusvara. These are the cluster starts hb-shape reports (0, 2, 5).
    clusters(cps("పుస్తకం")).toList shouldBe List((0, 2), (2, 5), (5, 7))
  }

  "a virama keeps a conjunct in one cluster" in {
    clusters(cps("క్క")).toList shouldBe List((0, 3)) // క ్ క
  }

  "an independent vowel starts its own cluster" in {
    clusters(cps("అక")).toList shouldBe List((0, 1), (1, 2)) // అ | క
  }

  "the base is the FIRST consonant, the joined ones subjoining beneath it" in {
    // The reverse of Devanagari and Bengali, whose base is the last consonant of the conjunct.
    baseIndex(cps("క్క"), 0, 3) shouldBe 0
    baseIndex(cps("స్త"), 0, 3) shouldBe 0
    baseIndex(cps("కి"), 0, 2) shouldBe 0
  }

  "a cluster of only an independent vowel reports its first character as the base" in {
    baseIndex(cps("అ"), 0, 1) shouldBe 0
  }

  "Telugu has no pre-base vowel signs, so nothing reorders" in {
    preBaseMatras shouldBe empty
    // With no pre-base sign there is nothing to lift, so the glyphs come back in the order they were shaped.
    reorderPreBaseMatra(cps("కి"), Array(100, 200), 200, 900).toList shouldBe List(100, 200)
  }

  "almost every vowel sign belongs beside the base, ahead of the subjoined consonants" in {
    // The vocalic r pair is drawn below the syllable and so stays after whatever is subjoined; every other
    // sign is drawn on the base itself and moves back to it. Confirmed sign by sign against hb-shape.
    preSubjoinedMatras should contain(0x0c3e)     // ా aa
    preSubjoinedMatras should contain(0x0c3f)     // ి i
    preSubjoinedMatras should contain(0x0c41)     // ు u
    preSubjoinedMatras should contain(0x0c47)     // ే ee
    preSubjoinedMatras should contain(0x0c55)     // length mark
    preSubjoinedMatras should contain(0x0c62)     // vocalic l sign
    preSubjoinedMatras should not contain 0x0c43  // ృ vocalic r — drawn below, stays after
    preSubjoinedMatras should not contain 0x0c44  // ౄ vocalic rr — likewise
    preSubjoinedMatras should not contain 0x0c15  // a consonant is not a sign
    preSubjoinedMatras should not contain 0x0c4d  // nor the virama
  }

  "the ai sign is written as the e sign and the ai length mark" in {
    // The font carries the pair, not the single sign, so ై splits before the cluster is shaped. No other
    // Telugu sign has two parts.
    decompose(0x0c48) shouldBe Some((0x0c46, 0x0c56))
    decompose(0x0c47) shouldBe None
    decompose(0x0c3e) shouldBe None
    decompose(0x0c15) shouldBe None
  }

  "a vowel sign moves back across the consonants subjoined beneath it" in {
    // The sign is typed after the whole conjunct but belongs to the base alone, so it is moved to the base's
    // side — స్త్రే sets sa, its e sign, then the subjoined ta and ra, which is the order hb-shape reports.
    // Nothing else changes place: a trailing anusvara stays last.
    reorderPreSubjoinedMatra(Array(10, 20, 30), Array(30)).toList shouldBe List(10, 30, 20)
    reorderPreSubjoinedMatra(Array(10, 20, 21, 30), Array(30)).toList shouldBe List(10, 30, 20, 21)
    reorderPreSubjoinedMatra(Array(10, 20, 30, 40), Array(30)).toList shouldBe List(10, 30, 20, 40)
  }

  "both parts of a two-part sign move together, in reading order" in {
    reorderPreSubjoinedMatra(Array(10, 20, 30, 31), Array(30, 31)).toList shouldBe List(10, 30, 31, 20)
  }

  "a sign already beside its base, or one the font consumed, leaves the run alone" in {
    reorderPreSubjoinedMatra(Array(10, 30, 20), Array(30)).toList shouldBe List(10, 30, 20)
    reorderPreSubjoinedMatra(Array(10, 20), Array.empty).toList shouldBe List(10, 20)
    reorderPreSubjoinedMatra(Array(10, 20), Array(99)).toList shouldBe List(10, 20) // 99 was substituted away
    reorderPreSubjoinedMatra(Array(10), Array(10)).toList shouldBe List(10)         // the base is never moved
  }

  "Telugu never forms a reph, even on a word-initial ra with a virama" in {
    // ర్క — ra, virama, ka. In Devanagari this ra would rise as a reph; in Telugu it stays an ordinary base
    // and the ka subjoins beneath it, which is what hb-shape reports (ratelu + kapostscripttelu).
    startsWithReph(cps("ర్క")) shouldBe false
    clusters(cps("ర్క")).toList shouldBe List((0, 3))
    baseIndex(cps("ర్క"), 0, 3) shouldBe 0 // the ra itself is the base
  }
