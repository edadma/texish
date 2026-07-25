package io.github.edadma.texish.opentype

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Font-free tests for the Gurmukhi cluster front end: character categorization, cluster segmentation, base
  * identification and pre-base reordering. The example words are the same ones checked against the bundled font
  * with hb-shape — ਕਿ (ka, sihari) reorders the i-sign to the front, ਪ੍ਰ (pa, virama, ra) holds a subjoined ra
  * in one cluster with ra as the base, and Gurmukhi never forms a reph. */
class GurmukhiTests extends AnyFreeSpec with Matchers:

  import Gurmukhi.*
  import IndicCategory.*

  private def cps(s: String): Array[Int] = s.toArray.map(_.toInt)

  "categorization places the Gurmukhi characters" in {
    category(0x0A15) shouldBe Consonant        // ਕ ka
    category(0x0A39) shouldBe Consonant        // ਹ ha
    category(0x0A5C) shouldBe Consonant        // ੜ rra (nukta-composed)
    category(0x0A72) shouldBe IndependentVowel // ੲ iri (a vowel bearer)
    category(0x0A05) shouldBe IndependentVowel // ਅ a
    category(0x0A14) shouldBe IndependentVowel // ਔ au
    category(0x0A4D) shouldBe Virama           // ੍ virama
    category(0x0A3C) shouldBe Nukta            // ਼ nukta
    category(0x0A3F) shouldBe PreBaseMatra     // ਿ sihari (reorders)
    category(0x0A3E) shouldBe Matra            // ਾ aa-sign (post-base)
    category(0x0A41) shouldBe Matra            // ੁ aunkar u-sign (below-base)
    category(0x0A70) shouldBe SyllableModifier // ੰ tippi
    category(0x0A71) shouldBe SyllableModifier // ੱ addak
    category(0x0A66) shouldBe Other            // ੦ digit
    category('A'.toInt) shouldBe Other         // outside the block
  }

  "hasGurmukhi sees letters and signs but not a bare digit" in {
    hasGurmukhi("ਪੰਜਾਬੀ") shouldBe true
    hasGurmukhi("hello") shouldBe false
    hasGurmukhi("੦") shouldBe false // a Gurmukhi digit alone needs no shaping
  }

  "ਕਿ keeps the consonant and its pre-base sign in one cluster" in {
    clusters(cps("ਕਿ")).toList shouldBe List((0, 2)) // ਕ ਿ — the sihari attaches to the ka
  }

  "a virama keeps a conjunct in one cluster" in {
    clusters(cps("ਪ੍ਰ")).toList shouldBe List((0, 3)) // ਪ ੍ ਰ — the ra is held by the virama
  }

  "addak binds to the preceding cluster; the next consonant opens a new one" in {
    // ਪ ੱ | ਕ ਾ — the addak stays with pa, and the ka begins a fresh cluster carrying the aa-sign.
    clusters(cps("ਪੱਕਾ")).toList shouldBe List((0, 2), (2, 4))
  }

  "an independent vowel starts its own cluster" in {
    clusters(cps("ਅਕ")).toList shouldBe List((0, 1), (1, 2)) // ਅ | ਕ
  }

  "the base is the last consonant of the cluster" in {
    baseIndex(cps("ਕਿ"), 0, 2) shouldBe 0 // ka in ਕਿ
    baseIndex(cps("ਪ੍ਰ"), 0, 3) shouldBe 2 // ra (the subjoined consonant) is the base in ਪ੍ਰ
  }

  "the sihari moves to the front of its cluster's glyphs" in {
    // ਕਿ — ka then the sihari. After the (empty here) basic pass the glyphs stand in memory order [ka, i];
    // the sihari is lifted to the front so it is drawn before its base. Glyph ids are stand-ins: 100 for ka,
    // 200 for the sihari U+0A3F.
    reorderPreBaseMatra(cps("ਕਿ"), Array(100, 200), 200).toList shouldBe List(200, 100)
  }

  "the sihari moves ahead of a conjunct and its base" in {
    // A conjunct cluster with a sihari (300 = the subjoined form, 400 = base consonant, 200 = sihari U+0A3F).
    reorderPreBaseMatra(cps("ਸ੍ਰਿ"), Array(300, 400, 200), 200).toList shouldBe List(200, 300, 400)
  }

  "a cluster with only a post-base sign is left untouched" in {
    // ਕਾ — ka aa-sign: the aa is post-base, no reordering.
    reorderPreBaseMatra(cps("ਕਾ"), Array(100, 250), 200).toList shouldBe List(100, 250)
  }

  "Gurmukhi never forms a reph, even on a word-initial ra with a virama" in {
    // ਰ੍ਕ — ra, virama, ka. In Devanagari this ra would rise as a reph; in Gurmukhi the ra takes a subjoined
    // below-base form and stays in place, so the shaper reports no reph and the base is the ka.
    startsWithReph(cps("ਰ੍ਕ")) shouldBe false
    clusters(cps("ਰ੍ਕ")).toList shouldBe List((0, 3))
    baseIndex(cps("ਰ੍ਕ"), 0, 3) shouldBe 2
  }
