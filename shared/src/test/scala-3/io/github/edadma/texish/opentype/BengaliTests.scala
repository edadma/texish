package io.github.edadma.texish.opentype

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Font-free tests for the Bengali cluster front end: character categorization, cluster segmentation, base
  * identification, two-part vowel decomposition and pre-base reordering. The example words are the same ones
  * checked against the bundled font with hb-shape — কিন্তু (ka, i, na, virama, ta, u) segments into two
  * clusters, and ক্ত (ka, virama, ta) stays one. */
class BengaliTests extends AnyFreeSpec with Matchers:

  import Bengali.*
  import IndicCategory.*

  private def cps(s: String): Array[Int] = s.toArray.map(_.toInt)

  "categorization places the Bengali characters" in {
    category(0x0995) shouldBe Consonant        // ক ka
    category(0x09B9) shouldBe Consonant        // হ ha
    category(0x09CE) shouldBe Consonant        // ৎ khanda ta
    category(0x09DF) shouldBe Consonant        // য় yya (nukta-composed)
    category(0x0985) shouldBe IndependentVowel // অ A
    category(0x0994) shouldBe IndependentVowel // ঔ AU
    category(0x09CD) shouldBe Virama           // ্ hasant
    category(0x09BC) shouldBe Nukta            // ় nukta
    category(0x09BF) shouldBe PreBaseMatra     // ি i-sign (reorders)
    category(0x09C7) shouldBe PreBaseMatra     // ে e-sign (reorders)
    category(0x09C8) shouldBe PreBaseMatra     // ৈ ai-sign (reorders)
    category(0x09BE) shouldBe Matra            // া aa-sign (post-base)
    category(0x09C1) shouldBe Matra            // ু u-sign (below-base)
    category(0x0982) shouldBe SyllableModifier // ং anusvara
    category(0x09E6) shouldBe Other            // ০ digit
    category('A'.toInt) shouldBe Other         // outside the block
  }

  "hasBengali sees letters and signs but not a bare digit" in {
    hasBengali("বাংলা") shouldBe true
    hasBengali("hello") shouldBe false
    hasBengali("০") shouldBe false // a Bengali digit alone needs no shaping
  }

  "কিন্তু segments into two clusters at the virama-free consonant" in {
    // ক ি | ন ্ ত ু — the na begins a new cluster (its predecessor ি is not a virama); the ta stays in it
    // because a virama binds it to the na.
    clusters(cps("কিন্তু")).toList shouldBe List((0, 2), (2, 6))
  }

  "a virama keeps a conjunct in one cluster" in {
    clusters(cps("ক্ত")).toList shouldBe List((0, 3)) // ক ্ ত — ta is held by the virama
  }

  "an independent vowel starts its own cluster" in {
    clusters(cps("অক")).toList shouldBe List((0, 1), (1, 2)) // অ | ক
  }

  "the base is the last consonant of the cluster" in {
    val k = cps("কিন্তু")
    baseIndex(k, 0, 2) shouldBe 0 // ka in the first cluster
    baseIndex(k, 2, 6) shouldBe 4 // ta (not the half-form na) in the second
  }

  "the two-part o and au signs decompose; every other sign does not" in {
    decompose(0x09CB) shouldBe Some((0x09C7, 0x09BE)) // o  → e-sign + aa-sign
    decompose(0x09CC) shouldBe Some((0x09C7, 0x09D7)) // au → e-sign + au length mark
    decompose(0x09BE) shouldBe None                   // aa is a plain post-base sign
    decompose(0x0995) shouldBe None                   // a consonant does not decompose
  }

  "a pre-base sign moves to the front of its cluster's glyphs" in {
    // কে — ka then the e-sign. After the (empty here) basic pass the glyphs stand in memory order [ka, e];
    // the e-sign is lifted to the front so it is drawn before its base. Glyph ids are stand-ins: 100 for ka,
    // 200 for the e-sign U+09C7.
    reorderPreBaseMatra(cps("কে"), Array(100, 200), 200, 900).toList shouldBe List(200, 100)
  }

  "a pre-base sign moves ahead of a conjunct and its base" in {
    // A conjunct cluster with an i-sign (300 = half-form, 400 = base consonant, 200 = i-sign U+09BF).
    reorderPreBaseMatra(cps("ন্তি"), Array(300, 400, 200), 200, 900).toList shouldBe List(200, 300, 400)
  }

  "a cluster with only a post-base sign is left untouched" in {
    // কা — ka aa-sign: the aa is post-base, no reordering.
    reorderPreBaseMatra(cps("কা"), Array(100, 250), 200, 900).toList shouldBe List(100, 250)
  }

  "a word-initial ra + virama before a base is a reph" in {
    startsWithReph(cps("র্ক")) shouldBe true // ra virama ka — the ra is drawn as a reph over ka
  }

  "a ra + virama with no following consonant is not a reph" in {
    startsWithReph(cps("র্")) shouldBe false // a dead ra, nothing to sit over
  }

  "a ra that does not open the cluster is not a reph" in {
    startsWithReph(cps("রা")) shouldBe false // ra with a vowel sign — an ordinary base, no virama
  }
