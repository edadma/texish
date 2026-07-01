package io.github.edadma.texish.opentype

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Font-free tests for the Devanagari cluster front end: character categorization, cluster segmentation and
  * base identification. The example words are the same ones checked against the bundled font with hb-shape —
  * हिन्दी (ha, i-matra, na, virama, da, ii-matra) segments into two clusters, and क्ष (ka, virama, ssa)
  * stays one. */
class DevanagariTests extends AnyFreeSpec with Matchers:

  import Devanagari.*
  import Devanagari.Category.*

  private def cps(s: String): Array[Int] = s.toArray.map(_.toInt)

  "categorization places the Devanagari characters" in {
    category(0x0915) shouldBe Consonant       // क
    category(0x0939) shouldBe Consonant       // ह
    category(0x0905) shouldBe IndependentVowel // अ
    category(0x094D) shouldBe Virama          // ्
    category(0x093C) shouldBe Nukta           // ़
    category(0x093F) shouldBe PreBaseMatra    // ि  (the reordering sign)
    category(0x0940) shouldBe Matra           // ी
    category(0x0941) shouldBe Matra           // ु
    category(0x0902) shouldBe SyllableModifier // ं anusvara
    category(0x0966) shouldBe Other           // ० digit
    category('A'.toInt) shouldBe Other        // outside the block
  }

  "hasDevanagari sees letters and signs but not a bare digit" in {
    hasDevanagari("हिन्दी") shouldBe true
    hasDevanagari("hello") shouldBe false
    hasDevanagari("०") shouldBe false // a Devanagari digit alone needs no shaping
  }

  "हिन्दी segments into two clusters at the virama-free consonant" in {
    // ह ि | न ् द ी — the na begins a new cluster (its predecessor ि is not a virama); the da stays in it
    // because a virama binds it to the na.
    clusters(cps("हिन्दी")).toList shouldBe List((0, 2), (2, 6))
  }

  "a virama keeps a conjunct in one cluster" in {
    clusters(cps("क्ष")).toList shouldBe List((0, 3)) // क ् ष — ssa is held by the virama
  }

  "an independent vowel starts its own cluster" in {
    clusters(cps("अक")).toList shouldBe List((0, 1), (1, 2)) // अ | क
  }

  "the base is the last consonant of the cluster" in {
    val h = cps("हिन्दी")
    baseIndex(h, 0, 2) shouldBe 0 // ह in the first cluster
    baseIndex(h, 2, 6) shouldBe 4 // द (not the half-form na) in the second
    val k = cps("क्ष")
    baseIndex(k, 0, 3) shouldBe 2 // ष is the base of the conjunct
  }
