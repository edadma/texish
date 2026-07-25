package io.github.edadma.texish.opentype

import scala.collection.mutable.ArrayBuffer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Ordering the points of a pointed Hebrew letter and combining them into it ([[HebrewShaping]]).
  *
  * Canonical ordering sorts a syllable's points by combining class, which leaves the point drawn inside the
  * letter — the dagesh, the shin dot — separated from it by the vowels; a font's composition rule expects the
  * two together. These pin the drawing order the points are put into, which was confirmed against `hb-shape`
  * point by point, and the rule that a run only leaves the plain path when the font actually combines
  * something.
  */
class HebrewShapingTests extends AnyFreeSpec with Matchers:

  private val Dalet  = 0x05d3
  private val Shin   = 0x05e9
  private val Qamats = 0x05b8
  private val Dagesh = 0x05bc
  private val ShinDot = 0x05c1
  private val Meteg  = 0x05bd
  private val Sheva  = 0x05b0
  private val Holam  = 0x05b9

  // Shape with the identity glyph mapping, capturing the run the font's feature is handed.
  private def seen(cps: Int*): Seq[Int] =
    val got = ArrayBuffer.empty[Int]
    HebrewShaping.shape(cps.toArray, identity, run => { got ++= run; run })
    got.toSeq

  // Shape with a stand-in feature that combines one named pair wherever it appears, as a real font's does.
  private def combining(pair: (Int, Int), into: Int)(cps: Int*): Array[Int] =
    HebrewShaping.shape(
      cps.toArray,
      identity,
      run =>
        val i = run.indices.indexWhere(k => k + 1 < run.length && run(k) == pair._1 && run(k + 1) == pair._2)
        if i < 0 then run else run.patch(i, Array(into), 2),
    )

  "hasHebrew sees Hebrew and nothing else" in {
    HebrewShaping.hasHebrew("שלום") shouldBe true
    HebrewShaping.hasHebrew("דָּבָר") shouldBe true
    HebrewShaping.hasHebrew("hello") shouldBe false
    HebrewShaping.hasHebrew("مرحبا") shouldBe false
    HebrewShaping.hasHebrew("") shouldBe false
  }

  "a letter's own point is moved to its side, ahead of the vowels" in {
    // דָּ stores the qamats before the dagesh, canonical ordering ranking it lower, but the dagesh is drawn
    // inside the dalet and has to reach it for the font's rule to match.
    seen(Dalet, Qamats, Dagesh) shouldBe Seq(Dalet, Dagesh, Qamats)
    seen(Shin, Qamats, ShinDot) shouldBe Seq(Shin, ShinDot, Qamats)
    seen(Shin, Sheva, ShinDot) shouldBe Seq(Shin, ShinDot, Sheva)
  }

  "a point already beside its letter stays there" in {
    seen(Dalet, Dagesh, Qamats) shouldBe Seq(Dalet, Dagesh, Qamats)
  }

  "the meteg sorts after every vowel, and the dagesh before them" in {
    seen(Dalet, Meteg, Qamats, Dagesh) shouldBe Seq(Dalet, Dagesh, Qamats, Meteg)
    seen(Dalet, Meteg, Holam) shouldBe Seq(Dalet, Holam, Meteg)
  }

  "each letter's points are ordered within its own syllable" in {
    // Two letters, each carrying a point out of drawing order: neither reaches across the letter between them.
    seen(Dalet, Qamats, Dagesh, Shin, Qamats, ShinDot) shouldBe
      Seq(Dalet, Dagesh, Qamats, Shin, ShinDot, Qamats)
  }

  "a run the font combines nothing in stays on the plain path" in {
    // Reordering alone is not a reason to leave the plain path: the mark shaper places the points from their
    // anchors either way, and this is what the Noto Hebrew face does.
    HebrewShaping.shape(Array(Dalet, Qamats, Dagesh), identity, run => run) shouldBe null
    HebrewShaping.shape(Array(Dalet), identity, run => run) shouldBe null
  }

  "a combined pair gives the run to draw" in {
    combining((Dalet, Dagesh), 999)(Dalet, Qamats, Dagesh).toSeq shouldBe Seq(999, Qamats)
    combining((Shin, ShinDot), 998)(Shin, Qamats, ShinDot).toSeq shouldBe Seq(998, Qamats)
  }

  "marks of another script keep the order they were typed in" in {
    // Arabic harakat and Indic signs share this path when they meet a Hebrew run; ranking equal, they must be
    // left exactly as they came.
    seen(0x0628, 0x064e, 0x0651) shouldBe Seq(0x0628, 0x064e, 0x0651)
    seen(0x0915, 0x093f, 0x0902) shouldBe Seq(0x0915, 0x093f, 0x0902)
  }
