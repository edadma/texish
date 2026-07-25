package io.github.edadma.texish.opentype

import scala.collection.mutable.ArrayBuffer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Ordering the marks of an Arabic syllable for drawing ([[ArabicShaping.orderMarks]]).
  *
  * A letter carrying both a shadda and a vowel is typed with the vowel first — that is canonical order, since
  * the vowel's combining class is the lower — but drawn the other way round, the shadda on the letter and the
  * vowel above it. These pin the order the marks come out in, which every expectation below was confirmed
  * against with `hb-shape`.
  */
class ArabicMarkOrderTests extends AnyFreeSpec with Matchers:

  private val Fatha    = 0x064e
  private val Kasra    = 0x0650
  private val Shadda   = 0x0651
  private val Sukun    = 0x0652
  private val Maddah   = 0x0653 // above, but not a modifier combining mark
  private val HamzaAbv = 0x0654
  private val HamzaBlw = 0x0655
  private val Beh      = 0x0628

  // Order the codepoints of one syllable, the way the engine drives orderMarks from its reorder units.
  private def order(cps: Int*): Seq[Int] =
    val unit = ArrayBuffer.range(0, cps.length)
    ArabicShaping.orderMarks(unit, i => cps(i))
    unit.toSeq.map(cps)

  "a shadda is drawn under the vowel it is typed after" in {
    // The bug this fixes: typed base, fatha, shadda — drawn base, shadda, fatha, so the shadda sits on the
    // letter and the fatha above it. Without the reorder the two stack the wrong way up and a font's
    // shadda-plus-vowel ligature never matches.
    order(Beh, Fatha, Shadda) shouldBe Seq(Beh, Shadda, Fatha)
    order(Beh, Kasra, Shadda) shouldBe Seq(Beh, Shadda, Kasra)
    order(Beh, Sukun, Shadda) shouldBe Seq(Beh, Shadda, Sukun)
  }

  "a shadda already next to its letter stays put" in {
    // Text typed in the drawing order is left alone, so both ways of typing the syllable converge.
    order(Beh, Shadda, Fatha) shouldBe Seq(Beh, Shadda, Fatha)
  }

  "the modifier combining marks are drawn nearest the letter, below before above" in {
    // Hamza and the Qur'anic reading marks belong to the letter more closely than a vowel does: those below
    // come first, then those above, then the shadda, then everything else.
    order(Beh, Fatha, Shadda, HamzaAbv) shouldBe Seq(Beh, HamzaAbv, Shadda, Fatha)
    order(Beh, Fatha, Shadda, HamzaBlw) shouldBe Seq(Beh, HamzaBlw, Shadda, Fatha)
    order(Beh, Fatha, HamzaAbv, HamzaBlw) shouldBe Seq(Beh, HamzaBlw, HamzaAbv, Fatha)
    order(Beh, Fatha, Shadda, HamzaAbv, HamzaBlw) shouldBe Seq(Beh, HamzaBlw, HamzaAbv, Shadda, Fatha)
    order(Beh, 0x06e3, 0x0658) shouldBe Seq(Beh, 0x06e3, 0x0658) // small low seen, then noon ghunna
    order(Beh, 0x0658, 0x06e3) shouldBe Seq(Beh, 0x06e3, 0x0658) // and the same typed the other way
  }

  "a mark the algorithm has no opinion about keeps the order it was typed in" in {
    // Only the modifier marks and the shadda move; a maddah is drawn above like a vowel and is left where
    // canonical ordering already put it, so the sort must be stable rather than merely correct at the front.
    order(Beh, Fatha, Maddah) shouldBe Seq(Beh, Fatha, Maddah)
    order(Beh, Maddah, Fatha) shouldBe Seq(Beh, Maddah, Fatha)
    order(Beh, Fatha, Maddah, Shadda) shouldBe Seq(Beh, Shadda, Fatha, Maddah)
  }

  "a syllable with nothing to reorder is returned untouched" in {
    order(Beh) shouldBe Seq(Beh)
    order(Beh, Fatha) shouldBe Seq(Beh, Fatha)
    order(Beh, Shadda) shouldBe Seq(Beh, Shadda) // a lone mark never moves ahead of its base
    order(Beh, Fatha, Sukun) shouldBe Seq(Beh, Fatha, Sukun)
  }

  "the base of a syllable is never moved" in {
    // The shadda outranks every vowel but is still a mark: it may never overtake the letter it sits on.
    order(Shadda, Fatha).head shouldBe Shadda
    order(Beh, Shadda, HamzaBlw).head shouldBe Beh
  }

  "marks of another script are left exactly as they were" in {
    // Hebrew points and Devanagari signs share the reorder units this runs over, so the rule must be inert
    // outside Arabic: a sheva and a dagesh keep their typed order, whichever way round they come.
    order(0x05d1, 0x05b0, 0x05bc) shouldBe Seq(0x05d1, 0x05b0, 0x05bc)
    order(0x05d1, 0x05bc, 0x05b0) shouldBe Seq(0x05d1, 0x05bc, 0x05b0)
    order(0x0915, 0x093f, 0x0902) shouldBe Seq(0x0915, 0x093f, 0x0902)
  }
