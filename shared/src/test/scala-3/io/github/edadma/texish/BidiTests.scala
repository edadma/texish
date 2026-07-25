package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The Unicode Bidirectional Algorithm core, exercised on strings alone — no font, no typesetter. Each
  * case states a logical string and the visual string its characters should reorder into. Glyph
  * mirroring of brackets (UAX #9 rule L4) is a draw-time concern and is not applied here, so the tests
  * use letters, digits and spaces rather than relying on a mirrored parenthesis.
  */
class BidiTests extends AnyFreeSpec with Matchers:

  // A few Hebrew letters, right-to-left, named for readability.
  private val Alef  = 'א' // א
  private val Bet   = 'ב' // ב
  private val Gimel = 'ג' // ג
  private val Shin  = 'ש' // ש
  private val Lamed = 'ל' // ל

  "classify assigns the expected bidi classes" in {
    Bidi.classify(Alef.toInt) shouldBe BidiClass.R
    Bidi.classify('a'.toInt) shouldBe BidiClass.L
    Bidi.classify('Z'.toInt) shouldBe BidiClass.L
    Bidi.classify('5'.toInt) shouldBe BidiClass.EN
    Bidi.classify(' '.toInt) shouldBe BidiClass.WS
    Bidi.classify(','.toInt) shouldBe BidiClass.CS
    Bidi.classify('('.toInt) shouldBe BidiClass.ON
    Bidi.classify('ـ'.toInt) shouldBe BidiClass.AL // Arabic tatweel, classified AL
    Bidi.classify('٠'.toInt) shouldBe BidiClass.AN // Arabic-Indic digit zero
    Bidi.classify('ְ'.toInt) shouldBe BidiClass.NSM // Hebrew point sheva
  }

  "Thai combining signs classify as non-spacing marks, and its spacing ones do not" in {
    // Thai is left to right and so is never reordered, but the classification is also what routes a run to
    // the mark-positioning path, where the marks are placed by the font's anchors rather than in sequence.
    Bidi.classify(0x0e34) shouldBe BidiClass.NSM // sara i, an above vowel
    Bidi.classify(0x0e39) shouldBe BidiClass.NSM // sara uu, a below vowel
    Bidi.classify(0x0e48) shouldBe BidiClass.NSM // mai ek, a tone mark
    Bidi.classify(0x0e4c) shouldBe BidiClass.NSM // thanthakhat
    Bidi.classify(0x0e01) should not be BidiClass.NSM // ko kai, a consonant
    Bidi.classify(0x0e32) should not be BidiClass.NSM // sara aa — spacing, not combining
    Bidi.hasMarks("ผู้") shouldBe true
    Bidi.hasMarks("กทย") shouldBe false
  }

  "autoBaseLevel follows the first strong character (P2/P3)" in {
    Bidi.autoBaseLevel(Bidi.classifyString("abc")) shouldBe 0
    Bidi.autoBaseLevel(Bidi.classifyString(s"$Alef$Bet$Gimel")) shouldBe 1
    Bidi.autoBaseLevel(Bidi.classifyString(s"  $Alef")) shouldBe 1  // leading spaces skipped
    Bidi.autoBaseLevel(Bidi.classifyString("  123")) shouldBe 0     // no strong char at all
    Bidi.autoBaseLevel(Bidi.classifyString(s"99 $Alef")) shouldBe 1 // digits are not strong
  }

  "a pure Hebrew run reverses into visual order" in {
    Bidi.visualString(s"$Alef$Bet$Gimel") shouldBe s"$Gimel$Bet$Alef"
  }

  "a pure Latin run is left unchanged" in {
    Bidi.visualString("hello world") shouldBe "hello world"
  }

  "Latin followed by Hebrew (LTR paragraph): Latin stays, Hebrew reverses to the right" in {
    Bidi.visualString(s"abc $Alef$Bet$Gimel") shouldBe s"abc $Gimel$Bet$Alef"
  }

  "Hebrew followed by Latin (RTL paragraph): Latin sits left, Hebrew reads right" in {
    Bidi.visualString(s"$Alef$Bet$Gimel abc") shouldBe s"abc $Gimel$Bet$Alef"
  }

  "digits embedded in Hebrew keep their left-to-right order" in {
    // logical ש 1 2 3 ל  →  visual ל 1 2 3 ש : the RTL frame reverses but the number does not.
    Bidi.visualString(s"${Shin}123$Lamed") shouldBe s"${Lamed}123$Shin"
  }

  "a forced base level overrides automatic detection" in {
    // Pure Latin under a forced RTL base still reads left-to-right within the word, but the whole run
    // is a single level-2 island reversed once by the level-1 frame — i.e. it stays in order here
    // because there is only one word and nothing to its side.
    Bidi.visualString("abc", forcedBase = Some(1)) shouldBe "abc"
    // Forcing LTR on Hebrew still reverses the Hebrew (it is strong R regardless of base).
    Bidi.visualString(s"$Alef$Bet$Gimel", forcedBase = Some(0)) shouldBe s"$Gimel$Bet$Alef"
  }

  "the visual order is always a permutation of the logical indices" in {
    val s     = s"abc $Alef$Bet 12 $Gimel def"
    val order = Bidi.visualOrder(s)
    order.sorted.toSeq shouldBe (0 until s.length).toSeq
    Bidi.visualString(s).length shouldBe s.length
  }

  "an empty string and a single character are handled" in {
    Bidi.visualString("") shouldBe ""
    Bidi.visualString("x") shouldBe "x"
    Bidi.visualString(s"$Alef") shouldBe s"$Alef"
  }

  "reorderByLevels reverses a right-to-left run and nests an embedded number" in {
    // levels for ש 1 2 ל under base 1: [1,2,2,1] — the digits are an even island inside the RTL run.
    val order = Bidi.reorderByLevels(Array(1, 2, 2, 1))
    order.toSeq shouldBe Seq(3, 1, 2, 0) // RTL frame reverses; the 1,2 island keeps its order
  }

  "reorderByLevels leaves an all-even (left-to-right) line untouched" in {
    Bidi.reorderByLevels(Array(0, 0, 0)).toSeq shouldBe Seq(0, 1, 2)
    Bidi.reorderByLevels(Array(0, 2, 2, 0)).toSeq shouldBe Seq(0, 1, 2, 3)
  }

  "neutral punctuation between two Hebrew words takes the Hebrew direction" in {
    // ש ל , ש ל  (comma between two RTL words) reverses wholesale; the comma stays between them.
    Bidi.visualString(s"$Shin$Lamed,$Shin$Lamed") shouldBe s"$Lamed$Shin,$Lamed$Shin"
  }
