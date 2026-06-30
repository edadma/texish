package io.github.edadma.texish.opentype

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Font-free tests for the Arabic joining algorithm: the joining-type table against known characters, and
  * the contextual-form resolution against words whose correct shaping is well known. Forms are computed in
  * memory order (the order one types the letters), independent of the right-to-left visual order the bidi
  * layer later produces.
  */
class ArabicShapingTests extends AnyFreeSpec with Matchers:

  import JoiningForm.*

  // Manual codepoint extraction — String.codePoints() (an IntStream) is unsupported on Scala.js.
  private def codePoints(s: String): Array[Int] =
    val buf = scala.collection.mutable.ArrayBuffer.empty[Int]
    var i   = 0
    while i < s.length do
      val cp = s.codePointAt(i)
      buf += cp
      i += Character.charCount(cp)
    buf.toArray

  private def forms(s: String): Array[JoiningForm] =
    ArabicShaping.resolveForms(codePoints(s))

  "joining types" - {
    "dual-joining letters" in {
      for cp <- Seq(0x0628, 0x062A, 0x0644, 0x0645, 0x0646, 0x0647, 0x064A, // beh teh lam meem noon heh yeh
                    0x067E, 0x06AF, 0x06CC) // Persian peh, gaf, farsi yeh
      do ArabicShaping.joiningType(cp) shouldBe 'D'
    }
    "right-joining letters do not connect to the following letter" in {
      for cp <- Seq(0x0627, 0x0622, 0x0623, 0x0625, // alef and its hamza/madda forms
                    0x062F, 0x0630, 0x0631, 0x0632, 0x0648, 0x0629) // dal thal reh zain waw teh-marbuta
      do ArabicShaping.joiningType(cp) shouldBe 'R'
    }
    "marks are transparent" in {
      for cp <- Seq(0x064B, 0x064E, 0x064F, 0x0650, 0x0651, 0x0652, 0x0670) // tanwin/harakat/shadda/sukun/superscript alef
      do ArabicShaping.joiningType(cp) shouldBe 'T'
    }
    "tatweel and ZWJ cause joining; hamza and ZWNJ do not join" in {
      ArabicShaping.joiningType(0x0640) shouldBe 'C' // tatweel
      ArabicShaping.joiningType(0x200D) shouldBe 'C' // ZWJ
      ArabicShaping.joiningType(0x0621) shouldBe 'U' // hamza (free-standing)
      ArabicShaping.joiningType(0x200C) shouldBe 'U' // ZWNJ
    }
    "non-Arabic characters are non-joining" in {
      for cp <- Seq('A'.toInt, '0'.toInt, ' '.toInt, 0x05D0 /* Hebrew alef */, 0x4E00 /* CJK */ )
      do ArabicShaping.joiningType(cp) shouldBe 'U'
    }
  }

  "hasArabic" - {
    "is true for text with a joining letter" in {
      ArabicShaping.hasArabic("ب") shouldBe true
      ArabicShaping.hasArabic("hello السلام") shouldBe true
    }
    "is false for Latin, digits, and bare marks" in {
      ArabicShaping.hasArabic("hello world") shouldBe false
      ArabicShaping.hasArabic("12345") shouldBe false
      ArabicShaping.hasArabic("َّ") shouldBe false // only harakat, no base letter
    }
  }

  "form resolution" - {
    "a lone letter is isolated" in {
      forms("ب") shouldBe Array(Isolated) // beh
    }

    "two dual-joining letters: initial then final" in {
      // بب  beh + beh
      forms("بب") shouldBe Array(Initial, Final)
    }

    "three dual-joining letters: initial, medial, final" in {
      // ببب
      forms("ببب") shouldBe Array(Initial, Medial, Final)
    }

    "a right-joining letter ends the connection to its right" in {
      // بد  beh + dal: beh cannot connect forward to dal? dal is right-joining (joins trailing),
      // so beh (dual) joins to dal → beh initial, dal final.
      forms("بد") shouldBe Array(Initial, Final)
    }

    "a right-joining letter does not connect to the following letter" in {
      // دب  dal + beh: dal is right-joining (no leading join), so it cannot connect to beh.
      // dal isolated, beh isolated.
      forms("دب") shouldBe Array(Isolated, Isolated)
    }

    "alef after a dual letter takes final, and breaks the chain" in {
      // لاب  lam + alef + beh: lam joins to alef (lam initial, alef final); alef is right-joining so
      // it does not connect forward to beh, which stands isolated.
      forms("لاب") shouldBe Array(Initial, Final, Isolated)
    }

    "the word سلام (salaam)" in {
      // seen(D) lam(D) alef(R) meem(D)
      // seen initial, lam medial, alef final, meem isolated (alef breaks forward chain)
      forms("سلام") shouldBe Array(Initial, Medial, Final, Isolated)
    }

    "the word كتب (kataba) all dual" in {
      // kaf teh beh
      forms("كتب") shouldBe Array(Initial, Medial, Final)
    }

    "a harakat between two letters does not break the join" in {
      // بَب  beh + fatha + beh: the mark is transparent; beh still joins beh.
      forms("بَب") shouldBe Array(Initial, Isolated, Final)
    }

    "tatweel keeps a medial connection" in {
      // بـب  beh + tatweel + beh: tatweel is join-causing, all three connect.
      forms("بـب") shouldBe Array(Initial, Medial, Final)
    }

    "a non-joining gap isolates both sides" in {
      // ب ب  beh + space + beh
      forms("ب ب") shouldBe Array(Isolated, Isolated, Isolated)
    }
  }
