package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** CJK text carries no interword spaces, so without help it reaches the line breaker as one box and overflows
  * the measure. The paragraph builder offers a break between adjacent CJK characters — as stretchable glue, so
  * a line of characters with no space of their own can still justify — and applies kinsoku: a closing mark
  * never starts a line, an opening mark never ends one. These tests pin both the per-character classification
  * and the breaking behaviour, using the metrics-only HeadlessTypesetter (6 units per character).
  */
class CJKLineBreakTests extends AnyFreeSpec with Matchers:

  // The characters returned, in order, for a single broken line — glue and other boxes dropped.
  private def lineText(line: Seq[Box]): String =
    line.collect { case cb: CharBox => cb.text }.mkString

  "CJK.isCJK recognizes ideographs and CJK punctuation but not Latin" in {
    CJK.isCJK('中') shouldBe true
    CJK.isCJK('。') shouldBe true
    CJK.isCJK('あ') shouldBe true // hiragana
    CJK.isCJK('カ') shouldBe true // katakana
    CJK.isCJK('日') shouldBe true // a kanji
    CJK.isCJK('a') shouldBe false
    CJK.isCJK(' ') shouldBe false
  }

  "CJK.isCJK excludes Hangul, so Korean is not on the per-character break path" in {
    // Korean is written with interword spaces and breaks at them like Latin; breaking between arbitrary
    // syllables would split words. So Hangul is deliberately not a break-anywhere character.
    CJK.isCJK('한') shouldBe false
    CJK.isCJK('국') shouldBe false
    CJK.isCJK('가') shouldBe false
  }

  "CJK.hasCJK detects any CJK codepoint in a run, but a pure-Korean run is not CJK" in {
    CJK.hasCJK("hello") shouldBe false
    CJK.hasCJK("中文") shouldBe true
    CJK.hasCJK("ABC中") shouldBe true
    CJK.hasCJK("日本語") shouldBe true  // Japanese kanji still take the CJK path
    CJK.hasCJK("한국어") shouldBe false // Korean does not — it breaks at its spaces
  }

  "breakableBetween offers a break between two ideographs" in {
    CJK.breakableBetween('中', '文') shouldBe true
  }

  "breakableBetween forbids a break before a closing mark (no stranded 。)" in {
    CJK.breakableBetween('试', '。') shouldBe false
  }

  "breakableBetween forbids a break after an opening mark (no dangling 「)" in {
    CJK.breakableBetween('「', '中') shouldBe false
  }

  "breakableBetween leaves two Latin letters to space-only breaking" in {
    CJK.breakableBetween('a', 'b') shouldBe false
  }

  "breakableBetween offers no free break between two Hangul syllables" in {
    // Korean breaks at its interword spaces, not freely between the syllables of a word.
    CJK.breakableBetween('한', '국') shouldBe false
    CJK.breakableBetween('안', '녕') shouldBe false
  }

  "lastResortBetween offers a costly break inside a Korean word, and nowhere else" in {
    CJK.lastResortBetween('한', '국') shouldBe true
    CJK.lastResortBetween('안', '녕') shouldBe true
    CJK.lastResortBetween('中', '文') shouldBe false // Chinese already breaks freely
    CJK.lastResortBetween('한', 'a') shouldBe false  // a script boundary, not inside a Korean word
    CJK.lastResortBetween('a', 'b') shouldBe false
  }

  "needsCharacterBreaks routes Korean into the per-character pass, hasCJK still does not" in {
    CJK.needsCharacterBreaks("한국어") shouldBe true // Korean needs the pass (for its last-resort breaks)
    CJK.hasCJK("한국어") shouldBe false              // but it is not freely-breaking CJK
    CJK.needsCharacterBreaks("hello") shouldBe false
    CJK.needsCharacterBreaks("中文") shouldBe true
  }

  "breakableBetween still offers a break between two Japanese characters" in {
    CJK.breakableBetween('日', '本') shouldBe true // kanji
    CJK.breakableBetween('あ', 'い') shouldBe true // hiragana
  }

  "breakableBetween allows a break at a CJK–Latin boundary" in {
    CJK.breakableBetween('中', 'a') shouldBe true
    CJK.breakableBetween('a', '中') shouldBe true
  }

  // The inter-character glue carries only a little stretch, so these correctness checks raise \tolerance to
  // keep the breaker from deferring to the greedy fallback at the artificially narrow test measures; the
  // kinsoku rules being checked do not depend on how loosely a line is justified.
  private def loose: HeadlessTypesetter =
    val t = new HeadlessTypesetter
    t.set("tolerance", 10000.0)
    t

  "a CJK run that exceeds the measure breaks into several lines instead of overflowing" in {
    val t     = loose
    val boxes = Seq(t.charBox("中文中文中文中文中文中文")) // 12 chars × 6 = 72 units
    val result = KnuthPlass.breakParagraph(boxes, 30.0, t)
    result shouldBe defined
    result.get.length should be > 1
  }

  "an unspaced Korean run breaks as a last resort rather than overflowing" in {
    // A Korean run with no space in it cannot break at a space, so the breaker falls back on the costly
    // in-word breaks; without them the run would be one atomic box that runs off the page.
    //
    // Deliberately unforgiving: the ordinary tolerance rather than `loose`, so this exercises what a real
    // document does, and a measure of 31 against a 6-unit syllable, so no line can come out to an exact fit
    // and the breaking cannot be a coincidence of the numbers.
    val t  = new HeadlessTypesetter
    val ko = KnuthPlass.breakParagraph(Seq(t.charBox("한국어한국어한국어한국어")), 31.0, t) // 12 syllables
    ko shouldBe defined
    ko.get.length should be > 1
    ko.get.map(lineText).mkString shouldBe "한국어한국어한국어한국어" // every syllable survives, in order
    // The point of the exercise: no line runs past the measure. Six syllables would be 36 units, over 31.
    ko.get.foreach(line => lineText(line).length should be <= 5)
  }

  "a Korean run that fits is not broken at all" in {
    val t = loose
    val r = KnuthPlass.breakParagraph(Seq(t.charBox("한국어조판")), 200.0, t)
    r shouldBe defined
    r.get.length shouldBe 1
  }

  "the breaker splits at a space rather than inside a Korean word when it can" in {
    // Two whole words with glue between them, at a measure that fits one word per line. The in-word breaks
    // are legal here too, but far more expensive, so the space is chosen and neither word is split.
    val t     = loose
    val boxes = Seq(t.charBox("한국어"), Glue(6, 3, 2), t.charBox("조판법"))
    val r     = KnuthPlass.breakParagraph(boxes, 20.0, t) // each word is 3 chars x 6 = 18 units
    r shouldBe defined
    r.get.map(lineText) shouldBe Seq("한국어", "조판법")
  }

  "kinsoku keeps a closing 。 off the start of every line" in {
    val t      = loose
    val boxes  = Seq(t.charBox("中。" * 20))
    val result = KnuthPlass.breakParagraph(boxes, 40.0, t)
    result shouldBe defined
    result.get.length should be > 1
    result.get.foreach { line => lineText(line) should not startWith "。" }
  }

  "kinsoku keeps an opening 「 off the end of every line" in {
    val t      = loose
    val boxes  = Seq(t.charBox("「中" * 20))
    val result = KnuthPlass.breakParagraph(boxes, 40.0, t)
    result shouldBe defined
    result.get.length should be > 1
    result.get.foreach { line => lineText(line) should not endWith "「" }
  }

  // Every character must survive the split — the concatenation of all lines is the original run.
  "the broken lines preserve all characters in order" in {
    val t      = loose
    val text   = "中文测试一二三四五六七八九十"
    val result = KnuthPlass.breakParagraph(Seq(t.charBox(text)), 36.0, t)
    result shouldBe defined
    result.get.map(lineText).mkString shouldBe text
  }
