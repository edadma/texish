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

  "breakableBetween offers no break between two Hangul syllables" in {
    // Korean breaks at its interword spaces, not between the syllables of a word.
    CJK.breakableBetween('한', '국') shouldBe false
    CJK.breakableBetween('안', '녕') shouldBe false
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

  "a Korean run is not broken per character, unlike a Chinese one" in {
    // At the same narrow measure a Chinese run breaks between its characters, but a Korean run — off the
    // per-character path — stays a single box (in a real document it would break at its interword spaces).
    val t  = loose
    val zh = KnuthPlass.breakParagraph(Seq(t.charBox("中文中文中文中文中文中文")), 30.0, t) // 12 chars
    val ko = KnuthPlass.breakParagraph(Seq(t.charBox("한국어한국어한국어한국어")), 30.0, t)  // 12 syllables
    zh shouldBe defined
    zh.get.length should be > 1 // Chinese breaks per character
    // The Korean run has no per-character breaks and no spaces here, so it is one atomic box the breaker
    // cannot fit at this measure — it finds no legal breakpoint at all. (A real Korean paragraph carries
    // spaces and breaks at them.) Had Hangul stayed a break-anywhere character it would break like Chinese.
    ko shouldBe empty
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
