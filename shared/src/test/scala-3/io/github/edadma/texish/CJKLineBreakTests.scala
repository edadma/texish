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
    CJK.isCJK('a') shouldBe false
    CJK.isCJK(' ') shouldBe false
  }

  "CJK.hasCJK detects any CJK codepoint in a run" in {
    CJK.hasCJK("hello") shouldBe false
    CJK.hasCJK("中文") shouldBe true
    CJK.hasCJK("ABC中") shouldBe true
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
