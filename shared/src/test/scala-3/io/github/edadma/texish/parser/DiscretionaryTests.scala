package io.github.edadma.texish.parser

import io.github.edadma.texish.{
  Box,
  Builder,
  CharBox,
  DiscretionaryBox,
  Glue,
  HeadlessTypesetter,
  KnuthPlass,
}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** A discretionary is the author's hook into the line breaker. Breaking at it ends the line with its `pre`
  * material and opens the next with `post`; leaving it unbroken shows `noBreak`. \discretionary takes the three
  * parts explicitly and \softhyphen is the common \discretionary{-}{}{} (a soft hyphen). Each test character is
  * six units wide (HeadlessTypesetter), so widths are exact.
  */
class DiscretionaryTests extends AnyFreeSpec with Matchers:

  private val t = new HeadlessTypesetter
  private def ch(s: String): CharBox = new CharBox(t, s)
  // The breaker itself accounts for \parfillskip on the final line (ParagraphMode adds the real glue when
  // setting it), so a short last line is always fine and no stand-in fill is needed.

  private def texts(box: Box): List[String] = box match
    case c: CharBox => List(c.text)
    case _          => Nil
  private def lineTexts(line: Seq[Box]): List[String] = line.flatMap(texts).toList

  "the breaker breaks at a discretionary, ending the line with its pre material" in {
    // "aaa" (18) + disc + "bbbb" (24) at measure 24: must break, and the only legal break is the disc.
    val boxes = Seq(ch("aaa"), new DiscretionaryBox(Seq(ch("-")), Nil, Nil), ch("bbbb"))
    val Some(lines) = KnuthPlass.breakParagraph(boxes, 24, t): @unchecked

    lines.length shouldBe 2
    lineTexts(lines(0)) shouldBe List("aaa", "-") // pre (the hyphen) ends the first line
    lineTexts(lines(1)) shouldBe List("bbbb")     // and no hyphen leaks onto the next line
  }

  "an unbroken discretionary shows its no-break material" in {
    // everything fits on one line, so the disc stays unbroken and its noBreak glyph "X" shows in place
    val boxes = Seq(ch("aa"), new DiscretionaryBox(Seq(ch("-")), Nil, Seq(ch("X"))), ch("bb"))
    val Some(lines) = KnuthPlass.breakParagraph(boxes, 100, t): @unchecked

    lines.length shouldBe 1
    lineTexts(lines(0)) shouldBe List("aa", "X", "bb") // no hyphen; the no-break form is used
  }

  "breaking at a discretionary opens the next line with its post material" in {
    // a respelling discretionary: break → "aaa-" on line one, "Z" begins line two (post). The post
    // material's width counts against line two — "Z"+"bbb" is exactly the 24-unit measure.
    val boxes = Seq(ch("aaa"), new DiscretionaryBox(Seq(ch("-")), Seq(ch("Z")), Nil), ch("bbb"))
    val Some(lines) = KnuthPlass.breakParagraph(boxes, 24, t): @unchecked

    lines.length shouldBe 2
    lineTexts(lines(0)) shouldBe List("aaa", "-")
    lineTexts(lines(1)) shouldBe List("Z", "bbb") // post leads the next line, before the remaining text
  }

  // ---- primitive wiring (full processor stack) ----

  private def fixture(): (HeadlessTypesetter, Processor) =
    val ht      = new HeadlessTypesetter
    val handler = new TypesetterHandler(ht)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    ht.document = new io.github.edadma.texish.DocumentMode(ht)
    (ht, proc)

  private def discsIn(ht: HeadlessTypesetter): List[DiscretionaryBox] =
    ht.mode.asInstanceOf[Builder].list.collect { case d: DiscretionaryBox => d }

  "\\discretionary adds a discretionary box with the three parts typeset" in {
    val (ht, proc) = fixture()
    proc.process("aaa\\discretionary{-}{}{}bbbb")

    val discs = discsIn(ht)
    discs.length shouldBe 1
    discs.head.pre.flatMap(texts) shouldBe List("-")
    discs.head.post shouldBe empty
    discs.head.noBreak shouldBe empty
  }

  "\\softhyphen adds a discretionary hyphen" in {
    val (ht, proc) = fixture()
    proc.process("aaa\\softhyphen bbbb")

    val discs = discsIn(ht)
    discs.length shouldBe 1
    discs.head.pre.flatMap(texts) shouldBe List("-") // hyphen in pre
    discs.head.post shouldBe empty
    discs.head.noBreak shouldBe empty
  }
