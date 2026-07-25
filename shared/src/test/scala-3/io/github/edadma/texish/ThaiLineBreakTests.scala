package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Thai carries no spaces between words, so a run of it would reach the line breaker as one box and overflow
  * the measure. Breaks are offered between orthographic clusters, as a last resort at a heavy penalty — Thai
  * properly breaks at word boundaries, which needs a dictionary texish does not have, so the phrase spaces
  * Thai does use are strongly preferred and a cluster break is taken only where nothing else fits.
  *
  * What the cluster rule must guarantee is that no break falls somewhere visibly wrong: a vowel or tone mark
  * is never parted from its consonant, and a pre-posed vowel never stranded from the consonant it precedes.
  */
class ThaiLineBreakTests extends AnyFreeSpec with Matchers:

  private def lineText(line: Seq[Box]): String =
    line.collect { case cb: CharBox => cb.text }.mkString

  // Characters used below, named so the assertions read without knowing the block by heart.
  private val ko      = 0x0e01 // ก, a consonant
  private val tho     = 0x0e17 // ท
  private val yo      = 0x0e22 // ย
  private val saraI   = 0x0e34 // ิ, an above vowel (combining)
  private val saraU   = 0x0e39 // ู, a below vowel (combining)
  private val maiEk   = 0x0e48 // ่, a tone mark (combining)
  private val saraE   = 0x0e40 // เ, a pre-posed vowel
  private val saraAi  = 0x0e44 // ไ, a pre-posed vowel
  private val saraAa  = 0x0e32 // า, a spacing vowel that follows its base
  private val saraAm  = 0x0e33 // ำ
  private val maiyamok = 0x0e46 // ๆ, the repetition mark

  "isThai recognizes the block and nothing outside it" in {
    Thai.isThai(ko) shouldBe true
    Thai.isThai(saraU) shouldBe true
    Thai.isThai('a'.toInt) shouldBe false
    Thai.isThai(0x0e00) shouldBe true  // block start
    Thai.isThai(0x0e80) shouldBe false // just past the block — Lao
  }

  "has detects Thai in a run" in {
    Thai.has("ภาษาไทย") shouldBe true
    Thai.has("hello") shouldBe false
    Thai.has("Thai ไทย mixed") shouldBe true
  }

  "a combining mark may not begin a line" in {
    Thai.noBreakBefore(saraI) shouldBe true  // above vowel
    Thai.noBreakBefore(saraU) shouldBe true  // below vowel
    Thai.noBreakBefore(maiEk) shouldBe true  // tone mark
    Thai.noBreakBefore(ko) shouldBe false    // a consonant may
  }

  "a spacing vowel that belongs to the base before it may not begin a line either" in {
    Thai.noBreakBefore(saraAa) shouldBe true
    Thai.noBreakBefore(saraAm) shouldBe true
    Thai.noBreakBefore(maiyamok) shouldBe true
  }

  "a pre-posed vowel may not end a line, since its consonant follows it" in {
    Thai.noBreakAfter(saraE) shouldBe true
    Thai.noBreakAfter(saraAi) shouldBe true
    Thai.noBreakAfter(ko) shouldBe false
  }

  "breakableBetween keeps a cluster whole and allows a break between clusters" in {
    Thai.breakableBetween(ko, tho) shouldBe true      // consonant | consonant
    Thai.breakableBetween(ko, saraI) shouldBe false   // never before a mark
    Thai.breakableBetween(ko, saraU) shouldBe false
    Thai.breakableBetween(saraE, ko) shouldBe false   // never after a pre-posed vowel
    Thai.breakableBetween(ko, 'a'.toInt) shouldBe false // not a Thai-internal boundary
  }

  private def loose: HeadlessTypesetter =
    val t = new HeadlessTypesetter
    t.set("tolerance", 10000.0)
    t

  "a Thai run breaks instead of overflowing the measure" in {
    // Sixteen consonants, no spaces: without cluster breaks this is one atomic box that runs off the page.
    val t    = new HeadlessTypesetter
    val text = "กททยกททยกททยกททย"
    val r    = KnuthPlass.breakParagraph(Seq(t.charBox(text)), 31.0, t)
    r shouldBe defined
    r.get.length should be > 1
    r.get.map(lineText).mkString shouldBe text          // every character survives, in order
    r.get.foreach(line => lineText(line).length should be <= 5) // and no line runs past the measure
  }

  "a break never parts a mark from its consonant" in {
    // Each consonant carries a below vowel, so the only legal breaks are between the pairs. If a break fell
    // before a mark, a line would begin with one.
    val t    = loose
    val text = "กูทูยูกูทูยูกูทูยู"
    val r    = KnuthPlass.breakParagraph(Seq(t.charBox(text)), 30.0, t)
    r shouldBe defined
    r.get.length should be > 1
    r.get.foreach(line => Thai.noBreakBefore(lineText(line).head.toInt) shouldBe false)
    r.get.map(lineText).mkString shouldBe text
  }

  "a break never strands a pre-posed vowel at the end of a line" in {
    val t    = loose
    val text = "เกเทเยเกเทเยเกเทเย"
    val r    = KnuthPlass.breakParagraph(Seq(t.charBox(text)), 30.0, t)
    r shouldBe defined
    r.get.length should be > 1
    r.get.foreach(line => Thai.noBreakAfter(lineText(line).last.toInt) shouldBe false)
    r.get.map(lineText).mkString shouldBe text
  }

  "a Thai run that fits is not broken at all" in {
    val t = loose
    val r = KnuthPlass.breakParagraph(Seq(t.charBox("กททย")), 200.0, t)
    r shouldBe defined
    r.get.length shouldBe 1
  }
