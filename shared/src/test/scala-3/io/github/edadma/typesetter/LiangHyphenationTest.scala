package io.github.edadma.typesetter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LiangHyphenationTest extends AnyFlatSpec with Matchers:

  // Load English patterns for real-word tests
  // Use file path for cross-platform compatibility (getResourceAsStream doesn't work on Native)
  lazy val englishHyphenator: LiangHyphenation =
    LiangHyphenation.fromFile("shared/src/main/resources/hyph-en-us.tex")

  "parsePattern" should "parse simple patterns" in {
    LiangHyphenation.parsePattern("hy3ph") shouldBe ("hyph", IndexedSeq(0, 0, 3, 0, 0))
  }

  it should "parse patterns with leading digit" in {
    LiangHyphenation.parsePattern("1ba") shouldBe ("ba", IndexedSeq(1, 0, 0))
  }

  it should "parse patterns with trailing digit" in {
    LiangHyphenation.parsePattern("ab2") shouldBe ("ab", IndexedSeq(0, 0, 2))
  }

  it should "parse patterns with multiple digits" in {
    LiangHyphenation.parsePattern("a1b2c3") shouldBe ("abc", IndexedSeq(0, 1, 2, 3))
  }

  it should "parse patterns with dot markers" in {
    LiangHyphenation.parsePattern(".ab4") shouldBe (".ab", IndexedSeq(0, 0, 0, 4))
  }

  "parsePatterns" should "parse TeX format with \\patterns block" in {
    val content = """
      |\patterns{
      |hy3ph
      |4tion
      |}
      |""".stripMargin
    val patterns = LiangHyphenation.parsePatterns(content)
    patterns should contain key "hyph"
    patterns should contain key "tion"
  }

  it should "parse plain whitespace-separated patterns" in {
    val patterns = LiangHyphenation.parsePatterns("hy3ph 4tion ab2c")
    patterns.size shouldBe 3
  }

  it should "ignore comments" in {
    val patterns = LiangHyphenation.parsePatterns("hy3ph %comment\n4tion")
    patterns.size shouldBe 2
  }

  "hyphenate" should "find hyphenation points" in {
    val h = LiangHyphenation.fromPatterns("hy3ph", "he2n", "hen4a", "4na.", "1na")
    val points = h.hyphenate("hyphenation")
    points should not be empty
  }

  it should "return empty for short words" in {
    val h = LiangHyphenation.fromPatterns("hy3ph")
    h.hyphenate("hi") shouldBe empty
    h.hyphenate("the") shouldBe empty
  }

  it should "be case insensitive" in {
    val h = LiangHyphenation.fromPatterns("hy3ph")
    h.hyphenate("HYPHEN") shouldBe h.hyphenate("hyphen")
  }

  it should "respect minLeft constraint" in {
    val h = LiangHyphenation.fromPatterns("1a")
    // With minLeft=2, first break must be after at least 2 chars
    val points = h.hyphenate("aaa")
    points.forall(_ >= 1) shouldBe true // index 1 means break after 2 chars
  }

  "apply" should "return None when no hyphenation points" in {
    val h = LiangHyphenation.fromPatterns("xyz9")
    h("hello") shouldBe None
  }

  it should "return iterator of break pairs" in {
    val h = LiangHyphenation.fromPatterns("el1lo")
    h("hello") match
      case Some(iter) =>
        val pairs = iter.toList
        pairs should not be empty
        pairs.foreach { case (before, after) =>
          before should endWith("-")
          (before.init + after) shouldBe "hello"
        }
      case None => fail("Expected hyphenation points")
  }

  "Hyphenation wrapper" should "return None when no hyphenator set" in {
    Hyphenation.clear()
    Hyphenation("test") shouldBe None
  }

  it should "delegate to LiangHyphenation when set" in {
    Hyphenation.clear()
    val h = LiangHyphenation.fromPatterns("el1lo")
    Hyphenation.setHyphenator(h)
    Hyphenation.isEnabled shouldBe true
    Hyphenation("hello") should not be None
    Hyphenation.clear()
  }

  it should "load patterns from string" in {
    Hyphenation.clear()
    Hyphenation.loadPatternsFromString("el1lo hy3ph")
    Hyphenation.isEnabled shouldBe true
    Hyphenation.clear()
  }

  it should "support multiple languages" in {
    Hyphenation.clear()
    Hyphenation.loadPatternsFromString("en", "el1lo")
    Hyphenation.loadPatternsFromString("test", "es1ti")

    Hyphenation.languages shouldBe Set("en", "test")
    Hyphenation.getLanguage shouldBe Some("en") // first loaded becomes active

    Hyphenation.setLanguage("test")
    Hyphenation.getLanguage shouldBe Some("test")

    Hyphenation.clear()
  }

  it should "switch between languages" in {
    Hyphenation.clear()
    Hyphenation.loadPatternsFromString("lang1", "el1lo")
    Hyphenation.loadPatternsFromString("lang2", "ol1la")

    Hyphenation.setLanguage("lang1")
    Hyphenation("hello") should not be None

    Hyphenation.setLanguage("lang2")
    Hyphenation("holla") should not be None

    Hyphenation.clear()
  }

  // Real English word hyphenation tests
  // Format: word -> expected syllables (hyphenation points are between syllables)
  // Values verified against hyph-en-us.tex patterns
  val expectedHyphenations: List[(String, List[String])] = List(
    "algorithm"      -> List("al", "go", "rithm"),
    "computer"       -> List("com", "put", "er"),
    "programming"    -> List("pro", "gram", "ming"),
    "extraordinary"  -> List("ex", "tra", "or", "di", "nary"),
    "information"    -> List("in", "for", "ma", "tion"),
    "development"    -> List("de", "vel", "op", "ment"),
    "international"  -> List("in", "ter", "na", "tion", "al"),
    "organization"   -> List("or", "ga", "ni", "za", "tion"),
    "university"     -> List("uni", "ver", "si", "ty"),
    "communication"  -> List("com", "mu", "ni", "ca", "tion"),
    "understanding"  -> List("un", "der", "stand", "ing"),
    "representative" -> List("rep", "re", "sen", "ta", "tive"),
    "possibility"    -> List("pos", "si", "bil", "i", "ty"),
    "responsibility" -> List("re", "spon", "si", "bil", "i", "ty"),
    "mathematics"    -> List("math", "e", "mat", "ics"),
    "bibliography"   -> List("bib", "li", "og", "ra", "phy"),
  )

  def syllablesToBreakPoints(syllables: List[String]): IndexedSeq[Int] =
    syllables.init.scanLeft(-1)((pos, syl) => pos + syl.length).tail.toIndexedSeq

  "English hyphenation" should "correctly hyphenate words" in {
    expectedHyphenations.foreach { case (word, expectedSyllables) =>
      val expectedPoints = syllablesToBreakPoints(expectedSyllables)
      val actualPoints = englishHyphenator.hyphenate(word)
      withClue(s"'$word' (expected ${expectedSyllables.mkString("-")} -> points $expectedPoints): ") {
        actualPoints shouldBe expectedPoints
      }
    }
  }

  it should "not hyphenate short words" in {
    val shortWords = List("the", "and", "is", "to", "a", "in", "it", "of")
    shortWords.foreach { word =>
      withClue(s"'$word' should have no hyphenation points: ") {
        englishHyphenator.hyphenate(word) shouldBe empty
      }
    }
  }

  it should "produce valid break pairs that reconstruct the word" in {
    expectedHyphenations.foreach { case (word, _) =>
      englishHyphenator(word) match
        case Some(iter) =>
          iter.foreach { case (before, after) =>
            withClue(s"Break pair for '$word': ") {
              before should endWith("-")
              (before.stripSuffix("-") + after) shouldBe word
            }
          }
        case None => fail(s"Expected hyphenation points for '$word'")
    }
  }
