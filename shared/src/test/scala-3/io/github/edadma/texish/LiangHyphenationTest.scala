package io.github.edadma.texish

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

  // The Hyphenation object is a shared, append-only pattern cache with a pure lookup that takes the language
  // explicitly. There is no global "active language" — selection is per-document on the Typesetter — so these
  // tests never mutate state another suite can observe, and need no clear()/reset between them.

  "Hyphenation cache lookup" should "return None when no language is selected" in {
    Hyphenation(None, "test") shouldBe None
  }

  it should "return None for a language whose patterns are not loaded" in {
    Hyphenation(Some("no-such-lang"), "hello") shouldBe None
  }

  it should "hyphenate through patterns loaded under the default name" in {
    Hyphenation.setHyphenator(LiangHyphenation.fromPatterns("el1lo"))
    Hyphenation.isLoaded("default") shouldBe true
    Hyphenation(Some("default"), "hello") should not be None
  }

  it should "load patterns from a string under a named language" in {
    Hyphenation.loadPatternsFromString("strlang", "el1lo hy3ph")
    Hyphenation.isLoaded("strlang") shouldBe true
    Hyphenation(Some("strlang"), "hello") should not be None
  }

  it should "hold several languages at once and look each up by name" in {
    Hyphenation.loadPatternsFromString("en-test", "el1lo")
    Hyphenation.loadPatternsFromString("xx-test", "ol1la")
    Hyphenation.languages should contain allOf ("en-test", "xx-test")
    Hyphenation(Some("en-test"), "hello") should not be None
    Hyphenation(Some("xx-test"), "holla") should not be None
    // selecting by name keeps the languages independent: en-test's patterns do not break "holla"
    Hyphenation(Some("en-test"), "holla") shouldBe None
  }

  it should "load the build-time embedded en-US patterns via enableEmbedded" in {
    Hyphenation.enableEmbedded("en-us") shouldBe true
    // a famously long word the embedded patterns break in several places
    Hyphenation(Some("en-us"), "pneumonoultramicroscopicsilicovolcanoconiosis") should not be None
  }

  it should "bundle en-us, es, and fr patterns" in {
    Hyphenation.embeddedLanguages should contain allOf ("en-us", "es", "fr")
    Hyphenation.enableEmbedded("xx-nope") shouldBe false // an unbundled tag reports failure, not an error
  }

  it should "hyphenate Spanish words once the embedded es patterns are loaded" in {
    Hyphenation.enableEmbedded("es") shouldBe true
    // the bundled es patterns find at least one break in an ordinary multi-syllable Spanish word, and
    // every break splits the whole word (the prefix carries the trailing hyphen, as for English)
    val points = Hyphenation(Some("es"), "palabra").map(_.toList)
    points should not be None
    points.get should not be empty
    all(points.get.map((before, after) => before.stripSuffix("-") + after)) shouldBe "palabra"
  }

  it should "select a hyphenation language per document, not globally" in {
    Hyphenation.enableEmbedded("en-us")
    val withLang = new HeadlessTypesetter
    withLang.hyphenationLanguage = Some("en-us")
    val without = new HeadlessTypesetter
    // the document that selected a language hyphenates; a fresh document does not, no matter what any other
    // document or concurrently-running suite has loaded or selected
    Hyphenation(withLang.hyphenationLanguage, "pneumonoultramicroscopicsilicovolcanoconiosis") should not be None
    without.hyphenationLanguage shouldBe None
    Hyphenation(without.hyphenationLanguage, "pneumonoultramicroscopicsilicovolcanoconiosis") shouldBe None
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

  it should "ignore surrounding punctuation, so a short core does not hyphenate" in {
    // The bracket and period are not part of the word; once stripped the core "Rom"/"Cor" is too short to break.
    // Without stripping these broke as "(R-om." and "Cor-." against a scripture reference.
    englishHyphenator.hyphenate("(Rom.") shouldBe empty
    englishHyphenator.hyphenate("Cor.") shouldBe empty
    englishHyphenator.hyphenate("(15:3") shouldBe empty
  }

  it should "hyphenate the letters of a word wrapped in punctuation, offset to the original" in {
    // "computer" breaks at the same letters whether or not it carries a leading bracket and trailing period;
    // every point shifts right by the one stripped leading character.
    val bare = englishHyphenator.hyphenate("computer")
    bare should not be empty
    englishHyphenator.hyphenate("(computer).") shouldBe bare.map(_ + 1)
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
