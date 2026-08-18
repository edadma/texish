package io.github.edadma.texish

import io.github.edadma.path.Path

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The bundled hyphenation patterns: the whole `hyph-utf8` set as files in `hyphenation/`, five of them also
  * compiled into the artifact, and the dialect those files are written in.
  *
  * Every language here is a file somebody else wrote and texish reads, so what these tests pin is the *reading*
  * — that each of the 87 files yields the table it holds rather than a truncated or empty one, and that the four
  * things the files do beyond listing patterns (balanced groups, macros, `\input`, the minima in the header) are
  * all honoured. A file read wrongly does not fail loudly; it quietly breaks words in the wrong places, which is
  * why the reading is worth pinning language by language.
  */
class HyphenationCatalogueTests extends AnyFreeSpec with Matchers:

  /** The tags whose files are deliberately not shipped: their licences do not let texish pass them on freely,
    * or the file holds no patterns at all. `hyphenation/README.md` names each one and why. */
  private val NotShipped = Set("cs", "hu", "hy", "id", "lv", "mk", "sr-cyrl", "ro", "mn-cyrl-x-lmc", "grc-x-ibycus")

  /** The languages upstream ships an explicitly *empty* table for, saying that the language is not hyphenated —
    * Arabic and Hebrew are not broken at all, Persian and Vietnamese not by pattern. Naming one is meant to
    * succeed and produce no breaks, so they are the one exception to "a bundled language has patterns". */
  private val Unhyphenated = Set("ar", "fa", "he", "vi")

  private def read(tag: String): LiangHyphenation.PatternFile =
    val file = Hyphenation.patternFile(tag).getOrElse(fail(s"no pattern file for '$tag'"))

    LiangHyphenation.parse(file.readText(), LiangHyphenation.beside(file.toPlatformString))

  /** The word with a hyphen at each point the patterns of `lang` offer. */
  private def marked(lang: String, word: String): String =
    Hyphenation(Some(lang), word) match
      case None => word
      case Some(points) =>
        val at = points.toList.map((before, _) => before.length - 1).toSet
        word.zipWithIndex.map((c, i) => if at(i) then s"-$c" else c.toString).mkString

  private def enabled(tag: String): String =
    Hyphenation.enable(tag) shouldBe true
    tag

  "the tree holds every language hyph-utf8 has patterns for" in {
    Hyphenation.bundledLanguages should have size 78
    Hyphenation.bundledLanguages should contain allOf ("en-us", "en-gb", "de-1996", "ru", "hi", "kn", "gu", "mul-ethi")
    Hyphenation.embeddedLanguages shouldBe Set("en-us", "es", "fr", "it", "pt")
    Hyphenation.embeddedLanguages.subsetOf(Hyphenation.bundledLanguages) shouldBe true

    // A language left out for its licence must stay left out: it is not the engine's to redistribute, and a
    // file quietly copied back in would ship under a licence texish cannot offer.
    Hyphenation.bundledLanguages intersect NotShipped shouldBe empty
  }

  "every bundled language loads, and holds the table its file holds" in {
    for tag <- Hyphenation.bundledLanguages.toSeq.sorted do
      withClue(s"$tag: ") {
        Hyphenation.enable(tag) shouldBe true

        val source = read(tag)

        if Unhyphenated(tag) then source.patterns shouldBe empty
        else source.patterns.size should be > 20

        source.minLeft should be >= 1
        source.minRight should be >= 1
      }
  }

  "a language whose patterns are files hyphenates as its own typographers do" in {
    // The German compound is the canonical demonstration of German patterns, and the word is what the language
    // calls the thing being tested: syllable division.
    marked(enabled("de-1996"), "Silbentrennung") shouldBe "Sil-ben-tren-nung"
  }

  "the languages the Indic scripts are set in are among them" in {
    marked(enabled("hi"), "व्याकरण") should not be "व्याकरण"
    marked(enabled("kn"), "ಕನ್ನಡದಲ್ಲಿ") should not be "ಕನ್ನಡದಲ್ಲಿ"
  }

  "an unhyphenated language loads and offers nothing" in {
    Hyphenation.enable("ar") shouldBe true
    Hyphenation(Some("ar"), "الحروف") shouldBe None
  }

  "a language whose file is only an \\input and an exception list gets the patterns it inherits" in {
    // hyph-nb.tex is `\input hyph-no.tex` and a dozen words: read without following the input, Norwegian
    // Bokmål would have no patterns at all.
    val nb = read("nb")

    nb.patterns.size should be > 1000
    nb.patterns shouldBe read("no").patterns
    nb.exceptions.keySet should contain("attende")
    marked(enabled("nb"), "attende") shouldBe "at-ten-de"
  }

  "a language written with macros gets the patterns they generate" in {
    // Esperanto writes most of its table as \nom{…} and \ver{…} calls over stems. Reading it without expanding
    // them, and without balancing the braces those calls contain, stops at the first one.
    val eo = read("eo")

    eo.patterns.size should be > 1000
    eo.patterns.keySet should contain("esper")

    // \nom{1a2n} generates the patterns for the noun forms of the stem — `1a2no.`, `1a2noj.`, and through
    // \adj the adjective ones too. None of them is written out anywhere in the file.
    eo.patterns.keySet should contain allOf ("ano.", "anoj.", "anaj.")
  }

  "the exception list settles a word the patterns get wrong" in {
    // en-us's patterns offer "as-sociate"; its \hyphenation list spells the word out, and TeX prefers the list.
    marked(enabled("en-us"), "associate") shouldBe "as-so-ciate"
    marked("en-us", "reciprocity") shouldBe "reci-procity"
  }

  "an exception may forbid every break in a word" in {
    // "project" is in en-us's exception list with no hyphen in it, which is how TeX says "never break this".
    marked(enabled("en-us"), "project") shouldBe "project"
    marked("en-us", "present") shouldBe "present"
  }

  "the minima come from the language, not from a default" in {
    // English asks for three letters after a break, where texish's own default was two — so a language read
    // without its header offers breaks English typography does not allow.
    read("en-us").minLeft shouldBe 2
    read("en-us").minRight shouldBe 3

    // Where a file states minima for generating its patterns and for typesetting with them, the typesetting
    // pair is the one documents are meant to use — Swedish was generated at one letter before a break and is
    // set at two.
    read("sv").minLeft shouldBe 2
    read("sv").minRight shouldBe 2

    def withRight(n: Int) =
      LiangHyphenation.fromString(s"""% hyphenmins:
                                     |%     typesetting:
                                     |%         left: 2
                                     |%         right: $n
                                     |\\patterns{el1lo}""".stripMargin)

    withRight(2).hyphenate("hello") shouldBe IndexedSeq(2)
    withRight(3).hyphenate("hello") shouldBe IndexedSeq.empty
  }

  "a group runs to the brace that closes it, not to the first one" in {
    // Stopping at the first `}` loses every pattern after a brace inside the table — silently, and as too few
    // breaks rather than wrong ones, which is the hardest kind of defect to notice in a page of prose.
    val file = LiangHyphenation.parse(
      """\patterns{
        |ra1si
        |\somemacro{ignored}
        |el1lo
        |}""".stripMargin,
    )

    file.patterns should contain key "rasi"
    file.patterns should contain key "ello"
  }

  "a table with no TeX around it is read as the table it is" in {
    // `\loadhyphenation` of a file somebody wrote by hand, and every one-line pattern set in these tests.
    LiangHyphenation.parse("hy3ph 4tion ab2c").patterns should have size 3
  }

  "an \\input that cannot be resolved leaves the rest of the file readable" in {
    val file = LiangHyphenation.parse("""\input hyph-nowhere.tex
                                        |\hyphenation{
                                        |be-tre
                                        |}""".stripMargin)

    file.patterns shouldBe empty
    file.exceptions.keySet shouldBe Set("betre")
  }
