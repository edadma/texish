package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Reading a TeX hyphenation pattern file, and the words the bundled patterns break.
  *
  * A TeX comment runs from its `%` to the end of the line. The reader dropped only the whitespace-separated
  * tokens that *began* with one, so the prose of a comment was read as patterns and its braces closed the
  * pattern block early. The English file is almost uncommented and came out right; every other bundled
  * language did not, and broke words where no break belongs — `fr-ançais`, `pro-phète` as `pr-o-phète`.
  */
class HyphenationPatternFileTests extends AnyFreeSpec with Matchers:

  /** The word with a hyphen at each point the patterns of `lang` offer. */
  private def marked(lang: String, word: String): String =
    Hyphenation(Some(lang), word) match
      case None => word
      case Some(points) =>
        val at = points.toList.map((before, _) => before.length - 1).toSet
        word.zipWithIndex.map((c, i) => if at(i) then s"-$c" else c.toString).mkString

  private def embedded(lang: String): String =
    Hyphenation.enableEmbedded(lang) shouldBe true
    lang

  /** Register a pattern set written out here, under a name of its own, and give that name back. */
  private def loaded(name: String, patterns: String): String =
    Hyphenation.loadPatternsFromString(name, patterns)
    name

  "a comment is read to the end of its line, not to the next space" in {
    // `b1r` sits inside the comment and must never be used: were it read, `abrasive` would take a second
    // break and set `ab-ra-sive`. Only the real pattern below counts.
    val lang = loaded(
      "comment-trap",
      """\patterns{
        |% phonetic patterns b1r % etymological patterns
        |ra1si
        |}""".stripMargin,
    )
    marked(lang, "abrasive") shouldBe "abra-sive"
  }

  "a brace inside a comment does not end the pattern block" in {
    // Every pattern after such a comment was lost, which is how a file could yield too few breaks rather
    // than wrong ones.
    val lang = loaded(
      "brace-trap",
      """\patterns{
        |% a note with a brace } in it
        |ra1si
        |}""".stripMargin,
    )
    marked(lang, "abrasive") shouldBe "abra-sive"
  }

  "French breaks its words where French breaks them" in {
    marked(embedded("fr"), "français") shouldBe "fran-çais"
    marked(embedded("fr"), "prophète") shouldBe "pro-phète"
    marked(embedded("fr"), "parole") shouldBe "pa-role"
    marked(embedded("fr"), "différent") shouldBe "dif-fé-rent"
    marked(embedded("fr"), "espérance") shouldBe "es-pé-rance"
    marked(embedded("fr"), "résurrection") shouldBe "ré-sur-rec-tion"
    marked(embedded("fr"), "miséricorde") shouldBe "mi-sé-ri-corde"
    marked(embedded("fr"), "bibliothèque") shouldBe "bi-blio-thèque"
  }

  "Spanish and Italian likewise" in {
    marked(embedded("es"), "palabra") shouldBe "pa-la-bra"
    marked(embedded("es"), "misericordia") shouldBe "mi-se-ri-cor-dia"
    marked(embedded("es"), "resurrección") shouldBe "re-su-rrec-ción"
    marked(embedded("it"), "parola") shouldBe "pa-ro-la"
    marked(embedded("it"), "misericordia") shouldBe "mi-se-ri-cor-dia"
  }

  "English, which was always right about which patterns match" in {
    marked(embedded("en-us"), "hyphenation") shouldBe "hy-phen-ation"
    marked(embedded("en-us"), "constitution") shouldBe "con-sti-tu-tion"
    marked(embedded("en-us"), "beautiful") shouldBe "beau-ti-ful"
  }

  "English asks for three letters after a break, and the file says so" in {
    // The patterns offer `comput-er`, and English typography does not take it: two letters on the next line
    // is not enough. Every `hyph-utf8` file states the minima its language is set with, and until they were
    // read texish used two-and-two for every language, which is TeX's default for no language in particular.
    marked(embedded("en-us"), "computer") shouldBe "com-puter"
    marked(embedded("en-us"), "flourishes") shouldBe "flour-ishes"
  }
