package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import io.github.edadma.texish.KnuthPlass.Item.*

/** Breaking a word that already carries a hyphen.
  *
  * `self-evident` may be broken at the hyphen it has, and the line then ends with that hyphen and nothing
  * added. What must not happen is a hyphenation point being offered *beside* the existing hyphen: breaking
  * there ends the line with the word's own hyphen and the one added for the break, setting `self--`. The
  * greedy setter never had the fault, breaking such words only at their own hyphens; the optimal breaker
  * hyphenated the whole word, hyphen and all, and the fault showed only when the measure happened to make
  * that break the best one.
  */
class HyphenatedCompoundTests extends AnyFreeSpec with Matchers:

  private def points(word: String, lang: String): Seq[String] =
    val t = new HeadlessTypesetter
    Hyphenation.enableEmbedded(lang) shouldBe true
    KnuthPlass
      .buildItems(Seq(t.charBox(word)), 50, Some(lang))
      .collect { case BoxItem(b: CharBox, _) => b.text }
      .toSeq

  "a hyphenated compound is never offered a break beside its own hyphen" in {
    // Every segment the breaker may end a line with, for words carrying a hyphen. None may end with the
    // hyphen — that segment plus the added hyphen is the doubled one — and none may begin with it either.
    for (word, lang) <- Seq(
        "self-evident"  -> "en-us",
        "well-established" -> "en-us",
        "Croyez-vous"   -> "fr",
        "aujourd-hui"   -> "fr",
      )
    do
      val segs = points(word, lang)
      segs.mkString shouldBe word              // the word is still spelled the same, however it is cut
      forAll(segs)(s => s should not endWith "-")
      forAll(segs)(s => s should not startWith "-")
  }

  private def forAll[A](xs: Seq[A])(f: A => Unit): Unit = xs.foreach(f)

  "a word without a hyphen still offers its hyphenation points" in {
    // The filter must not disarm ordinary hyphenation: this word is still cut into several pieces.
    val segs = points("hyphenation", "en-us")
    segs.length should be > 1
    segs.mkString shouldBe "hyphenation"
  }
