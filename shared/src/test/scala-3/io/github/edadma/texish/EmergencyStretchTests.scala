package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** A paragraph that cannot be justified within `\tolerance` must still be broken by Knuth-Plass — a relaxed
  * second pass (optionally with `\emergencystretch`) distributes the unavoidable looseness evenly, the way TeX
  * does, instead of dropping to the greedy first-fit fallback. Only a genuinely unbreakable paragraph still
  * yields `None`.
  */
class EmergencyStretchTests extends AnyFreeSpec with Matchers:

  private class W(val width: Double) extends ContentBox:
    val xAdvance: Double = width
    val ascent: Double   = 8
    val descent: Double  = 2
    def draw(t: Typesetter, x: Double, y: Double): Unit = ()

  // A flexible inter-word space and a rigid one (no stretch), so a line's only give can be removed at will.
  private def flex  = Glue(naturalSize = 4, stretch = 4, shrink = 1)
  private def rigid = Glue(naturalSize = 4, stretch = 0, shrink = 0)

  "a paragraph that won't justify within tolerance is still broken by Knuth-Plass" in {
    // Two lines of `W40 _ W40` are the only legal shape (a third word never fits, a lone word is rigid). Each
    // interior line is badly stretched (badness ~6400, far above the default tolerance of 200), so the first
    // pass finds no solution — without the second pass this fell through to greedy.
    val t = new HeadlessTypesetter
    val boxes = Seq(new W(40), flex, new W(40), flex, new W(40), flex, new W(40))

    val result = KnuthPlass.breakParagraph(boxes, 100.0, t)
    result shouldBe defined
    result.get.length shouldBe 2
  }

  "with rigid spacing and no emergency stretch, an unjustifiable paragraph yields None" in {
    // Every inter-word space is rigid, so no line can stretch to its measure: there is no Knuth-Plass solution
    // at any tolerance, and the breaker reports None (the caller then uses the greedy fallback).
    val t = new HeadlessTypesetter
    val boxes = Seq(new W(40), rigid, new W(40), rigid, new W(40), rigid, new W(40))

    KnuthPlass.breakParagraph(boxes, 100.0, t) shouldBe None
  }

  "emergency stretch lends each line enough give to justify a rigid paragraph" in {
    // The same rigid paragraph, but \emergencystretch adds per-line stretchability so the second pass can
    // justify every line at acceptable badness — Knuth-Plass now succeeds where it returned None above.
    val t = new HeadlessTypesetter
    t.set("emergencystretch", 50.0)
    val boxes = Seq(new W(40), rigid, new W(40), rigid, new W(40), rigid, new W(40))

    val result = KnuthPlass.breakParagraph(boxes, 100.0, t)
    result shouldBe defined
    result.get.length shouldBe 2
  }
