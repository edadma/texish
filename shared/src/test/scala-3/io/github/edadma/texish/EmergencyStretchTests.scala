package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `\emergencystretch` is TeX's escape hatch for a paragraph that cannot be justified within `\tolerance`: a
  * second Knuth-Plass pass at the same tolerance, but with extra per-line stretchability so otherwise-too-loose
  * lines come within tolerance. It is off by default (0), so ordinary documents are unaffected — only when the
  * document asks for it does the breaker prefer an even, justified break over the greedy fallback.
  */
class EmergencyStretchTests extends AnyFreeSpec with Matchers:

  private class W(val width: Double) extends ContentBox:
    val xAdvance: Double = width
    val ascent: Double   = 8
    val descent: Double  = 2
    def draw(t: Typesetter, x: Double, y: Double): Unit = ()

  // A line of `W40 _ W40` (with this flexible space) needs far more stretch than tolerance allows, so it has no
  // Knuth-Plass solution at the default tolerance — a third word never fits and a lone word is rigid.
  private def flex  = Glue(naturalSize = 4, stretch = 4, shrink = 1)
  private val boxes = Seq(new W(40), flex, new W(40), flex, new W(40), flex, new W(40))

  "by default an unjustifiable paragraph yields None, leaving the greedy fallback in charge" in {
    val t = new HeadlessTypesetter
    KnuthPlass.breakParagraph(boxes, 100.0, t) shouldBe None
  }

  "emergency stretch lets Knuth-Plass justify a paragraph that would otherwise fall back to greedy" in {
    val t = new HeadlessTypesetter
    t.set("emergencystretch", 50.0)

    val result = KnuthPlass.breakParagraph(boxes, 100.0, t)
    result shouldBe defined
    result.get.length shouldBe 2
  }

  "emergency stretch keeps the normal tolerance — a tiny amount is not enough and still yields None" in {
    val t = new HeadlessTypesetter
    t.set("emergencystretch", 1.0)

    KnuthPlass.breakParagraph(boxes, 100.0, t) shouldBe None
  }
