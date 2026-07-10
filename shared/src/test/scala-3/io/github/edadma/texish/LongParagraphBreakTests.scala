package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** A long paragraph with many break opportunities per line must break in near-linear time. Two things bound the
  * Knuth-Plass active set so it does not grow with paragraph length:
  *
  *   1. an active node is retired the moment its line to the current position is overfull — cumulative width is
  *      monotonic, so such a node can never start a feasible line again;
  *   2. when every line has the same measure, active nodes are pruned by (position, fitness) alone — the line
  *      number cannot change a node's future, so keeping one per line-count only bloats the set.
  *
  * Without both, a whole Bible chapter set as a single densely-hyphenated paragraph (a penalty between most
  * syllables) drove the solver cubic and turned rendering into an effective hang. These tests exercise that
  * shape directly, so dropping either bound shows up as the suite failing to finish rather than as a wrong break.
  */
class LongParagraphBreakTests extends AnyFreeSpec with Matchers:

  private class W(val width: Double) extends ContentBox:
    val xAdvance: Double = width
    val ascent: Double   = 8
    val descent: Double  = 2
    def draw(t: Typesetter, x: Double, y: Double): Unit = ()

  // A "word" is four small fragments joined by breakable penalties — the shape hyphenation produces — and words
  // are separated by flexible interword glue, so every word contributes four tightly-spaced break opportunities.
  private def flex = Glue(naturalSize = 4, stretch = 2, shrink = 1)

  private def denselyBreakableParagraph(words: Int): Seq[Box] =
    val out = collection.mutable.ArrayBuffer[Box]()
    for w <- 0 until words do
      if w > 0 then out += flex
      out += new W(6)
      for _ <- 0 until 3 do
        out += new Penalty(50) // a syllable break: allowed, mildly discouraged
        out += new W(6)
    out.toSeq

  "a long, densely-breakable paragraph breaks and terminates promptly" in {
    val t = new HeadlessTypesetter

    // ~1000 words × 4 fragments ≈ 8000 breakpoints in one paragraph — the scale that hung before the active set
    // was bounded. It now solves in well under a second; the assertions prove it finished and produced a sane
    // number of lines rather than a degenerate one-line or per-word break.
    val boxes  = denselyBreakableParagraph(1000)
    val result = KnuthPlass.breakParagraph(boxes, 200.0, t)

    result shouldBe defined
    val lines = result.get
    lines.length should (be > 50 and be < 1000)
  }

  "a hanging indent still keeps line-numbered nodes, so it breaks correctly" in {
    // With \hangindent the measure depends on the line number, so the solver must keep per-line active nodes.
    // This exercises that branch: the paragraph must still break (and into more than one line).
    val t = new HeadlessTypesetter
    t.set("hangindent", 40.0)
    t.set("hangafter", 1.0) // full-width first line, the rest indented

    val boxes  = denselyBreakableParagraph(60)
    val result = KnuthPlass.breakParagraph(boxes, 200.0, t)

    result shouldBe defined
    result.get.length should be > 1
  }
