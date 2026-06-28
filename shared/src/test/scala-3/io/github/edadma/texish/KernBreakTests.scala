package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** A kern is a rigid box, not an interword space, so the line breaker must never break at one. Knuth-Plass
  * already treats it as a rigid box, but the greedy fallback (used when no solution fits within \tolerance)
  * once backed up to "the last space" by `isSpace`, which an HSpaceBox kern reports true — so the negative
  * kerns inside a wordmark like \TeX let the logo split across a line. This pins the fix: the greedy breaker
  * backs up only to real glue. Each test character is six units wide (HeadlessTypesetter).
  */
class KernBreakTests extends AnyFreeSpec with Matchers:

  // A vertical galley that also records the line boxes the paragraph contributes to it.
  private class CapturingGalley(t0: Typesetter) extends VBoxBuilder(t0):
    val contributed = ArrayBuffer[Box]()
    override infix def add(box: Box): Unit =
      contributed += box
      super.add(box)

  private def charTexts(b: Box): Seq[String] = b match
    case h: HBox    => h.boxes.flatMap(charTexts)
    case c: CharBox => Seq(c.text)
    case _          => Seq.empty

  "the greedy breaker keeps a kern-joined wordmark whole — it does not break at a kern" in {
    val t = new HeadlessTypesetter
    t.set("hsize", 60.0)
    t.set("tolerance", 0.0) // no line can justify perfectly, so the greedy fallback runs

    val galley = new CapturingGalley(t)
    t.push(galley)
    val pm = new ParagraphMode(t)
    t.push(pm)

    // "wwwwwwww" (48) + space (6) + "A" (6) exactly fills the 60-unit measure; the −3 kern then leaves "B"
    // unable to fit. The bug ended the line right after "A" (treating the kern as a space); the fix backs up
    // past the rigid kern to the real space, carrying the whole "A〈kern〉B" to the next line.
    pm add new CharBox(t, "wwwwwwww")
    pm add t.getGlue("spaceskip")
    pm add new CharBox(t, "A")
    pm add HSpaceBox(-3)
    pm add new CharBox(t, "B")
    pm add t.getGlue("spaceskip")
    pm add new CharBox(t, "cccc")
    pm.done()

    val lines   = galley.contributed.collect { case h: HBox => h }
    val lineOfA = lines.indexWhere(charTexts(_).contains("A"))
    val lineOfB = lines.indexWhere(charTexts(_).contains("B"))

    lineOfA should be >= 0
    lineOfB shouldBe lineOfA // A and B share a line: the kern between them was not a breakpoint
  }
