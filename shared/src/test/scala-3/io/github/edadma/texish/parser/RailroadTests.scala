package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, CharBox, HBox, PictureBox, PictureOp, HeadlessTypesetter, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The bundled `railroad` package draws a railroad (syntax) diagram from a grammar in W3C-style EBNF, one picture
  * per rule, entirely in the document language over the `\picture` layer. These tests load the package from
  * `packages/` and check that a grammar lexes, parses and lays out into the expected number of pictures with a
  * non-empty display list — i.e. that the whole pipeline runs without error.
  */
class RailroadTests extends AnyFreeSpec with Matchers:

  private class Capture extends HeadlessTypesetter:
    val pictures = ArrayBuffer[PictureBox]()
    override infix def add(box: Box): Typesetter =
      box match
        case pb: PictureBox => pictures += pb
        case _              =>
      super.add(box)

  private def run(src: String): Capture =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.process(src)
    t

  /** The text of every box the picture places, so a test can say which labels the diagram drew and how often. */
  private def labels(p: PictureBox): Vector[String] =
    def text(b: Box): String = b match
      case c: CharBox => c.text
      case h: HBox    => h.boxes.map(text).mkString
      case _          => ""
    p.displayList.collect { case PictureOp.Place(b, _, _, _) => text(b) }.filter(_.nonEmpty)

  "a separated list folds into one item on the line with its separator on a return loop" in {
    // A ("," A)* is drawn the bottlecaps way — the item once, with the comma looping back — rather than as the
    // literal "A then zero-or-more of (comma A)". The fold is decided by comparing the two leaves, and that
    // comparison read one of them through a name carrying a digit, which tokenizes as the name without the
    // digit followed by text: the two leaves never matched, and the whole idiom was dead.
    val folded = labels(run("\\use{railroad}\\railroad{ list ::= item (\",\" item)* }").pictures.head)
    folded.count(_ == "item") shouldBe 1
    folded should contain(",")

    // the same grammar written out with two distinct items has nothing to fold, and draws both
    val literal = labels(run("\\use{railroad}\\railroad{ pair ::= item (\",\" other)* }").pictures.head)
    literal.count(_ == "item") shouldBe 1
    literal.count(_ == "other") shouldBe 1
  }

  "a single-rule grammar produces one diagram" in {
    val t = run("\\use{railroad}\\railroad{greeting ::= \"hello\" name}")
    t.pictures should have size 1
    t.pictures.head.displayList should not be empty
  }

  "each rule produces its own diagram" in {
    val t = run("\\use{railroad}\\railroad{ a ::= \"x\"\n b ::= \"y\"\n c ::= \"z\" }")
    t.pictures should have size 3
  }

  "alternation, grouping, optional and repetition all lay out" in {
    val t = run(
      "\\use{railroad}\\railroad{ stmt ::= \"if\" \"(\" cond \")\" stmt (\"else\" stmt)? | \"{\" stmt* \"}\" }",
    )
    t.pictures should have size 1
    t.pictures.head.displayList should not be empty
  }

  "a character class lays out (as a hexagon)" in {
    val t = run("\\use{railroad}\\railroad{ digit ::= [0-9]+ }")
    t.pictures should have size 1
    t.pictures.head.displayList should not be empty
  }

  "an optional choice lays out (skip folded into the choice)" in {
    val t = run("\\use{railroad}\\railroad{ factor ::= primary ( \"?\" | \"*\" | \"+\" )? }")
    t.pictures should have size 1
    t.pictures.head.displayList should not be empty
  }
