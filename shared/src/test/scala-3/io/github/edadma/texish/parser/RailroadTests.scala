package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, PictureBox, HeadlessTypesetter, Typesetter}
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

  "a character class is accepted as a terminal" in {
    val t = run("\\use{railroad}\\railroad{ digit ::= [0-9]+ }")
    t.pictures should have size 1
  }
