package io.github.edadma.texish.parser

import io.github.edadma.texish.HeadlessTypesetter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The complex-script demos in `scripts/` are what the corpus renderer exercises the shapers with, and each is
  * also the longest piece of real text in its script that anyone reads. A font that failed to load, a
  * character outside the bundled face's coverage, or a `\font` name that has been renamed breaks them — and
  * that break is otherwise only found by rendering the corpus with the native binary. Processing the scripts
  * here catches it in the ordinary suite instead, the same way [[MathDemoTests]] does for math mode.
  *
  * [[HeadlessTypesetter]] loads the bundled catalogue itself, which is what makes the catalogue faces these
  * demos name resolvable from the checkout. */
class ScriptDemoTests extends AnyFreeSpec with Matchers:

  private def process(path: String): Unit =
    // read the way the engine itself reads a file, which is the API that cross-compiles to every platform
    val bytes = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(path))
    val text  = new String(bytes, "UTF-8")

    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)

    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(text))

  "the Gujarati demo processes without error" in {
    noException should be thrownBy process("scripts/gujarati-demo.script")
  }

  "the Kannada demo processes without error" in {
    noException should be thrownBy process("scripts/kannada-demo.script")
  }

  "the Ethiopic demo processes without error" in {
    noException should be thrownBy process("scripts/ethiopic-demo.script")
  }

  // The chess demo is here for a second reason: its diagrams are positions a line of moves reached, so a defect
  // in reading the notation shows up as a wrong diagram rather than as an error, and processing it at least
  // proves the whole path from a game score to a drawn board still runs.
  "the chess demo processes without error" in {
    noException should be thrownBy process("scripts/chess-demo.script")
  }

  "the hyphenation demo processes without error" in {
    // Not a script demo, but the same kind of file for the same reason: it names languages from the pattern
    // tree, and a tag that has been renamed or a file that has been dropped breaks it.
    noException should be thrownBy process("scripts/hyphenation-demo.script")
  }

  // The document and book demos are the two that claim to exercise every command their package provides, so a
  // feature added to either package is added to its demo as well — and a demo that has stopped processing is
  // the first sign that it was not.
  "the document demo processes without error" in {
    noException should be thrownBy process("scripts/document-demo.script")
  }

  "the book demo processes without error" in {
    noException should be thrownBy process("scripts/book-demo.script")
  }

  // The plot demo is the package's own worked example: every series kind, both auto-ranging modes, the legend
  // and the frame options, in one document. Its series are now folds over the point list rather than hand-rolled
  // walks, so a defect in \chunk, \reverse, \minimum or a macro's value shows up here as a broken figure.
  "the plot demo processes without error" in {
    noException should be thrownBy process("scripts/plot-demo.script")
  }

  // The QR demo is the qrcode package end to end: \use, the option bracket, the verbatim URL and the picture.
  // It was an engine primitive until the package replaced it, so this is also what says the replacement still
  // draws something — a broken encoder throws here rather than shipping a symbol that quietly will not scan.
  "the QR demo processes without error" in {
    noException should be thrownBy process("scripts/qr-demo.script")
  }
