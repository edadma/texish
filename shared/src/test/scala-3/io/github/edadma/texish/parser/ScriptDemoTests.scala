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

  "the hyphenation demo processes without error" in {
    // Not a script demo, but the same kind of file for the same reason: it names languages from the pattern
    // tree, and a tag that has been renamed or a file that has been dropped breaks it.
    noException should be thrownBy process("scripts/hyphenation-demo.script")
  }
