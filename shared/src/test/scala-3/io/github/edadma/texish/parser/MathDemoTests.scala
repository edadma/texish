package io.github.edadma.texish.parser

import io.github.edadma.texish.HeadlessTypesetter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The math demo in `scripts/` is the corpus renderer's exercise of math mode, and it is also the longest piece
  * of real texish anyone reads. A construct that has been renamed, or a demo line written outside the `$…$` it
  * needs, breaks it — and that break is otherwise only found by rendering the corpus with the native binary.
  * Processing the script here catches it in the ordinary suite instead.
  */
class MathDemoTests extends AnyFreeSpec with Matchers:

  "the math demo processes without error" in {
    // read the way the engine itself reads a file, which is the API that cross-compiles to every platform
    val bytes = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get("scripts/math-demo.script"))
    val text  = new String(bytes, "UTF-8")

    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)

    noException should be thrownBy Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(text))
  }
