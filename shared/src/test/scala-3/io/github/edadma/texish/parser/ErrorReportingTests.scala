package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.{HeadlessTypesetter, TexishException}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Errors raised by the host engine while handling a token are reported at that token's source position, the same way
  * language-level errors are — a failure from deep in the engine should still point at the script line that triggered
  * it.
  *
  * The engine raises a `TexishException` for a fault in the document. Anything else that escapes it is a defect in
  * texish, and is reported as such rather than as the author's mistake; see the last test.
  */
class ErrorReportingTests extends AnyFreeSpec with Matchers:

  def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  "engine errors gain the position of the token that triggered them" in {
    val (_, proc) = fixture()
    proc.registerPrimitive(
      "boom",
      new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit =
          throw io.github.edadma.texish.TexishException("engine failure")
      },
    )
    val e = intercept[TexishException](proc.process("first line\n\\boom"))
    e.getMessage should include("engine failure")
    e.getMessage should not include "internal error" // the document is at fault, and is told so plainly
    e.pos should not be null
    e.pos.line shouldBe 2
  }

  "language errors keep their original position and are not re-wrapped" in {
    val (_, proc) = fixture()
    val e = intercept[TexishException](proc.process("\\vskip {nonsense}"))
    e.getMessage should include("\\vskip expects a dimension or glue")
    e.pos.line shouldBe 1
  }

  // A mistake in the document and a defect in texish are different failures and are reported differently. The
  // first is the author's to fix; the second is not, and blaming the document for it sends them hunting through
  // their source for a mistake that is not there.

  "an unexpected exception is reported as an internal error and keeps its cause" in {
    // Not a TexishException: this stands in for a defect in the engine — a null dereference in layout, say.
    val (_, proc) = fixture()
    val bug       = new IllegalStateException("index out of range")
    proc.registerPrimitive(
      "bug",
      new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit = throw bug
      },
    )
    val e = intercept[TexishException](proc.process("ok\n\\bug"))
    e.getMessage should include("internal error")
    e.getMessage should include("IllegalStateException") // the reader is told what actually failed
    e.getMessage should include("index out of range")
    e.pos.line shouldBe 2      // still located, so it is known which token provoked it
    e.getCause shouldBe bug    // and the stack trace that finds the bug is not thrown away
  }
