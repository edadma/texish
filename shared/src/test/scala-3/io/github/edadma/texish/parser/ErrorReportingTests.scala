package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.StubTypesetter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Errors raised by the host engine while handling a token are reported at that token's source position, the same way
  * language-level errors are — a bare `sys.error` from deep in the engine should still point at the script line that
  * triggered it.
  */
class ErrorReportingTests extends AnyFreeSpec with Matchers:

  def fixture(): (StubTypesetter, Processor) =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  "engine errors gain the position of the token that triggered them" in {
    val (_, proc) = fixture()
    proc.registerPrimitive(
      "boom",
      new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit = sys.error("engine failure")
      },
    )
    val e = intercept[ParserException](proc.process("first line\n\\boom"))
    e.getMessage should include("engine failure")
    e.pos should not be null
    e.pos.line shouldBe 2
  }

  "language errors keep their original position and are not re-wrapped" in {
    val (_, proc) = fixture()
    val e = intercept[ParserException](proc.process("\\vskip {nonsense}"))
    e.getMessage should include("\\vskip expects a dimension or glue")
    e.pos.line shouldBe 1
  }
