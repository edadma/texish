package io.github.edadma.typesetter.parser

import io.github.edadma.typesetter.StubTypesetter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PrimitivesTests extends AnyFreeSpec with Matchers:

  def fixture(): (StubTypesetter, Processor) =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  def capturedOutput(body: => Unit): String =
    val out = new java.io.ByteArrayOutputStream
    Console.withOut(out)(body)
    out.toString

  "\\vbox without to: builds at natural size, not target 0" in {
    val (_, proc) = fixture()
    // a null target boxed through java.lang.Double once unboxed to 0.0, making
    // every natural-size \vbox warn that its content didn't match a 0pt target
    val output = capturedOutput {
      proc.process("\\vbox{hello}")
    }
    output should not include "does not match target"
  }

  "\\hbox without to: builds at natural size, not target 0" in {
    val (_, proc) = fixture()
    val output = capturedOutput {
      proc.process("\\hbox{hello}")
    }
    output should not include "does not match target"
  }

  "\\hbox to:<dimension> targets the given size" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("\\hbox to:100 {\\hss hello\\hss}")
  }
