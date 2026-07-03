package io.github.edadma.texish.parser

import java.nio.file.Files

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `\include` path resolution — a real filesystem is required, so this lives in the JVM suite. */
class IncludeResolutionTests extends AnyFreeSpec with Matchers:

  "\\include resolves a relative path against the document's directory" in {
    // the included file sits beside the (virtual) document, not under the process CWD
    val dir = Files.createTempDirectory("texish-include")
    Files.writeString(dir.resolve("part.texish"), "world")

    val handler = new StringHandler
    val proc    = new Processor(handler)
    proc.setBaseDir(dir.toString)
    proc.process("hello \\include{part.texish}")
    handler.result shouldBe "hello world"
  }

  "\\include still accepts an absolute path" in {
    val dir  = Files.createTempDirectory("texish-include-abs")
    val file = dir.resolve("abs.texish")
    Files.writeString(file, "content")

    val handler = new StringHandler
    val proc    = new Processor(handler)
    proc.process(s"\\include{$file}")
    handler.result shouldBe "content"
  }
