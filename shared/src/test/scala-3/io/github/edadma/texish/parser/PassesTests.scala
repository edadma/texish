package io.github.edadma.texish.parser

import io.github.edadma.texish.{HeadlessTypesetter, Passes, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The multi-pass driver: a document is typeset repeatedly over one shared reference table until its
  * cross-references stop moving, so a forward `\ref` (to a label defined later) resolves on the following pass. A
  * document with no references settles in a single pass.
  */
class PassesTests extends AnyFreeSpec with Matchers:

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  private def runOnce(t: Typesetter, source: String): Unit =
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.process(source)
    t.end()

  private def textOf(t: Typesetter, key: String): String = t.get(key) match
    case Some(Value.Text(s)) => s
    case Some(v)             => Value.display(v)
    case None                => "<unset>"

  "a forward reference is unresolved on the first pass and resolved on the next" in quietly {
    val captured = ArrayBuffer[String]()

    Passes.untilStable() { () => new HeadlessTypesetter } { t =>
      runOnce(t, "\\set seen {\\ref{later}}\\set currentlabel {42}\\label{later}")
      captured += textOf(t, "seen")
    }

    captured.head shouldBe "??" // pass 1: the label has not been seen yet
    captured.last shouldBe "42" // resolved from the previous pass
    captured.length shouldBe 2  // and the references have settled, so it stops
  }

  "a document with no cross-references runs exactly one pass" in quietly {
    var passes = 0

    Passes.untilStable() { () => new HeadlessTypesetter } { t =>
      runOnce(t, "hello world\n\n")
      passes += 1
    }

    passes shouldBe 1
  }

  "the pass count is capped even if references never settle" in quietly {
    var passes = 0

    // a self-referential page number that shifts every pass would never converge; the cap stops it
    Passes.untilStable(maxPasses = 3) { () => new HeadlessTypesetter } { t =>
      runOnce(t, s"\\set currentlabel {$passes}\\label{x}")
      passes += 1
    }

    passes shouldBe 3
  }
