package io.github.edadma.texish.parser

import io.github.edadma.path.Path

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `\use{name}` imports a texish module: it locates `name.texish`, loads it once with output suppressed (so the
  * file's prose and blank lines typeset nothing — only its definitions take effect), resolves a relative `\use`
  * inside a module against that module's own directory, and skips a repeat `\use` of the same file (load-once).
  */
class UseTests extends AnyFreeSpec with Matchers:

  /** Run `src` with the document's base directory set to `dir`, so `\use` resolves packages there. */
  private def runIn(dir: String, src: String): StringHandler =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.setBaseDir(dir)
    proc.process(src)
    h

  /** Create a fresh temp directory, write the given (name -> content) files into it, run the body, then clean up. */
  private def withModules(files: (String, String)*)(body: Path => Unit): Unit =
    val dir = Path.createTempDirectory("texish-use-")
    val written = files.map { (name, content) =>
      val p = dir / name
      p.writeText(content)
      p
    }
    try body(dir)
    finally
      written.foreach(p => try p.delete() catch { case _: Exception => () })
      try dir.delete() catch { case _: Exception => () }

  "\\use loads a module's definitions" in {
    withModules(
      "greet.texish" ->
        """A line of prose that the module documents itself with.
          |
          |\def greet {hello}
          |""".stripMargin,
    ) { dir =>
      runIn(dir.toPlatformString, "\\use{greet}\\greet").result shouldBe "hello"
    }
  }

  "\\use typesets nothing while loading — only definitions take effect" in {
    withModules(
      "greet.texish" ->
        """This prose, and the blank line below, must not appear in the output.
          |
          |\def greet {hello}
          |""".stripMargin,
    ) { dir =>
      // the \use alone emits nothing; the macro it defined is what produces output when later invoked
      runIn(dir.toPlatformString, "\\use{greet}").result shouldBe ""
    }
  }

  "a module loaded twice runs only once (load-once)" in {
    withModules(
      "counter.texish" -> "\\set loaded {\\calc{\\loaded + 1}}\n",
    ) { dir =>
      runIn(dir.toPlatformString, "\\set loaded {0}\\use{counter}\\use{counter}\\the\\loaded").result shouldBe "1"
    }
  }

  "a relative \\use inside a module resolves against the module's own directory" in {
    withModules(
      "a.texish" -> "\\use{b}\n",
      "b.texish" -> "\\def fromB {B}\n",
    ) { dir =>
      runIn(dir.toPlatformString, "\\use{a}\\fromB").result shouldBe "B"
    }
  }

  "\\use of a missing module is an error" in {
    withModules() { dir =>
      a[ParserException] should be thrownBy runIn(dir.toPlatformString, "\\use{nothingHere}")
    }
  }
