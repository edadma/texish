package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Hooks: `\addtohook name {code}` registers deferred code under a name and `\usehook name` runs every fragment,
  * in registration order, at that point in the document. The code is kept verbatim and only executed when the
  * hook fires, so it can refer to things defined after it was registered. Hooks are global, so a fragment added
  * inside a group still fires later.
  */
class HookTests extends AnyFreeSpec with Matchers:

  private def run(src: String): String =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h.result

  "\\usehook runs registered code" in {
    run("\\addtohook h {hello}\\usehook h") shouldBe "hello"
  }

  "fragments run in registration order" in {
    run("\\addtohook h {A}\\addtohook h {B}\\usehook h") shouldBe "AB"
  }

  "the code is deferred — it can use a macro defined after it was registered" in {
    run("\\addtohook h {\\foo}\\def foo {bar}\\usehook h") shouldBe "bar"
  }

  "an empty or unknown hook is a silent no-op" in {
    run("before\\usehook nope after") shouldBe "before after"
  }

  "hooks are global — a fragment added in a group still fires" in {
    run("{\\addtohook h {x}}\\usehook h") shouldBe "x"
  }

  "a hook can be run more than once" in {
    run("\\addtohook h {x}\\usehook h\\usehook h") shouldBe "xx"
  }
