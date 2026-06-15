package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The keyed-store primitives: \mapset writes a value under a (computed) key in a map variable, \mapget reads it,
  * \maphas tests for it. Keys are expressions, so the store can be addressed dynamically — the foundation the
  * counters package builds on. Tested with a StringHandler.
  */
class MapStoreTests extends AnyFreeSpec with Matchers:

  private def run(src: String): StringHandler =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h

  "\\mapset then \\mapget round-trips a value under a key" in {
    val h = run("\\mapset m {section} {3}\\set v {\\mapget m {section}}")
    Value.display(h.get("v")) shouldBe "3"
  }

  "the key may be computed" in {
    val h = run("\\set k {sub}\\mapset m {\\k} {7}\\set v {\\mapget m {sub}}")
    Value.display(h.get("v")) shouldBe "7"
  }

  "\\mapget of a missing key is Undefined" in {
    val h = run("\\mapset m {a} {1}\\set v {\\mapget m {b}}")
    h.get("v") shouldBe Value.Undefined
  }

  "\\maphas reports key presence" in {
    val h = run("\\mapset m {a} {1}\\set p {\\maphas m {a}}\\set q {\\maphas m {z}}")
    h.get("p") shouldBe Value.Bool(true)
    h.get("q") shouldBe Value.Bool(false)
  }

  "\\mapset updates an existing key without disturbing the others" in {
    val h = run("\\mapset m {a} {1}\\mapset m {b} {2}\\mapset m {a} {9}" +
      "\\set x {\\mapget m {a}}\\set y {\\mapget m {b}}")
    Value.display(h.get("x")) shouldBe "9"
    Value.display(h.get("y")) shouldBe "2"
  }

  "a plain \\mapset is local to its group; \\global\\mapset persists" in {
    val h = run("{\\mapset loc {a} {1}\\global\\mapset glob {a} {1}}" +
      "\\set v {\\mapget glob {a}}")
    h.get("loc") shouldBe Value.Undefined    // the whole local map vanished with the group
    Value.display(h.get("v")) shouldBe "1"   // the global map survived
  }
