package io.github.edadma.typesetter.texish

import io.github.edadma.typesetter.{Box, Builder, Glue, StubTypesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ParagraphVskipSpacingTests extends AnyFreeSpec with Matchers:

  private def vlist(src: String): List[Box] =
    val t       = new StubTypesetter
    t.set("vsize", 1.0e9) // one page, no page breaking during the probe
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream) {
      proc.process(src)
      t.paragraph() // close the last paragraph so its lines reach the page list
    }
    t.mode.asInstanceOf[Builder].list

  private def describe(b: Box): String = b match
    case g: Glue => f"Glue(${g.naturalSize}%.1f)"
    case other   => f"${other.getClass.getSimpleName}(h=${other.height}%.1f)"

  // A vertical command ends the current paragraph wherever it appears, so a blank line above a
  // \vskip is redundant: TeX produces the same vertical list either way. The engine must too —
  // a stale pending newline-space must not leak a stray box into the vertical list when \vskip
  // closes the paragraph directly (no blank line before it).
  "a blank line above \\vskip does not change the spacing" in {
    val withBlank = vlist("alpha\n\n\\vskip 12pt\n\nbeta\n").map(describe)
    val without   = vlist("alpha\n\\vskip 12pt\n\nbeta\n").map(describe)
    without shouldBe withBlank
    // and the list is the clean shape: two text lines with only glue between them, no stray box
    without.count(_.startsWith("HBox")) shouldBe 2
    without.count(_.startsWith("CharBox")) shouldBe 0
  }
