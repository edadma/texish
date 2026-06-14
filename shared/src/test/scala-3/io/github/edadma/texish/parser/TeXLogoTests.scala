package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, CharBox, HSpaceBox, ShiftBox, StubTypesetter, standardPrelude}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// The TeX logo and its building blocks. \TeX is predefined with the plain.tex recipe
// (T\kern-.1667em\lower.5ex\hbox{E}\kern-.125em X), so these also exercise \kern (a negative rigid
// kern) and \lower (a box shifted down).
class TeXLogoTests extends AnyFreeSpec with Matchers:

  private def boxesOf(src: String): List[Box] =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.process(standardPrelude) // the standard format, where \TeX is defined
    proc.process(src)
    t.mode.asInstanceOf[Builder].list

  "\\TeX is predefined: T, a lowered E, and X, kerned together" in {
    val bs = boxesOf(raw"\noindent\TeX")
    bs.collect { case c: CharBox => c.text } shouldBe List("T", "X") // the E sits inside the lowered box
    val shifted = bs.collect { case s: ShiftBox => s }
    shifted should have size 1
    shifted.head.shift should be > 0.0 // lowered = shifted down
    bs.collect { case h: HSpaceBox if h.width < 0 => h } should have size 2 // the two negative kerns
  }

  "a document can redefine \\TeX (so it is a macro, not a fixed primitive)" in {
    // The redefinition still relies on \kern/\lower, and overriding it proves \TeX is a macro —
    // a primitive of the same name could not be shadowed by \def.
    val bs = boxesOf(raw"\noindent\def\TeX{X\lower.5ex\hbox{E}T}\TeX")
    bs.collect { case c: CharBox => c.text } shouldBe List("X", "T")
    bs.collect { case s: ShiftBox => s } should have size 1
  }

  "\\kern adds a rigid horizontal space, negative shrinking the run" in {
    val bs = boxesOf(raw"\noindent A\kern-2pt B")
    val kern = bs.collect { case h: HSpaceBox => h }
    kern should have size 1
    kern.head.width shouldBe -2.0 +- 1e-9
  }
