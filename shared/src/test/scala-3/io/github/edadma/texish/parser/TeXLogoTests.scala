package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, CharBox, GlyphBox, HBox, HSpaceBox, ShiftBox, StubTypesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// The TeX logo and its building blocks. \TeX is a primitive: it builds a T, an E lowered half an x-height
// and kerned back, then an X, with the three capitals placed through the glyph seam. \kern (a negative rigid
// kern) and \lower (a box shifted down) are exercised here too, and the lookup rule that a \def macro
// overrides a built-in primitive of the same name.
class TeXLogoTests extends AnyFreeSpec with Matchers:

  private def boxesOf(src: String): List[Box] =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.process(src)
    t.mode.asInstanceOf[Builder].list

  "\\TeX builds its logo: glyph T, a lowered glyph E, and glyph X, kerned together" in {
    val bs = boxesOf(raw"\noindent\TeX")
    // the whole logo is one hbox dropped into the line
    val hboxes = bs.collect { case h: HBox => h }
    hboxes should have size 1
    val logo = hboxes.head.boxes

    // the T and X ride the glyph seam directly; the E sits inside the lowered box
    logo.collect { case g: GlyphBox => g } should have size 2
    val shifted = logo.collect { case s: ShiftBox => s }
    shifted should have size 1
    shifted.head.box shouldBe a[GlyphBox]       // the lowered E is a glyph too
    shifted.head.shift should be > 0.0          // lowered = shifted down
    logo.collect { case h: HSpaceBox if h.width < 0 => h } should have size 2 // the two negative kerns
  }

  "a document can redefine \\TeX with \\def — a macro overrides the built-in primitive" in {
    // \def\TeX wins over the primitive of the same name (TeX's own rule), so the redefinition takes effect:
    // the new body uses \lower/\hbox and ordinary letters, so its X and T set as CharBoxes.
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
