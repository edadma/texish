package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// A CharBox must take its width from the pen advance, not the ink bounding box. The two differ by the run's
// leading and trailing side bearings; laying text out by the ink width drops them, which is invisible in
// justified prose but breaks exact character spacing (the reason the \TeX logo had to be built from glyphs).
class CharBoxAdvanceTests extends AnyFreeSpec with Matchers:

  // A stub whose advance is deliberately wider than the ink box, so the two are distinguishable: each
  // character inks 6 wide but advances 6, plus a 3-unit side bearing at each end of the whole run.
  private class BearingStub extends StubTypesetter:
    override def getTextExtents(text: String, font: RenderFont): TextExtents =
      val ink     = text.length * 6.0
      val advance = ink + 6.0 // 3 units of side bearing at each end
      TextExtents(3, -8, ink, 10, advance, 0)

  "a CharBox is as wide as its advance, not its ink box" in {
    val t = new BearingStub
    val box = t.charBox("AB")
    box.width shouldBe 18.0    // 2*6 ink + 6 bearings = advance
    box.xAdvance shouldBe 18.0
    box.width shouldBe box.xAdvance
  }

  "a run of text advances by the full advance so neighbours don't crowd" in {
    val t = new BearingStub
    // Two words set as separate boxes advance by their advances, so the second begins after the first's
    // full advance — not jammed against its ink.
    val first  = t.charBox("foo")
    val second = t.charBox("bar")
    (first.width + second.width) shouldBe 48.0 // (3*6+6) + (3*6+6)
  }
