package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The engine starts with TeX's default values for the layout parameters it reads, so a document that uses no
  * format still breaks paragraphs and pages the way plain TeX would. The `document` package overrides the
  * spacing parameters for its own look; these assert the bare-engine baseline. The [[HeadlessTypesetter]]
  * starts at the 14pt default font.
  */
class DefaultParametersTests extends AnyFreeSpec with Matchers:

  "the bare engine starts with TeX's default parameter values" in {
    val t = new HeadlessTypesetter

    t.getNumber("parindent") shouldBe (20.0 +- 1e-9) // \parindent = 20pt

    // \parskip = 0pt plus 1pt — a small *finite* stretch between paragraphs, not the infinite fil it used to be
    val parskip = t.getGlue("parskip")
    parskip.naturalSize shouldBe 0.0
    parskip.stretch shouldBe 1.0
    parskip.stretchOrder shouldBe 0 // finite, not fil

    t.getGlue("topskip").naturalSize shouldBe (10.0 +- 1e-9) // \topskip = 10pt

    // \parfillskip = 0pt plus 1fil — the last line of a paragraph still fills out to the right margin
    t.getGlue("parfillskip").stretchOrder shouldBe 1

    // \baselineskip = 1.2 × the font size, TeX's default leading
    t.getGlue("baselineskip").naturalSize shouldBe (t.currentFont.size * 1.2 +- 1e-9)
  }

  "changing the font size keeps TeX's 1.2x leading" in {
    val t = new HeadlessTypesetter
    t.selectFont("lmroman", 20, Set("regular"))
    t.getGlue("baselineskip").naturalSize shouldBe (24.0 +- 1e-9) // 20 × 1.2
  }
