package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** \vbox and \vtop stack the same lines to the same total height; they differ only in where the reference point
  * (the baseline aligning the box with its neighbours) falls. The HeadlessTypesetter gives each line ascent 8,
  * descent 2, height 10.
  */
class VerticalBoxTests extends AnyFreeSpec with Matchers:

  private def line(t: HeadlessTypesetter, s: String): HBox =
    new HBox(Seq(new CharBox(t, s, t.currentFont, t.currentColor)))

  "a \\vbox references its last line's baseline; a \\vtop references its first" in {
    val t    = new HeadlessTypesetter
    val vbox = new VBox(Seq(line(t, "AB"), line(t, "CD")))
    val vtop = new VTop(Seq(line(t, "AB"), line(t, "CD")))

    vbox.height shouldBe 20.0
    vtop.height shouldBe 20.0 // same total height

    vbox.ascent shouldBe 18.0 // everything above the last line's baseline
    vbox.descent shouldBe 2.0 // the last line's depth

    vtop.ascent shouldBe 8.0   // just the first line's height
    vtop.descent shouldBe 12.0 // everything below the first line's baseline
  }

  "a \\vbox ending in glue has zero depth, so its content rides above the baseline" in {
    val t = new HeadlessTypesetter
    // What `\vbox to:N {\hbox{..} \vss}` produces: a line at the top, then a vertical fill. The trailing glue
    // means the reference point is the very bottom, so the whole box sits above the baseline — this is what
    // lifts the small "A" of the LaTeX logo up to the cap height instead of leaving it on the baseline.
    val vbox = new VBox(Seq(line(t, "A"), new VSpaceBox(12.0)))

    vbox.height shouldBe 22.0  // line height 10 + 12 of fill
    vbox.descent shouldBe 0.0  // trailing glue ⇒ depth zero
    vbox.ascent shouldBe 22.0  // the entire box is above the baseline
  }

  "an empty vertical box has zero metrics" in {
    new VBox(Seq.empty).height shouldBe 0.0
    new VBox(Seq.empty).ascent shouldBe 0.0
    new VTop(Seq.empty).ascent shouldBe 0.0
    new VTop(Seq.empty).descent shouldBe 0.0
  }
