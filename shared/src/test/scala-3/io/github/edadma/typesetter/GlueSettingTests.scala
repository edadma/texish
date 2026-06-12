package io.github.edadma.typesetter

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Glue setting follows TeX's rules: all adjustment goes to the glue at the highest infinity order that can absorb
  * it, stretching is unbounded, and finite shrink is a hard limit (overfull box past it).
  */
class GlueSettingTests extends AnyFreeSpec with Matchers:

  def build(target: Double, boxes: Box*): HBox =
    val t = new StubTypesetter
    val b = new HBoxBuilder(t, target)
    boxes.foreach(b add _)
    b.result.asInstanceOf[HBox]

  def widths(box: HBox): Seq[Double] = box.boxes.map(_.width)

  def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  "stretching distributes the excess in proportion to stretch" in {
    val box = build(100, HSpaceBox(40), Glue(10, 5, 0), Glue(10, 15, 0))
    box.width shouldBe 100.0 +- 1e-9
    // excess of 40 split 1:3
    widths(box) shouldBe Seq(40.0, 20.0, 40.0)
  }

  "infinite-order glue absorbs all the stretch, finite glue stays natural" in {
    val box = build(100, HSpaceBox(40), Glue(10, 5, 0), FilGlue)
    box.width shouldBe 100.0 +- 1e-9
    // fil takes the whole 50; the finite glue is set to its natural 10
    widths(box) shouldBe Seq(40.0, 10.0, 50.0)
  }

  "shrinking distributes the deficit in proportion to shrink" in {
    val box = build(50, HSpaceBox(40), Glue(10, 0, 4), Glue(10, 0, 6))
    box.width shouldBe 50.0 +- 1e-9
    // deficit of 10 split 2:3
    widths(box) shouldBe Seq(40.0, 6.0, 4.0)
  }

  "infinite-order shrink absorbs the whole deficit, finite glue stays natural" in {
    // this is the case the old accounting double-shrank: leftover bookkeeping
    // after a successful infinite-order pass re-shrank the finite glue too
    val box = build(56, HSpaceBox(40), Glue(10, 0, 5), Glue(10, 0, 1, shrinkOrder = 1))
    box.width shouldBe 56.0 +- 1e-9
    widths(box) shouldBe Seq(40.0, 10.0, 6.0)
  }

  "finite shrink is a hard limit: glue stops at maximum shrink and the box is overfull" in {
    val box = quietly(build(40, HSpaceBox(40), Glue(10, 0, 3)))
    // wanted to shrink 10 but only 3 available: glue is set to 7, not 0 or negative
    widths(box) shouldBe Seq(40.0, 7.0)
    box.width shouldBe 47.0 +- 1e-9
  }

  "the overfull box is reported" in {
    val out = new java.io.ByteArrayOutputStream
    Console.withOut(out)(build(40, HSpaceBox(40), Glue(10, 0, 3)))
    out.toString should include("overfull")
  }

  "stretch and shrink orders are independent: finite stretch loses to fil, infinite shrink wins" in {
    // glue like `12pt plus 2pt minus 1fil` stretches at order 0 but shrinks at order 1
    val mixed = Glue(12, 2, 1, 0, 1)

    // stretching: FilGlue's order-1 stretch outranks the mixed glue's finite stretch
    val stretched = build(100, HSpaceBox(40), mixed, FilGlue)
    widths(stretched) shouldBe Seq(40.0, 12.0, 48.0)

    // shrinking: the mixed glue's order-1 shrink outranks finite shrink elsewhere
    val shrunk = build(45, HSpaceBox(40), Glue(10, 0, 5), mixed)
    widths(shrunk) shouldBe Seq(40.0, 10.0, -5.0)
  }

  "exact fit sets glue at natural size without warnings" in {
    val out = new java.io.ByteArrayOutputStream
    val box = Console.withOut(out)(build(50, HSpaceBox(40), Glue(10, 5, 5)))
    box.width shouldBe 50.0 +- 1e-9
    out.toString shouldBe empty
  }
