package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Regression tests for the backend-support defects found in the July 2026 review that live in shared code:
  * colour channel rounding, MarginBox's ignored left margin, and pica formatting. The per-backend drawing
  * fixes (stroke transforms, line caps, empty extents, Cairo rectangles and image ownership) are covered in
  * the JVM and native suites, where a real drawing surface exists. */
class BackendFixesTests extends AnyFreeSpec with Matchers:

  "colour channels round to the nearest byte value" in {
    // truncation mapped 0.5 to 127 while Cairo, fed the raw double, rendered 128 — a visible one-step
    // mismatch on any mid-tone between the raster/SVG backends and the PDF
    Color(0.5, 0.5, 0.5, 1.0).redInt shouldBe 128
    Color(1.0, 1.0, 1.0, 1.0).redInt shouldBe 255
    Color(0.0, 0.0, 0.0, 1.0).redInt shouldBe 0
    Color(127.4 / 255, 0, 0, 1.0).redInt shouldBe 127
  }

  "a MarginBox shifts its content right by the left margin" in {
    class Probe extends ContentBox:
      var drawnAt: Option[(Double, Double)] = None
      val width: Double                     = 10
      val xAdvance: Double                  = 10
      val ascent: Double                    = 5
      val descent: Double                   = 0
      def draw(t: Typesetter, x: Double, y: Double): Unit = drawnAt = Some((x, y))

    val t     = new HeadlessTypesetter
    val probe = new Probe
    val box   = new MarginBox(probe, top = 2, right = 3, bottom = 4, left = 5)

    box.width shouldBe 18 // 10 + 5 + 3
    box.draw(t, 100, 200)
    probe.drawnAt shouldBe Some((105.0, 200.0)) // content starts after the left margin, baseline unmoved
  }

  "toPicas splits a length into whole picas and leftover points" in {
    val uc = new UnitConverter(null)
    uc.toPicas(30) shouldBe (2.0, 6.0, "2p6")
    uc.toPicas(12) shouldBe (1.0, 0.0, "1p0")
    uc.toPicas(7) shouldBe (0.0, 7.0, "0p7")
  }
