package io.github.edadma.typesetter

import io.github.edadma.typesetter.opentype.ByteCursor
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The sfntTable seam on the Cairo backend, where the bytes come from FreeType's FT_Load_Sfnt_Table (the
  * binding added in freetype 0.0.7). Proves the table fetch works through the retained FreeType face; a
  * chosen math font's MATH table reaches the parser by the same route.
  */
class CairoMathSeamTests extends AnyFreeSpec with Matchers:

  "sfntTable reads a present table through FreeType and None for an absent one" in {
    val t  = new CairoImageTypesetter(100)
    val rf = t.currentFont.renderFont.asInstanceOf[t.RenderFont]

    val head = t.sfntTable(rf, "head")
    head shouldBe defined
    head.get.slice(12, 16).map(_ & 0xff) shouldBe Array(0x5f, 0x0f, 0x3c, 0xf5)
    ByteCursor(head.get, 18).u16 should (be > 0 and be <= 16384)

    t.sfntTable(rf, "MATH") shouldBe None
  }
