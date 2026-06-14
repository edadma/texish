package io.github.edadma.typesetter

import io.github.edadma.typesetter.opentype.{ByteCursor, Sfnt}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The sfntTable seam against real bundled fonts on the JVM backend. The text fonts carry no MATH table,
  * so this proves the fetch path (read the font file, walk its SFNT directory, slice a table) using the
  * universally-present 'head' table — a math font drops straight into the same path once one is chosen.
  */
class MathSeamTests extends AnyFreeSpec with Matchers:

  "sfntTable reads a present table (head) and reports None for an absent one (MATH)" in {
    val t  = new Graphics2DTypesetter(dpi = 72)
    val rf = t.currentFont.renderFont.asInstanceOf[t.RenderFont]

    val head = t.sfntTable(rf, "head")
    head shouldBe defined
    // head's magic number 0x5F0F3CF5 at offset 12 confirms the right bytes came back
    head.get.slice(12, 16).map(_ & 0xff) shouldBe Array(0x5f, 0x0f, 0x3c, 0xf5)
    ByteCursor(head.get, 18).u16 should (be > 0 and be <= 16384) // unitsPerEm is a sane design grid

    t.sfntTable(rf, "MATH") shouldBe None
  }

  "the SFNT directory parser locates tables in a real font file" in {
    val bytes = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get("fonts/NotoSerif/NotoSerif-Regular.ttf"))
    val sfnt  = Sfnt(bytes)

    sfnt.table("head") shouldBe defined
    sfnt.unitsPerEm should (be > 0 and be <= 16384)
    sfnt.table("MATH") shouldBe None
  }
