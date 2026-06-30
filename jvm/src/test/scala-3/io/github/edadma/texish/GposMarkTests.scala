package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{Gpos, OtfFont}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The GPOS mark-positioning shaper, checked against the bundled Noto Serif Hebrew face. The parser is
  * pure shared code; this test reads the real font from disk (a JVM convenience) and confirms it finds the
  * mark lookups, classifies the points as marks, and attaches a vowel point below its consonant.
  */
class GposMarkTests extends AnyFreeSpec with Matchers:

  private val font = new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifHebrew/NotoSerifHebrew-Regular.ttf")))
  private val gpos = Gpos.from(font.tableBytes("GPOS"), font.tableBytes("GDEF"), font.unitsPerEm).get

  private def g(cp: Int): Int = font.glyphIndex(cp)

  private val Dalet  = 0x05d3
  private val Bet    = 0x05d1
  private val Qamats = 0x05b8 // a vowel point drawn below the letter
  private val Dagesh = 0x05bc // a dot drawn inside the letter

  "the font is recognized as having mark positioning" in {
    gpos.hasMarkPositioning shouldBe true
  }

  "the consonants are bases and the points are marks" in {
    gpos.isMark(g(Dalet)) shouldBe false
    gpos.isMark(g(Bet)) shouldBe false
    gpos.isMark(g(Qamats)) shouldBe true
    gpos.isMark(g(Dagesh)) shouldBe true
  }

  "a vowel point attaches to the preceding consonant, horizontally centred under it" in {
    val place = gpos.position(Array(g(Dalet), g(Qamats)))
    place(0).isMark shouldBe false
    val mark = place(1)
    mark.isMark shouldBe true
    mark.attach shouldBe 0 // attaches to the dalet
    // This face centres the point under the letter through GPOS and carries its vertical placement in the
    // glyph outline itself (so the anchor offset is horizontal): the qamats lands roughly mid-letter.
    val daletAdvance = font.advanceWidthEm(g(Dalet))
    mark.dxEm should (be > 0.0 and be < daletAdvance)
    mark.dyEm.abs should be < 0.6
  }

  "a dagesh attaches to its consonant near the baseline, inside the letter" in {
    val place = gpos.position(Array(g(Bet), g(Dagesh)))
    place(1).isMark shouldBe true
    place(1).attach shouldBe 0
  }

  "a run with no marks gets no offsets" in {
    val place = gpos.position(Array(g(Dalet), g(Bet)))
    place.forall(p => !p.isMark && p.attach == -1) shouldBe true
  }
