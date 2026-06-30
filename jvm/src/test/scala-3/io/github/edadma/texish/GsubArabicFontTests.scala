package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{ArabicShaping, GlyphPlacement, Gpos, Gsub, JoiningForm, OtfFont}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The full Arabic shaping pipeline, checked against the bundled Noto Naskh Arabic face: joining-form
  * resolution ([[ArabicShaping]]) feeding GSUB composition and form selection ([[Gsub]]) feeding GPOS mark
  * placement ([[Gpos]]). Like Noto's other Arabic faces, this font decomposes a dotted letter into a
  * dotless skeleton plus a separate dot through `ccmp`, selects the skeleton's connecting shape through the
  * `init`/`medi`/`fina`/`isol` features, and positions the dot with GPOS — so a correct render proves all
  * three stages cooperate. The parsers are pure shared code; this test reads the real font from disk.
  */
class GsubArabicFontTests extends AnyFreeSpec with Matchers:

  import JoiningForm.*

  private val font = new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoNaskhArabic/NotoNaskhArabic-Regular.ttf")))
  private val gsub = Gsub.from(font.tableBytes("GSUB")).get
  private val gpos = Gpos.from(font.tableBytes("GPOS"), font.tableBytes("GDEF"), font.unitsPerEm).get

  private def g(cp: Int): Int = font.glyphIndex(cp)

  /** Shape a word and split the drawn glyphs into bases (letters) and marks (dots/points). */
  private def shape(word: String): (Seq[Int], Seq[Int]) =
    val cps    = word.toArray.map(_.toInt)
    val forms  = ArabicShaping.resolveForms(cps)
    val glyphs = gsub.shape(cps.map(g), forms)
    val places = gpos.position(glyphs)
    val bases  = glyphs.indices.filterNot(i => places(i).isMark).map(glyphs)
    val marks  = glyphs.indices.filter(i => places(i).isMark).map(glyphs)
    (bases, marks)

  private val Beh = 0x0628

  "the font carries Arabic form substitution" in {
    gsub.hasFormSubstitution shouldBe true
  }

  "a dotted letter decomposes into a skeleton and a dot" in {
    // A single beh becomes one base skeleton plus one dot mark.
    val (bases, marks) = shape("ب")
    bases.size shouldBe 1
    marks.size shouldBe 1
  }

  "the three behs of ببب take three distinct connecting skeletons" in {
    val (bases, marks) = shape("ببب")
    bases.size shouldBe 3
    bases.toSet.size shouldBe 3 // initial, medial and final skeletons all differ
    marks.size shouldBe 3       // each beh keeps its dot
  }

  "a two-letter word connects: initial then final, each with its dot" in {
    val (bases, marks) = shape("بب")
    bases.size shouldBe 2
    bases.toSet.size shouldBe 2
    marks.size shouldBe 2
  }

  "the dots attach to their skeletons through GPOS" in {
    val cps    = "ببب".toArray.map(_.toInt)
    val glyphs = gsub.shape(cps.map(g), ArabicShaping.resolveForms(cps))
    val places = gpos.position(glyphs)
    // Every mark in the run attaches to a base (attach index >= 0), i.e. no dot is left floating at the pen.
    places.filter(_.isMark).foreach(_.attach should be >= 0)
  }

  "an isolated letter is shaped to a single base skeleton" in {
    // Alef carries no dot, so it stays one base glyph and adds no mark.
    val (bases, marks) = shape("ا")
    bases.size shouldBe 1
    marks.size shouldBe 0
  }
