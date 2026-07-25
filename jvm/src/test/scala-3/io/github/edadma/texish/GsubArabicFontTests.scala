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
  private val gsub = Gsub.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")).get
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

  /** Shape a word the way the engine does: the marks of each syllable are put into drawing order first (see
    * [[ArabicShaping.orderMarks]], driven from the reorder units in `Bidi`), then the run is shaped. Returns
    * the glyphs in logical order — `hb-shape` prints a right-to-left run reversed, so its output is compared
    * back to front. */
  private def shapeOrdered(word: String): Seq[Int] =
    val units = scala.collection.mutable.ArrayBuffer.empty[scala.collection.mutable.ArrayBuffer[Int]]
    for i <- word.indices do
      if Bidi.classify(word.charAt(i).toInt) == BidiClass.NSM && units.nonEmpty then units.last += i
      else units += scala.collection.mutable.ArrayBuffer(i)
    for u <- units do ArabicShaping.orderMarks(u, ci => word.charAt(ci).toInt)
    val cps = units.flatten.map(i => word.charAt(i).toInt).toArray
    gsub.shape(cps.map(g), ArabicShaping.resolveForms(cps)).toSeq

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

  "the lam-alef pair takes its contextual rlig forms" in {
    // Lam followed by alef forms the required ligature. This font draws it not as one ligature glyph but as
    // a contextual pair: where the connected lam meets the connected alef, the rlig feature swaps each for a
    // specially shaped variant. So both shaped glyphs differ from their plain initial and final forms.
    val lamInit  = gsub.substituteForm(g(0x0644), Initial)
    val alefFina = gsub.substituteForm(g(0x0627), Final)
    val cps      = "لا".toArray.map(_.toInt)
    val shaped   = gsub.shape(cps.map(g), ArabicShaping.resolveForms(cps))
    shaped.length shouldBe 2 // lam and alef are both dotless: two bases, no marks
    shaped(0) should not equal lamInit
    shaped(1) should not equal alefFina
  }

  "lam-alef in a medial context is replaced by its rlig variant" in {
    // In بلاد the lam joins on both sides, so it is medial; the rlig feature's medial subtable substitutes
    // the medial lam and the final alef. Neither plain form survives in the shaped run.
    val lamMedi  = gsub.substituteForm(g(0x0644), Medial)
    val alefFina = gsub.substituteForm(g(0x0627), Final)
    val cps      = "بلاد".toArray.map(_.toInt)
    val glyphs   = gsub.shape(cps.map(g), ArabicShaping.resolveForms(cps))
    glyphs should not contain lamMedi  // became the medial lam-alef rlig variant
    glyphs should not contain alefFina // became the final alef rlig variant
  }

  "the lam-alef ligature still forms across an intervening vowel mark" in {
    // لَا — lam, fatha, alef. The font's rlig lookup carries IGNORE_MARKS, so the contextual pair must match
    // across the fatha exactly as HarfBuzz does (verified with hb-shape: uni0644.init.rlig, uni064E,
    // uni0627.fina.rlig); the fatha survives in place, attached to the lam half of the ligature by GPOS.
    def shaped(word: String): Array[Int] =
      val cps = word.toArray.map(_.toInt)
      gsub.shape(cps.map(g), ArabicShaping.resolveForms(cps))

    val plain   = shaped("لا")  // the unpointed pair: [lam.rlig, alef.rlig]
    val pointed = shaped("لَا") // the same pair with a fatha between the letters

    pointed.length shouldBe 3
    pointed(0) shouldBe plain(0)  // the lam still takes its .rlig form
    pointed(2) shouldBe plain(1)  // and the alef its .rlig form
    pointed(1) shouldBe g(0x064e) // the fatha stays put between them
  }

  "the pointed word Allah forms its single calligraphic ligature" in {
    // alef, lam, lam, shadda, dagger-alef, heh: ccmp fuses the shadda and dagger into one mark, the lams
    // take their initial and medial forms, and liga substitutes the whole run for the Allah ligature glyph.
    // Six input characters collapsing to one glyph proves composition, form selection and the ligature all
    // cooperate.
    val cps    = Array(0x0627, 0x0644, 0x0644, 0x0651, 0x0670, 0x0647)
    val glyphs = gsub.shape(cps.map(g), ArabicShaping.resolveForms(cps))
    glyphs.length shouldBe 1
  }

  "the unpointed word Allah is left as four connected letters" in {
    // Without the shadda and dagger-alef the ligature's components are not present, so the word stays as the
    // ordinary connected alef-lam-lam-heh — the correct rendering for unpointed text.
    val cps    = Array(0x0627, 0x0644, 0x0644, 0x0647)
    val glyphs = gsub.shape(cps.map(g), ArabicShaping.resolveForms(cps))
    glyphs.length shouldBe 4
  }

  "vocalized words shape glyph for glyph as hb-shape does" in {
    // A syllable carrying both a shadda and a vowel is typed vowel-first and drawn shadda-first, so these
    // depend on the marks being reordered before shaping: without it the shadda and the vowel come out
    // stacked the wrong way up. The unvocalized words alongside them shape the same either way and are here
    // to show the reordering leaves them alone; they run longer than their letter count because this font
    // splits a dotted letter into a skeleton and a separate dot. These are the sequences `hb-shape` reports.
    shapeOrdered("اللَّهِ") shouldBe Seq(8, 70, 68, 374, 380, 81, 436)
    shapeOrdered("مُحَمَّدٌ") shouldBe Seq(77, 381, 24, 380, 76, 374, 380, 27, 385)
    shapeOrdered("بِسْمِ") shouldBe Seq(19, 323, 436, 34, 388, 75, 436)
    shapeOrdered("الْعَالَمِينَ") shouldBe Seq(8, 70, 388, 46, 380, 9, 70, 380, 76, 436, 18, 325, 79, 288, 380)
    shapeOrdered("كِتَابٌ") shouldBe Seq(59, 436, 18, 294, 380, 9, 14, 323, 385)
    shapeOrdered("كتاب") shouldBe Seq(59, 18, 294, 9, 14, 323)
    shapeOrdered("السلام") shouldBe Seq(8, 70, 34, 69, 11, 72)
  }

  "the shadda is placed on the letter and the vowel above it, however the two are typed" in {
    // The doubled lam of the word above, spelled out. Canonical order puts the fatha first, its combining
    // class being the lower; a writer may equally type the shadda first, the order it is drawn in. Both must
    // give the same run — the shadda nearest the letter, the fatha stacked above it.
    def word(marks: Int*) = (Seq(0x0627, 0x0644, 0x0644) ++ marks ++ Seq(0x0647, 0x0650)).map(_.toChar).mkString
    val canonical = shapeOrdered(word(0x064e, 0x0651)) // fatha then shadda, as normalized text arrives
    val asDrawn   = shapeOrdered(word(0x0651, 0x064e)) // shadda then fatha, as a writer may type it
    canonical shouldBe asDrawn
    canonical.slice(3, 5) shouldBe Seq(374, 380) // the shadda, then the fatha above it
    g(0x0651) shouldBe 374                       // …which is indeed the shadda
    g(0x064e) shouldBe 380                       // …and the fatha
  }

