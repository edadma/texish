package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{Gpos, Gsub, IndicShaper, OtfFont, Tamil}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The Tamil shaping pipeline, checked against the bundled Noto Serif Tamil face: cluster segmentation
  * ([[Tamil]]) feeding the font's GSUB features ([[IndicShaper]]) feeding GPOS placement ([[Gpos]]). Every
  * expected glyph id and order was confirmed against the same font with `hb-shape --shaper=ot`.
  *
  * Tamil gives this font only three substitution features to work with — `akhn` for its two conjunct
  * ligatures, `abvs`, and `psts`, which both fuses a consonant with its u sign and picks the width-matched
  * form of the i signs. Everything else the script needs is reordering, which is texish's own. The parsers are
  * pure shared code; this test reads the real font.
  */
class GsubTamilFontTests extends AnyFreeSpec with Matchers:

  private val font =
    new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifTamil/NotoSerifTamil-Regular.ttf")))
  private val shaper = IndicShaper.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")).get
  private val gpos   = Gpos.from(font.tableBytes("GPOS"), font.tableBytes("GDEF"), font.unitsPerEm).get

  private def g(cp: Int): Int                 = font.glyphIndex(cp)
  private def shape(word: String): Array[Int] = shaper.shape(word.toArray.map(_.toInt), g)

  private val Ka    = 0x0b95
  private val Ra    = 0x0bb0
  private val Pulli = 0x0bcd

  "the font is recognised as a Tamil shaper" in {
    IndicShaper.from(font.tableBytes("GSUB"), font.tableBytes("GDEF")) shouldBe defined
    shaper.script shouldBe Tamil
    Gsub.fromIndic(font.tableBytes("GSUB"), font.tableBytes("GDEF"), Tamil.scriptTags).get.boundToRequestedScript shouldBe true
  }

  "the Telugu face is not mistaken for a Tamil run, and vice versa" in {
    val telu = new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoSerifTelugu/NotoSerifTelugu-Regular.ttf")))
    IndicShaper.from(telu.tableBytes("GSUB"), telu.tableBytes("GDEF")).get.handles("தமிழ்") shouldBe false
    shaper.handles("தமிழ்") shouldBe true
    shaper.handles("hello") shouldBe false
  }

  "a silenced consonant keeps its pulli rather than forming a half-form" in {
    // க்க stays three glyphs — ka, pulli, ka — where Devanagari would fuse the first two into a half-form.
    shape("க்க").toSeq shouldBe Seq(g(Ka), g(Pulli), g(Ka))
  }

  "the pre-base ee sign moves before its own consonant, stopping at the pulli" in {
    // க்கே sets ka, pulli, ee, ka. The sign belongs to the second ka and does not leap over the first, which
    // is a complete letter — the placement that distinguishes Tamil from Devanagari.
    shape("க்கே").toSeq shouldBe Seq(72, 60, 44, 72)
    shape("கே").toSeq shouldBe Seq(44, 72) // with no pulli in the way it reaches the front
  }

  "a pre-base sign reaches the front when a ligature has swallowed the pulli" in {
    shape("க்ஷே").toSeq shouldBe Seq(44, 74)
  }

  "the three two-part vowel signs split into a pre-base and a post-base part" in {
    shape("கொ").toSeq shouldBe Seq(42, 72, 5)  // ொ o  = e sign, ka, aa sign
    shape("கோ").toSeq shouldBe Seq(44, 72, 5)  // ோ oo = ee sign, ka, aa sign
    shape("கௌ").toSeq shouldBe Seq(42, 72, 17) // ௌ au = e sign, ka, au length mark
    shape("கை").toSeq shouldBe Seq(7, 72)      // ை ai is one sign, and simply moves before the ka
  }

  "the two surviving conjuncts collapse to a single ligature glyph" in {
    shape("க்ஷ").toSeq shouldBe Seq(74)  // ksha, from ka + pulli + ssa
    shape("ஸ்ரீ").toSeq shouldBe Seq(165) // shrii, from sa + pulli + ra + ii sign
  }

  "a word-initial ra with a pulli is not a reph: the ra stays an ordinary letter" in {
    // ர்க — in Devanagari the ra would rise as a mark over the ka; in Tamil it keeps its pulli in place.
    shape("ர்க").toSeq shouldBe Seq(g(Ra), g(Pulli), g(Ka))
  }

  "the u sign fuses into its consonant, and the i sign takes a width-matched form" in {
    // The font's post-base substitutions do both: ய + ு is one drawn glyph, and ி is drawn plain after த but
    // in its narrow alternate after ம and ற.
    shape("யு").toSeq shouldBe Seq(215)
    shape("டு").toSeq shouldBe Seq(185)
    shape("தி").toSeq shouldBe Seq(175, 65)
    shape("மி").toSeq shouldBe Seq(92, 66)
    shape("றி").toSeq shouldBe Seq(151, 66)
  }

  "everyday words shape glyph for glyph as hb-shape does" in {
    // The sequences `hb-shape --shaper=ot` reports for this font, covering the whole Tamil path: silenced
    // consonants with their pulli, the reordered pre-base signs, the fused u signs, the alternate i signs and
    // a word closing on a pulli.
    shape("தமிழ்").toSeq shouldBe Seq(175, 92, 66, 84, 60)
    shape("வணக்கம்").toSeq shouldBe Seq(206, 107, 72, 60, 72, 92, 60)
    shape("நன்றி").toSeq shouldBe Seq(99, 109, 60, 151, 66)
    shape("இந்தியா").toSeq shouldBe Seq(64, 99, 60, 175, 65, 212, 5)
    shape("தமிழ்நாடு").toSeq shouldBe Seq(175, 92, 66, 84, 60, 99, 5, 185)
    shape("பள்ளி").toSeq shouldBe Seq(133, 82, 60, 82, 65)
    shape("தெரியும்").toSeq shouldBe Seq(42, 175, 149, 65, 215, 92, 60)
  }

  "the pulli is attached to its consonant rather than advancing on its own" in {
    val glyphs = shape("ம்")
    glyphs.toSeq shouldBe Seq(92, 60)
    val places = gpos.position(glyphs)
    places.head.attach shouldBe -1 // the ma stands on its own
    places.last.attach shouldBe 0  // the pulli hangs off it
  }

  "the aa sign is a GDEF mark that nonetheless keeps its own width" in {
    // The font classes ா as a mark, but it is a spacing one — hb-shape gives it an advance of 676 and no
    // offset — and no GPOS rule attaches it. Reporting it unattached is what lets CharBox count its width;
    // treating every mark as attached would let the next syllable overlap this one.
    val places = gpos.position(shape("கா"))
    places.last.isMark shouldBe true
    places.last.attach shouldBe -1
    places.last.dxEm shouldBe 0.0
  }
