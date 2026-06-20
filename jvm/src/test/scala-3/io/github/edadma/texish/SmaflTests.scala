package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{OtfFont, Smafl, SfntWriter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** SMaFL — assigning private-use codepoints to a math font's MATH-table size-variant and assembly glyphs so
  * they become addressable as text. These drive the real Latin Modern Math font end to end: convert it, then
  * re-parse the output with the same pure-Scala SFNT reader the renderer uses, proving the rebuilt `cmap` and
  * repackaged font are valid and that the new codepoints resolve to exactly the glyphs the MATH table named —
  * while the original outlines are left untouched.
  */
class SmaflTests extends AnyFreeSpec with Matchers:

  private val mathBytes = Files.readAllBytes(Paths.get("fonts/LatinModernMath/LatinModernMath-Regular.otf"))
  private val original  = new OtfFont(mathBytes)
  private val result    = Smafl.convert(mathBytes).getOrElse(fail("Latin Modern Math has a MATH table"))
  private val converted = new OtfFont(result.font)

  "the converter assigns private-use codepoints to the variant and assembly glyphs" in {
    result.entries should not be empty
    result.entries.map(_.pua).distinct.length shouldBe result.entries.length // no codepoint assigned twice
    result.entries.map(_.glyph).distinct.length shouldBe result.entries.length // no glyph assigned twice
    all(result.entries.map(_.pua)) should (be >= Smafl.PuaBase and be <= 0xf8ff)
  }

  "the assigned codepoints are exactly the MATH glyphs that had none before" in {
    for e <- result.entries do
      withClue(s"glyph ${e.glyph} (assigned ${e.pua}): ") {
        original.codepointForGlyph(e.glyph) shouldBe None // genuinely had no codepoint in the original
      }
  }

  "the rebuilt font parses, and each private-use codepoint resolves to its variant/assembly glyph" in {
    converted.unitsPerEm shouldBe original.unitsPerEm
    converted.numGlyphs shouldBe original.numGlyphs
    converted.isCff shouldBe true
    for e <- result.entries do
      withClue(s"U+${e.pua.toHexString} → glyph ${e.glyph}: ") {
        converted.glyphIndex(e.pua) shouldBe e.glyph
      }
  }

  "the extended cmap keeps every original mapping" in {
    // The base symbols the renderer already drew through fillText must still resolve after the rewrite.
    for cp <- Seq('x'.toInt, 'a'.toInt, '2'.toInt, '='.toInt, 0x221a /* √ */, 0x00b1 /* ± */, 0x2211 /* ∑ */) do
      withClue(s"codepoint U+${cp.toHexString}: ") {
        converted.glyphIndex(cp) shouldBe original.glyphIndex(cp)
        converted.glyphIndex(cp) should be > 0
      }
  }

  "a size variant of the radical now has a codepoint, the case SMaFL exists to fix" in {
    // The base surd √ (U+221A) grows to a taller precomposed variant for \sqrt{b^2-4ac}; that variant has no
    // Unicode codepoint of its own, so the canvas backend had to fill its outline. After conversion it carries
    // a private-use codepoint and so resolves back through codepointForGlyph — the hinted text path.
    val baseSurd = converted.glyphIndex(0x221a)
    val radical  = result.entries.find(e => e.base == 0x221a && e.kind == "variant")
    radical.isDefined shouldBe true
    radical.get.glyph should not be baseSurd
    converted.glyphIndex(radical.get.pua) shouldBe radical.get.glyph
  }

  "an outline is unchanged by the repackaging" in {
    // Only the cmap changed; the CFF glyph program for a sample letter must be byte-for-byte identical.
    val gid = original.glyphIndex('x'.toInt)
    converted.outline(gid) shouldBe original.outline(gid)
  }

  "rebuild recomputes a valid head.checkSumAdjustment" in {
    // The whole-file checksum plus checkSumAdjustment must sum to the OpenType magic 0xB1B0AFBA.
    val head = converted.table("head").getOrElse(fail("head table present"))
    def u32at(a: Array[Byte], i: Int): Long =
      ((a(i) & 0xffL) << 24) | ((a(i + 1) & 0xffL) << 16) | ((a(i + 2) & 0xffL) << 8) | (a(i + 3) & 0xffL)
    val adjustment = u32at(result.font, head.offset + 8)
    // Recompute the file checksum with checkSumAdjustment zeroed, the state the magic identity holds in.
    val probe = result.font.clone()
    for i <- 0 until 4 do probe(head.offset + 8 + i) = 0
    var sum = 0L
    var i   = 0
    while i < probe.length do { sum = (sum + u32at(probe, i)) & 0xffffffffL; i += 4 }
    ((sum + adjustment) & 0xffffffffL) shouldBe 0xb1b0afbaL
  }

  "cmapFormat12 coalesces contiguous runs and round-trips through the reader" in {
    // A direct unit check on the writer: a contiguous codepoint→glyph run and a gap, read back via Cmap.
    val cmap = SfntWriter.cmapFormat12(Seq(0x41 -> 10, 0x42 -> 11, 0x43 -> 12, 0xe000 -> 500))
    val font = io.github.edadma.texish.opentype.Cmap.parse(cmap, Some(io.github.edadma.texish.opentype.TableRecord("cmap", 0, cmap.length)))
    font.glyphIndex(0x41) shouldBe 10
    font.glyphIndex(0x43) shouldBe 12
    font.glyphIndex(0xe000) shouldBe 500
    font.glyphIndex(0x44) shouldBe 0 // the gap is not covered
  }
