package io.github.edadma.texish.opentype

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Golden-value tests for the OpenType MATH parser, driven by hand-built tables with known field values
  * so every code path — the constants run, coverage-keyed per-glyph values, and variant constructions
  * with an assembly — is pinned exactly. Big-endian bytes are assembled with [[BE]]. Lengths come back in
  * em (design units ÷ unitsPerEm), so at unitsPerEm 1000 a design value of 300 is 0.3.
  */
class MathTableTests extends AnyFreeSpec with Matchers:

  private val tol = 1e-9

  /** A comprehensive MATH table: full MathConstants (every value record = 300 design units), one italic
    * correction (glyph 5 = 200), and one vertical construction (base glyph 9) with two size variants and
    * a two-part assembly. Offsets are hand-computed and asserted by the section comments. */
  private def sampleMath: Array[Byte] =
    val b = BE()
    // MATH header (10 bytes)
    b.u16(1).u16(0)             // major, minor version
    b.u16(10).u16(224).u16(246) // offsets: constants, glyphInfo, variants
    // MathConstants @10 (214 bytes): 2 i16 + 2 u16 + 51 MathValueRecords + trailing i16
    b.i16(80).i16(60)           // scriptPercentScaleDown, scriptScriptPercentScaleDown
    b.u16(1500).u16(2000)       // delimitedSubFormulaMinHeight, displayOperatorMinHeight
    for _ <- 0 until 51 do b.i16(300).u16(0) // each MathValueRecord: value 300, null device offset
    b.i16(65)                   // radicalDegreeBottomRaisePercent
    // MathGlyphInfo @224 (8-byte header)
    b.u16(8).u16(0).u16(0).u16(0) // italicsOff=8, topAccent/extShape/kern absent
    // MathItalicsCorrectionInfo @232
    b.u16(8).u16(1)             // coverageOff=8 (→240), count=1
    b.i16(200).u16(0)           // one MathValueRecord: 200
    // Coverage @240 (format 1)
    b.u16(1).u16(1).u16(5)      // format 1, one glyph, glyph 5
    // MathVariants @246 (10-byte header)
    b.u16(50)                   // minConnectorOverlap
    b.u16(12).u16(0)            // vertCoverageOff=12 (→258), horizCoverageOff absent
    b.u16(1).u16(0)             // vertGlyphCount=1, horizGlyphCount=0
    b.u16(18)                   // vertGlyphConstruction[0] offset = 18 (→264)
    // vertGlyphCoverage @258 (format 1)
    b.u16(1).u16(1).u16(9)      // format 1, one glyph, base glyph 9
    // MathGlyphConstruction @264
    b.u16(12).u16(2)            // glyphAssemblyOff=12 (→276), variantCount=2
    b.u16(10).u16(1200)         // variant: glyph 10, advance 1200
    b.u16(11).u16(1800)         // variant: glyph 11, advance 1800
    // GlyphAssembly @276
    b.i16(40).u16(0)            // italicsCorrection MathValueRecord: 40
    b.u16(2)                    // partCount=2
    b.u16(12).u16(0).u16(100).u16(600).u16(0)   // part 0: glyph 12, start 0, end 100, full 600, end cap
    b.u16(13).u16(100).u16(0).u16(600).u16(1)   // part 1: glyph 13, start 100, end 0, full 600, extender
    b.result

  "MathConstants parses every field in order and scales to em" in {
    val c = MathTable.parse(sampleMath, 1000).constants

    c.scriptPercentScaleDown shouldBe 0.8 +- tol
    c.scriptScriptPercentScaleDown shouldBe 0.6 +- tol
    c.delimitedSubFormulaMinHeight shouldBe 1.5 +- tol
    c.displayOperatorMinHeight shouldBe 2.0 +- tol
    c.axisHeight shouldBe 0.3 +- tol
    c.fractionRuleThickness shouldBe 0.3 +- tol
    // the last value record and the trailing percent come out right only if exactly 51 records were read
    c.value("radicalKernAfterDegree") shouldBe 0.3 +- tol
    c.radicalDegreeBottomRaisePercent shouldBe 0.65 +- tol
  }

  "an unknown constant name fails loudly rather than returning a silent zero" in {
    val c = MathTable.parse(sampleMath, 1000).constants
    an[RuntimeException] should be thrownBy c.value("notAConstant")
  }

  "MathGlyphInfo keys italic corrections by glyph through the coverage table" in {
    val gi = MathTable.parse(sampleMath, 1000).glyphInfo

    gi.italicsCorrection.keySet shouldBe Set(5)
    gi.italicsCorrection(5) shouldBe 0.2 +- tol
    gi.italicsCorrection.get(6) shouldBe None
  }

  "MathVariants parses size variants and an extensible-glyph assembly" in {
    val v = MathTable.parse(sampleMath, 1000).variants

    v.minConnectorOverlap shouldBe 0.05 +- tol
    v.horizontal shouldBe empty
    v.vertical.keySet shouldBe Set(9)

    val con = v.vertical(9)
    con.variants.map(_.glyph) shouldBe Vector(10, 11)
    con.variants(0).advance shouldBe 1.2 +- tol
    con.variants(1).advance shouldBe 1.8 +- tol

    con.assembly shouldBe defined
    val a = con.assembly.get
    a.italicsCorrection shouldBe 0.04 +- tol
    a.parts.map(_.glyph) shouldBe Vector(12, 13)
    a.parts(0).isExtender shouldBe false
    a.parts(0).endConnector shouldBe 0.1 +- tol
    a.parts(0).fullAdvance shouldBe 0.6 +- tol
    a.parts(1).isExtender shouldBe true
  }

  "a Coverage format-2 (range) table maps glyphs to running indices" in {
    val b = BE()
    b.u16(2).u16(2)        // format 2, two ranges
    b.u16(3).u16(5).u16(0) // glyphs 3..5 → indices 0,1,2
    b.u16(8).u16(9).u16(3) // glyphs 8..9 → indices 3,4

    Coverage.parse(b.result, 0) shouldBe Map(3 -> 0, 4 -> 1, 5 -> 2, 8 -> 3, 9 -> 4)
  }

  "fromFontBytes locates MATH through the SFNT directory and scales by the head unitsPerEm" in {
    val math = sampleMath
    val b    = BE()
    // offset table: TrueType sfntVersion, two tables
    b.u32(0x00010000L).u16(2).u16(0).u16(0).u16(0)
    // table records (16 bytes each); head @44 len 54, MATH @98
    b.tag("head").u32(0).u32(44).u32(54)
    b.tag("MATH").u32(0).u32(98).u32(math.length.toLong)
    // head @44 — only unitsPerEm at offset 18 matters
    b.zeros(18).u16(2048).zeros(34)
    // MATH @98
    b.raw(math)

    val mt = MathTable.fromFontBytes(b.result)
    mt shouldBe defined
    mt.get.unitsPerEm shouldBe 2048
    mt.get.constants.axisHeight shouldBe (300.0 / 2048) +- tol
  }

  "fromFontBytes returns None when the font has no MATH table" in {
    val b = BE()
    b.u32(0x00010000L).u16(1).u16(0).u16(0).u16(0)
    b.tag("head").u32(0).u32(28).u32(54)
    b.zeros(18).u16(1000).zeros(34)

    MathTable.fromFontBytes(b.result) shouldBe None
  }

/** A tiny big-endian byte builder for assembling test fixtures. */
class BE:
  private val buf = scala.collection.mutable.ArrayBuffer.empty[Byte]
  def u8(v: Int): this.type  = { buf += v.toByte; this }
  def u16(v: Int): this.type = { buf += ((v >> 8) & 0xff).toByte; buf += (v & 0xff).toByte; this }
  def i16(v: Int): this.type = u16(v & 0xffff)
  def u32(v: Long): this.type = { u16(((v >> 16) & 0xffff).toInt); u16((v & 0xffff).toInt); this }
  def tag(s: String): this.type = { s.foreach(ch => buf += ch.toByte); this }
  def zeros(n: Int): this.type  = { for _ <- 0 until n do buf += 0.toByte; this }
  def raw(a: Array[Byte]): this.type = { buf ++= a; this }
  def result: Array[Byte] = buf.toArray
