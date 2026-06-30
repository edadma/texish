package io.github.edadma.texish.opentype

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Golden-value tests for the GSUB Arabic-form shaper, driven by a hand-built table with known glyph maps
  * so every path — script/feature selection, single substitution in both formats, and an extension lookup
  * — is pinned exactly. All offsets are hand-computed and noted in the comments; [[BE]] assembles the
  * big-endian bytes.
  *
  * The fixture defines one Arabic script whose default language system enables four features, one per
  * contextual form, each driving a single-substitution lookup:
  *   - isol: glyph 10 → 100         (format 2, explicit substitute list)
  *   - init: glyphs 10,11 → +10     (format 1, delta)
  *   - medi: glyph 11 → 211         (format 2)
  *   - fina: glyph 12 → +100        (format 1, wrapped in a type-7 extension)
  */
class GsubTests extends AnyFreeSpec with Matchers:

  import JoiningForm.*

  private def sampleGsub: Array[Byte] =
    val b = BE()

    // ── GSUB header @0 (10 bytes) ──
    b.u16(1).u16(0)        // major, minor version
    b.u16(10)              // scriptList offset
    b.u16(36)              // featureList offset
    b.u16(86)              // lookupList offset

    // ── ScriptList @10 (26 bytes) ──
    b.u16(1)               // scriptCount
    b.tag("arab").u16(8)   // scriptRecord: tag, offset (to Script table, rel to scriptList = 8)
    // Script table @ scriptList+8 (= 18)
    b.u16(4)               // defaultLangSys offset (rel to Script table)
    b.u16(0)               // langSysCount
    // LangSys @ Script+4
    b.u16(0)               // lookupOrder (reserved)
    b.u16(0xffff)          // requiredFeatureIndex (none)
    b.u16(4)               // featureIndexCount
    b.u16(0).u16(1).u16(2).u16(3) // featureIndices

    // ── FeatureList @36 (50 bytes) ──
    b.u16(4)               // featureCount
    b.tag("isol").u16(26)  // featureRecord 0 → Feature table at FL+26
    b.tag("init").u16(32)  // 1 → FL+32
    b.tag("medi").u16(38)  // 2 → FL+38
    b.tag("fina").u16(44)  // 3 → FL+44
    // Feature tables (6 bytes each): featureParams=0, lookupIndexCount=1, lookupListIndices[1]
    b.u16(0).u16(1).u16(0) // isol → lookup 0
    b.u16(0).u16(1).u16(1) // init → lookup 1
    b.u16(0).u16(1).u16(2) // medi → lookup 2
    b.u16(0).u16(1).u16(3) // fina → lookup 3

    // ── LookupList @86 ──
    b.u16(4)               // lookupCount
    b.u16(10).u16(32).u16(54).u16(76) // lookup offsets (rel to lookupList)

    // Lookup 0 @ LL+10 — type 1, single subst format 2: {10 → 100}
    b.u16(1).u16(0).u16(1).u16(8)     // type, flag, subCount, subtableOffset(rel lookup)
    b.u16(2).u16(8).u16(1).u16(100)   // SingleSubstFormat2: format, covOffset, glyphCount, subst[0]
    b.u16(1).u16(1).u16(10)           // Coverage format 1: format, count, {10}

    // Lookup 1 @ LL+32 — type 1, single subst format 1: {10,11} delta +10
    b.u16(1).u16(0).u16(1).u16(8)
    b.u16(1).u16(6).i16(10)           // SingleSubstFormat1: format, covOffset, deltaGlyphID
    b.u16(1).u16(2).u16(10).u16(11)   // Coverage format 1: format, count, {10,11}

    // Lookup 2 @ LL+54 — type 1, single subst format 2: {11 → 211}
    b.u16(1).u16(0).u16(1).u16(8)
    b.u16(2).u16(8).u16(1).u16(211)
    b.u16(1).u16(1).u16(11)

    // Lookup 3 @ LL+76 — type 7 extension wrapping single subst format 1: {12} delta +100
    b.u16(7).u16(0).u16(1).u16(8)     // lookup header (extension)
    b.u16(1).u16(1).u32(8)            // ExtensionSubst: format, extensionLookupType(1), extensionOffset
    b.u16(1).u16(6).i16(100)          // wrapped SingleSubstFormat1: format, covOffset, delta
    b.u16(1).u16(1).u16(12)           // Coverage: {12}

    b.result

  private val gsub = Gsub.from(Some(sampleGsub)).getOrElse(fail("expected a shaper from the GSUB table"))

  "the font reports Arabic form substitution" in {
    gsub.hasFormSubstitution shouldBe true
  }

  "isolated form via format-2 single substitution" in {
    gsub.substituteForm(10, Isolated) shouldBe 100
  }

  "initial form via format-1 delta over a two-glyph coverage" in {
    gsub.substituteForm(10, Initial) shouldBe 20
    gsub.substituteForm(11, Initial) shouldBe 21
  }

  "medial form via format-2 single substitution" in {
    gsub.substituteForm(11, Medial) shouldBe 211
  }

  "final form reached through a type-7 extension lookup" in {
    gsub.substituteForm(12, Final) shouldBe 112
  }

  "a glyph the feature does not cover is left unchanged" in {
    gsub.substituteForm(99, Initial) shouldBe 99 // not in init coverage
    gsub.substituteForm(10, Final) shouldBe 10   // fina covers only glyph 12
  }

  "a font without GSUB yields no shaper" in {
    Gsub.from(None) shouldBe None
  }

  /** A second fixture exercising the composition path: an Arabic script whose default language system enables
    * `ccmp` (which decomposes glyph 10 into a skeleton 50 and a dot 60, via a multiple substitution) and
    * `init` (which maps the skeleton 50 to its initial form 150, built as a one-element multiple
    * substitution, the way Noto's Arabic faces do). Shaping glyph 10 in its initial form should yield the
    * skeleton's initial glyph followed by the untouched dot. */
  private def sampleGsubDecomp: Array[Byte] =
    val b = BE()
    // header @0
    b.u16(1).u16(0).u16(10).u16(32).u16(58) // version, scriptList, featureList, lookupList

    // ScriptList @10 (22 bytes)
    b.u16(1).tag("arab").u16(8)         // one script, table at +8
    b.u16(4).u16(0)                     // Script: defaultLangSys at +4, no named langSys
    b.u16(0).u16(0xffff).u16(2).u16(0).u16(1) // LangSys: lookupOrder, required(none), 2 features {0,1}

    // FeatureList @32 (26 bytes)
    b.u16(2)                            // featureCount
    b.tag("ccmp").u16(14)               // → feature table at FL+14
    b.tag("init").u16(20)               // → FL+20
    b.u16(0).u16(1).u16(0)              // ccmp → lookup 0
    b.u16(0).u16(1).u16(1)              // init → lookup 1

    // LookupList @58
    b.u16(2).u16(6).u16(34)             // lookupCount, offsets (rel lookupList)

    // Lookup 0 @ LL+6 — type 2 multiple subst: 10 → [50, 60]
    b.u16(2).u16(0).u16(1).u16(8)       // type, flag, subCount, subtableOffset
    b.u16(1).u16(14).u16(1).u16(8)      // MultipleSubstFormat1: format, covOffset, seqCount, seqOffset
    b.u16(2).u16(50).u16(60)            // Sequence @ subtable+8: glyphCount, {50,60}
    b.u16(1).u16(1).u16(10)            // Coverage @ subtable+14: {10}

    // Lookup 1 @ LL+34 — type 2 multiple subst (one output): 50 → [150]
    b.u16(2).u16(0).u16(1).u16(8)
    b.u16(1).u16(12).u16(1).u16(8)      // format, covOffset, seqCount, seqOffset
    b.u16(1).u16(150)                   // Sequence @ subtable+8: glyphCount=1, {150}
    b.u16(1).u16(1).u16(50)            // Coverage @ subtable+12: {50}

    b.result

  "ccmp decomposition runs before form selection, in a buffer shape" in {
    val g = Gsub.from(Some(sampleGsubDecomp)).getOrElse(fail("expected a shaper"))
    // Glyph 10 in its initial form: ccmp splits it to skeleton 50 + dot 60, then init maps 50 → 150; the
    // dot 60 is covered by no feature and passes through.
    g.shape(Array(10), Array(Initial)) shouldBe Array(150, 60)
    // With no joining context a lone letter is isolated; the isol feature is absent here, so the skeleton
    // keeps its nominal glyph while the dot still splits off.
    g.shape(Array(10), Array(Isolated)) shouldBe Array(50, 60)
  }

  /** A third fixture for contextual substitution — the way this font's `rlig` forms the lam-alef pair. An
    * `isol` feature (covering an unrelated glyph) makes the table report form substitution so a shaper is
    * built; an `rlig` feature drives a ContextSubstFormat3 lookup that matches the glyph pair 10,11 and runs
    * a single substitution at each position: 10 → 110 (record at sequence 0) and 11 → 111 (record at
    * sequence 1). All offsets are hand-computed and noted. */
  private def sampleGsubContext: Array[Byte] =
    val b = BE()
    b.u16(1).u16(0).u16(10).u16(32).u16(58)          // header: version, scriptList, featureList, lookupList
    b.u16(1).tag("arab").u16(8)                      // ScriptList @10: one 'arab' script, table at +8
    b.u16(4).u16(0)                                  // Script @18: defaultLangSys at +4, no named langSys
    b.u16(0).u16(0xffff).u16(2).u16(0).u16(1)        // LangSys @22: lookupOrder, required(none), features {0,1}
    b.u16(2).tag("isol").u16(14).tag("rlig").u16(20) // FeatureList @32: isol → FL+14, rlig → FL+20
    b.u16(0).u16(1).u16(0)                           // Feature isol @46 → lookup 0
    b.u16(0).u16(1).u16(1)                           // Feature rlig @52 → lookup 1
    b.u16(4).u16(10).u16(32).u16(70).u16(92)         // LookupList @58: four lookups at these offsets
    // Lookup 0 @68 — type 1 single, glyph 99 → 199, present only so the table reports a form feature
    b.u16(1).u16(0).u16(1).u16(8)
    b.u16(2).u16(8).u16(1).u16(199)                  // SingleSubstFormat2 @76
    b.u16(1).u16(1).u16(99)                          // Coverage @84
    // Lookup 1 @90 — type 5 ContextSubstFormat3: input [10][11], records (seq 0 → lookup 2)(seq 1 → lookup 3)
    b.u16(5).u16(0).u16(1).u16(8)
    b.u16(3).u16(2).u16(2).u16(18).u16(24).u16(0).u16(2).u16(1).u16(3) // @98: format, glyphCount, substCount, covOffs, records
    b.u16(1).u16(1).u16(10)                          // input coverage 0 @116
    b.u16(1).u16(1).u16(11)                          // input coverage 1 @122
    // Lookup 2 @128 — type 1 single, 10 → 110
    b.u16(1).u16(0).u16(1).u16(8)
    b.u16(2).u16(8).u16(1).u16(110)
    b.u16(1).u16(1).u16(10)
    // Lookup 3 @150 — type 1 single, 11 → 111
    b.u16(1).u16(0).u16(1).u16(8)
    b.u16(2).u16(8).u16(1).u16(111)
    b.u16(1).u16(1).u16(11)
    b.result

  "rlig contextual substitution swaps a matched pair for their variant glyphs" in {
    val g = Gsub.from(Some(sampleGsubContext)).getOrElse(fail("expected a shaper"))
    // 10 and 11 are outside the form feature's coverage, so form selection leaves them; rlig then matches
    // the pair and substitutes each through its nested single substitution.
    g.shape(Array(10, 11), Array(Isolated, Isolated)) shouldBe Array(110, 111)
  }

  "rlig leaves a pair it does not match untouched" in {
    val g = Gsub.from(Some(sampleGsubContext)).getOrElse(fail("expected a shaper"))
    g.shape(Array(10, 77), Array(Isolated, Isolated)) shouldBe Array(10, 77)
  }

  /** A fourth fixture for chaining-context substitution: an `rlig` feature whose ChainContextSubstFormat3
    * lookup substitutes 11 → 211 only when 11 is immediately preceded by the backtrack glyph 10. */
  private def sampleGsubChain: Array[Byte] =
    val b = BE()
    b.u16(1).u16(0).u16(10).u16(32).u16(58)          // header
    b.u16(1).tag("arab").u16(8)                      // ScriptList @10
    b.u16(4).u16(0)                                  // Script @18
    b.u16(0).u16(0xffff).u16(2).u16(0).u16(1)        // LangSys @22, features {0,1}
    b.u16(2).tag("isol").u16(14).tag("rlig").u16(20) // FeatureList @32
    b.u16(0).u16(1).u16(0)                           // isol → lookup 0
    b.u16(0).u16(1).u16(1)                           // rlig → lookup 1
    b.u16(3).u16(8).u16(30).u16(68)                  // LookupList @58: three lookups
    // Lookup 0 @66 — isol single 99 → 199
    b.u16(1).u16(0).u16(1).u16(8)
    b.u16(2).u16(8).u16(1).u16(199)
    b.u16(1).u16(1).u16(99)
    // Lookup 1 @88 — type 6 ChainContextSubstFormat3: backtrack [10], input [11], record (seq 0 → lookup 2)
    b.u16(6).u16(0).u16(1).u16(8)
    b.u16(3).u16(1).u16(18).u16(1).u16(24).u16(0).u16(1).u16(0).u16(2) // @96: fmt, btCnt, btOff, inCnt, inOff, laCnt, substCnt, record
    b.u16(1).u16(1).u16(10)                          // backtrack coverage @114
    b.u16(1).u16(1).u16(11)                          // input coverage @120
    // Lookup 2 @126 — single 11 → 211
    b.u16(1).u16(0).u16(1).u16(8)
    b.u16(2).u16(8).u16(1).u16(211)
    b.u16(1).u16(1).u16(11)
    b.result

  "rlig chaining substitution fires only with the required backtrack glyph" in {
    val g = Gsub.from(Some(sampleGsubChain)).getOrElse(fail("expected a shaper"))
    g.shape(Array(10, 11), Array(Isolated, Isolated)) shouldBe Array(10, 211) // 11 preceded by 10
    g.shape(Array(12, 11), Array(Isolated, Isolated)) shouldBe Array(12, 11)  // backtrack absent
  }
