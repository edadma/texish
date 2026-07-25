package io.github.edadma.texish.opentype

/** The Indic-shaping front end for the Gurmukhi script (Punjabi as written in India): it supplies the
  * character knowledge the shared [[IndicScript]] segmentation and reordering build on. Gurmukhi is written
  * left to right, so like Devanagari and Bengali it needs no bidirectional reordering, only the cluster-scoped
  * work of an Indic script.
  *
  * Gurmukhi asks less of the shaper than the others. Just one vowel sign reorders before the base — the sihari
  * (i-sign) — and there are no two-part vowel signs and no word-position init/fina forms. Its distinguishing
  * feature is that it has no reph: a word-initial ra with a virama does not rise as a mark over the syllable
  * the way it does in Devanagari or Bengali. Instead ra, va, ha and ya take subjoined below-base forms (the
  * pairin), selected by the font's below-base `blwf` feature exactly as a conjunct is elsewhere. So this script
  * turns the reph detection off and leans on the shared below-base and conjunct features. The gemination sign
  * addak and the nasal signs tippi and bindi bind to the cluster after the base as syllable modifiers. */
object Gurmukhi extends IndicScript:

  import IndicCategory.*

  /** The OpenType Indic script tags, the modern `gur2` preferred over the legacy `guru`. */
  def scriptTags: Seq[String] = Seq("gur2", "guru")

  /** Whether a run contains Gurmukhi letters or signs worth shaping (a digit alone does not need the Indic
    * path). A named alias for [[IndicScript.has]]. */
  def hasGurmukhi(text: String): Boolean = has(text)

  /** The category of a codepoint in the Gurmukhi block (U+0A00–U+0A7F). Characters outside the block, and
    * block members that play no part in cluster shaping, are [[IndicCategory.Other]]. */
  def category(cp: Int): IndicCategory =
    cp match
      case 0x0A4D                          => Virama
      case 0x0A3C                          => Nukta
      case 0x0A3F                          => PreBaseMatra    // sihari (i-sign) — drawn before the base
      case 0x0A01 | 0x0A02 | 0x0A03        => SyllableModifier // adak bindi, bindi, visarga
      case 0x0A70 | 0x0A71                 => SyllableModifier // tippi (nasal), addak (gemination)
      case c if c >= 0x0A15 && c <= 0x0A28 => Consonant        // ਕ ka … ਨ na
      case c if c >= 0x0A2A && c <= 0x0A30 => Consonant        // ਪ pa … ਰ ra
      case 0x0A32 | 0x0A33                 => Consonant        // ਲ la, ਲ਼ lla
      case 0x0A35 | 0x0A36                 => Consonant        // ਵ va, ਸ਼ sha
      case 0x0A38 | 0x0A39                 => Consonant        // ਸ sa, ਹ ha
      case c if c >= 0x0A59 && c <= 0x0A5E => Consonant        // nukta-composed khha, ghha, za, rra, fa
      case 0x0A72 | 0x0A73                 => IndependentVowel // iri, ura — the vowel bearers
      case c if c >= 0x0A05 && c <= 0x0A0A => IndependentVowel // a, aa, i, ii, u, uu
      case 0x0A0F | 0x0A10                 => IndependentVowel // ee, ai
      case 0x0A13 | 0x0A14                 => IndependentVowel // oo, au
      case c if isDependentSign(c)         => Matra
      case _                               => Other

  // The dependent vowel signs other than the pre-base sihari: aa and bihari after the base, the below-base
  // aunkar/dulankar (u/uu), the above-base lavan/dulavan (ee/ai), the hora/kanaura (oo/au), and the below-base
  // yakash. They follow the base in logical order and are positioned by the font.
  private def isDependentSign(cp: Int): Boolean =
    cp == 0x0A3E ||                     // aa
      cp == 0x0A40 ||                   // bihari (ii)
      (cp >= 0x0A41 && cp <= 0x0A42) || // aunkar u, dulankar uu (below-base)
      (cp >= 0x0A47 && cp <= 0x0A48) || // lavan ee, dulavan ai (above-base)
      (cp >= 0x0A4B && cp <= 0x0A4C) || // hora oo, kanaura au
      cp == 0x0A75                      // yakash (subjoined ya sign)

  /** The sihari, the i-sign — the only Gurmukhi dependent sign written after its consonant in memory but drawn
    * before it, and so the only one that reorders to the front of its cluster. */
  val Sihari: Int = 0x0A3F

  def preBaseMatras: Set[Int] = Set(Sihari)

  /** The consonant ra and the virama. In Gurmukhi these do not make a reph — a subjoined ra is a below-base
    * form (see [[startsWithReph]]) — but the shared trait still needs them named. `Halant` names the virama
    * codepoint to avoid clashing with the [[IndicCategory.Virama]] enum case. */
  val Ra: Int     = 0x0A30
  val Halant: Int = 0x0A4D

  def ra: Int     = Ra
  def halant: Int = Halant

  // The same basic and presentation features as the other Indic scripts. Gurmukhi's subjoined pairin forms
  // (ra, va, ha, ya below the base) are selected by the below-base `blwf` feature, so no extra feature is
  // needed; the ones a given cluster has no use for do nothing.
  def basicFeatures: Seq[String] = Seq("nukt", "akhn", "rkrf", "blwf", "half", "vatu", "cjct")

  def presFeatures: Seq[String] = Seq("pres", "abvs", "blws", "psts", "haln", "calt")

  /** Gurmukhi has no reph: a word-initial ra with a virama takes a subjoined below-base form rather than rising
    * as a mark over the syllable, so ra+virama is never lifted out of its cluster. This overrides the shared
    * detection, which would otherwise treat a cluster-initial ra+virama+consonant as a reph. */
  override def startsWithReph(clusterCps: Array[Int]): Boolean = false
