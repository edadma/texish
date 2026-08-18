package io.github.edadma.texish.opentype

/** The Indic-shaping front end for Gujarati: it supplies the character knowledge — which codepoints are
  * consonants, vowels, viramas and vowel signs — that the shared [[IndicScript]] segmentation and reordering
  * build on. Gujarati is written left to right, so unlike Hebrew and Arabic it needs no bidirectional
  * reordering; what it needs is the cluster-scoped glyph reordering [[IndicScript]] provides.
  *
  * Gujarati is Devanagari's closest relative among the scripts texish sets — the same consonant-and-virama
  * model, the same half-forms and conjuncts, the same reph above the syllable — written without the
  * shirorekha, the headline that joins Devanagari's letters. That likeness runs all the way through the
  * shaping: the base is the last consonant of the cluster, the only dependent sign that reorders to the front
  * is the i matra, and there are no two-part matras and no word-position init/fina forms, so those
  * [[IndicScript]] hooks stay at their defaults. The o and au signs are drawn as an aa sign with a mark above
  * it, but each is a single codepoint the font maps on its own, so nothing decomposes.
  *
  * The feature lists are the OpenType Gujarati ones, which are the Devanagari lists exactly. A font that
  * implements only part of them is the normal case rather than a defect — a feature with no lookups leaves the
  * run untouched — and the bundled Noto Serif Gujarati carries `akhn`, `rphf`, `rkrf`, `blwf`, `half` and
  * `cjct` of the basic set. */
object Gujarati extends IndicScript:

  /** The broad category of a Gujarati character. A re-export of the shared [[IndicCategory]] so callers can
    * name the cases as `Gujarati.Category.*`. */
  type Category = IndicCategory
  val Category: IndicCategory.type = IndicCategory

  import IndicCategory.*

  /** The OpenType Indic script tags, the modern `gjr2` preferred over the legacy `gujr`. */
  def scriptTags: Seq[String] = Seq("gjr2", "gujr")

  /** Whether a run contains Gujarati letters or signs worth shaping (a digit or rupee sign alone does not need
    * the Indic path). A named alias for [[IndicScript.has]]. */
  def hasGujarati(text: String): Boolean = has(text)

  /** The category of a codepoint in the Gujarati block (U+0A80–U+0AFF). Characters outside the block, and block
    * members that play no part in cluster shaping — the digits, the abbreviation and rupee signs, and the
    * standalone avagraha — are [[IndicCategory.Other]]. */
  def category(cp: Int): IndicCategory =
    cp match
      case 0x0ACD                          => Virama
      case 0x0ABC                          => Nukta
      case 0x0ABF                          => PreBaseMatra
      case 0x0A81 | 0x0A82 | 0x0A83        => SyllableModifier // candrabindu, anusvara, visarga
      case c if c >= 0x0AFA && c <= 0x0AFF => SyllableModifier // sukun, shadda, maddah, the nukta-above signs
      case c if c >= 0x0A95 && c <= 0x0AB9 => Consonant        // ક … હ (0AA9, 0AB1 and 0AB4 are unassigned)
      case 0x0AF9                          => Consonant        // ZHA, a letter for a borrowed sound
      case 0x0AD0                          => Consonant        // OM behaves as a letter
      case c if c >= 0x0A85 && c <= 0x0A94 => IndependentVowel // A … AU (0A8E and 0A92 are unassigned)
      case c if c >= 0x0AE0 && c <= 0x0AE1 => IndependentVowel // vocalic RR, LL
      case c if isDependentSign(c)         => Matra
      case _                               => Other

  // The dependent vowel signs other than the pre-base i: they follow the base in logical order and are
  // positioned above, below or after it by the font.
  private def isDependentSign(cp: Int): Boolean =
    cp == 0x0ABE ||                     // AA
      (cp >= 0x0AC0 && cp <= 0x0AC5) || // II, U, UU, vocalic R, vocalic RR, candra E (short i excluded above)
      (cp >= 0x0AC7 && cp <= 0x0AC9) || // E, AI, candra O
      (cp >= 0x0ACB && cp <= 0x0ACC) || // O, AU
      (cp >= 0x0AE2 && cp <= 0x0AE3)    // vocalic L, LL signs

  /** The i vowel sign, the one dependent sign written after its consonant in memory but drawn before it. It is
    * the only Gujarati dependent sign that reorders to the front of its cluster. */
  val ISign: Int = 0x0ABF

  def preBaseMatras: Set[Int] = Set(ISign)

  /** The consonant ra and the virama — together, at the head of a syllable, they form a reph. `Halant` names
    * the virama codepoint to avoid clashing with the [[IndicCategory.Virama]] enum case. */
  val Ra: Int     = 0x0AB0
  val Halant: Int = 0x0ACD

  def ra: Int     = Ra
  def halant: Int = Halant

  // The OpenType Gujarati basic-form features, which are Devanagari's: nukta composition, the akhand
  // ligatures, the rakaar and below-base forms, the half- and post-base forms, the vattu variant and the
  // general conjunct. `rphf` is not listed — it must fire only on a cluster-initial ra, so IndicShaper applies
  // it on its own.
  def basicFeatures: Seq[String] = Seq("nukt", "akhn", "rkrf", "blwf", "half", "vatu", "cjct")

  def presFeatures: Seq[String] = Seq("pres", "abvs", "blws", "psts", "haln", "calt")
