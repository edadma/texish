package io.github.edadma.texish.opentype

/** The Indic-shaping front end for Devanagari: it supplies the character knowledge — which codepoints are
  * consonants, vowels, viramas and vowel signs — that the shared [[IndicScript]] segmentation and reordering
  * build on. Devanagari is written left to right, so unlike Hebrew and Arabic it needs no bidirectional
  * reordering; what it needs is the cluster-scoped glyph reordering [[IndicScript]] provides.
  *
  * The base is the consonant a vowel sign attaches to — for Hindi the last consonant of the cluster. The only
  * dependent sign that reorders to the front is the short-i matra; Devanagari has no two-part matras and no
  * word-position init/fina forms, so it leaves those [[IndicScript]] hooks at their defaults. */
object Devanagari extends IndicScript:

  /** The broad category of a Devanagari character. A re-export of the shared [[IndicCategory]] so callers can
    * name the cases as `Devanagari.Category.*`. */
  type Category = IndicCategory
  val Category: IndicCategory.type = IndicCategory

  import IndicCategory.*

  /** The OpenType Indic script tags, the modern `dev2` preferred over the legacy `deva`. */
  def scriptTags: Seq[String] = Seq("dev2", "deva")

  /** Whether a run contains Devanagari letters or signs worth shaping (a digit or danda alone does not need
    * the Indic path). A named alias for [[IndicScript.has]]. */
  def hasDevanagari(text: String): Boolean = has(text)

  /** The category of a codepoint in the Devanagari block (U+0900–U+097F). Characters outside the block, and
    * block members that play no part in cluster shaping, are [[IndicCategory.Other]]. */
  def category(cp: Int): IndicCategory =
    cp match
      case 0x094D                          => Virama
      case 0x093C                          => Nukta
      case 0x093F                          => PreBaseMatra
      case 0x0901 | 0x0902 | 0x0903        => SyllableModifier // candrabindu, anusvara, visarga
      case c if c >= 0x0915 && c <= 0x0939 => Consonant        // क … ह
      case c if c >= 0x0958 && c <= 0x095F => Consonant        // nukta-composed consonants
      case 0x0950 | 0x097F                 => Consonant        // OM and BdotN behave as letters
      case c if c >= 0x0904 && c <= 0x0914 => IndependentVowel // short-A … AU
      case c if c >= 0x0960 && c <= 0x0961 => IndependentVowel // vocalic RR, LL
      case c if isDependentSign(c)         => Matra
      case _                               => Other

  // The dependent vowel signs other than the pre-base short-i: they follow the base in logical order and are
  // positioned above, below or after it by the font.
  private def isDependentSign(cp: Int): Boolean =
    (cp >= 0x093A && cp <= 0x093E) ||   // OE, OOE, nukta handled above, avagraha, AA
      (cp >= 0x0940 && cp <= 0x094C) || // II, U, UU, R, RR, candra-E … AU (short-i excluded above)
      cp == 0x094E || cp == 0x094F ||   // prishthamatra E, AW
      (cp >= 0x0955 && cp <= 0x0957) || // candra long E and the below-base signs
      (cp >= 0x0962 && cp <= 0x0963)    // vocalic L, LL signs

  /** The short-i vowel sign, the one dependent sign written after its consonant in memory but drawn before
    * it. It is the only Devanagari dependent sign that reorders to the front of its cluster. */
  val ShortISign: Int = 0x093F

  def preBaseMatras: Set[Int] = Set(ShortISign)

  /** The consonant ra and the virama (halant) — together, at the head of a syllable, they form a reph.
    * `Halant` names the virama codepoint to avoid clashing with the [[IndicCategory.Virama]] enum case. */
  val Ra: Int     = 0x0930
  val Halant: Int = 0x094D

  def ra: Int     = Ra
  def halant: Int = Halant

  // Half-forms and conjuncts are the ones Hindi leans on; the akhand, rakaar, below-base and vattu forms are
  // applied when present and do nothing on a cluster that has no use for them.
  def basicFeatures: Seq[String] = Seq("nukt", "akhn", "rkrf", "blwf", "half", "vatu", "cjct")

  def presFeatures: Seq[String] = Seq("pres", "abvs", "blws", "psts", "haln", "calt")
