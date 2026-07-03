package io.github.edadma.texish.opentype

/** The Indic-shaping front end for the Bengali–Assamese script: it supplies the character knowledge the
  * shared [[IndicScript]] segmentation and reordering build on. Bengali is written left to right, so like
  * Devanagari it needs no bidirectional reordering, only the cluster-scoped work of an Indic script.
  *
  * Bengali asks a little more of the shaper than Devanagari does. Three vowel signs reorder before the base
  * (i, e and ai), not one. Two of the vowel signs are two-part — o renders as an e-sign before the base and
  * an aa-sign after it, au as an e-sign before and a length mark after — and are split into those parts, per
  * Unicode canonical decomposition, before the cluster is shaped. Its post-base ya form (`ya-phalaa`) is
  * selected by the `pstf` feature. And the font gives the pre-base e/ai signs and the post-base aa/ii/length
  * signs distinct word-initial and word-final forms, chosen by `init` and `fina` on the first and last glyph
  * of a word. The reph, conjuncts and below-base signs work exactly as in Devanagari. The same script serves
  * Bengali, Assamese and the other languages written in this script. */
object Bengali extends IndicScript:

  import IndicCategory.*

  /** The OpenType Indic script tags, the modern `bng2` preferred over the legacy `beng`. */
  def scriptTags: Seq[String] = Seq("bng2", "beng")

  /** Whether a run contains Bengali letters or signs worth shaping (a digit or currency mark alone does not
    * need the Indic path). A named alias for [[IndicScript.has]]. */
  def hasBengali(text: String): Boolean = has(text)

  /** The category of a codepoint in the Bengali block (U+0980–U+09FF). Characters outside the block, and
    * block members that play no part in cluster shaping, are [[IndicCategory.Other]]. */
  def category(cp: Int): IndicCategory =
    cp match
      case 0x09CD                          => Virama
      case 0x09BC                          => Nukta
      case 0x09BF | 0x09C7 | 0x09C8        => PreBaseMatra    // i, e, ai — drawn before the base
      case 0x0981 | 0x0982 | 0x0983        => SyllableModifier // candrabindu, anusvara, visarga
      case c if c >= 0x0995 && c <= 0x09A8 => Consonant        // ka … na
      case c if c >= 0x09AA && c <= 0x09B0 => Consonant        // pa … ra
      case 0x09B2                          => Consonant        // la
      case c if c >= 0x09B6 && c <= 0x09B9 => Consonant        // sha, ssa, sa, ha
      case 0x09DC | 0x09DD | 0x09DF        => Consonant        // rra, rha, yya (nukta-composed)
      case 0x09CE                          => Consonant        // khanda ta, a standalone consonant form
      case 0x09F0 | 0x09F1                 => Consonant        // Assamese ra and wa/va
      case c if c >= 0x0985 && c <= 0x098C => IndependentVowel // A, AA, I, II, U, UU, vocalic R, vocalic L
      case 0x098F | 0x0990                 => IndependentVowel // E, AI
      case 0x0993 | 0x0994                 => IndependentVowel // O, AU
      case 0x09E0 | 0x09E1                 => IndependentVowel // vocalic RR, LL
      case c if isDependentSign(c)         => Matra
      case _                               => Other

  // The dependent vowel signs other than the pre-base i/e/ai: aa, ii, the below-base u/uu/vocalic signs, the
  // two-part o and au, the au length mark, and the vocalic L/LL signs. They follow the base in logical order
  // and are positioned above, below or after it by the font.
  private def isDependentSign(cp: Int): Boolean =
    cp == 0x09BE ||                     // aa
      (cp >= 0x09C0 && cp <= 0x09C4) || // ii, u, uu, vocalic r, vocalic rr
      cp == 0x09CB || cp == 0x09CC ||   // o, au (two-part, split before shaping)
      cp == 0x09D7 ||                   // au length mark
      (cp >= 0x09E2 && cp <= 0x09E3)    // vocalic L, LL signs

  /** The vowel signs drawn before the base: i, e and ai. The e-sign is also the pre-base part the two-part o
    * and au signs decompose to. */
  def preBaseMatras: Set[Int] = Set(0x09BF, 0x09C7, 0x09C8)

  /** Split the two-part vowel signs into their pre-base and post-base parts, per Unicode canonical
    * decomposition: o (U+09CB) into the e-sign before the base and the aa-sign after it, au (U+09CC) into the
    * e-sign before and the au length mark after. Every other character has no decomposition. */
  override def decompose(cp: Int): Option[(Int, Int)] =
    cp match
      case 0x09CB => Some((0x09C7, 0x09BE)) // o  = e-sign (pre-base) + aa-sign (post-base)
      case 0x09CC => Some((0x09C7, 0x09D7)) // au = e-sign (pre-base) + au length mark (post-base)
      case _      => None

  /** The consonant ra and the virama — together, at the head of a syllable, they form a reph. */
  def ra: Int     = 0x09B0
  def halant: Int = 0x09CD

  // Bengali adds the post-base form `pstf` to the Devanagari basic set — it selects the ya-phalaa, the
  // post-base form of ya. The rest are applied when present and do nothing on a cluster with no use for them.
  def basicFeatures: Seq[String] = Seq("nukt", "akhn", "rkrf", "blwf", "half", "pstf", "vatu", "cjct")

  def presFeatures: Seq[String] = Seq("pres", "abvs", "blws", "psts", "haln", "calt")

  /** The word-initial form of the pre-base e and ai signs, chosen by `init` on the first glyph of a word. */
  override def initFeature: Option[String] = Some("init")

  /** The word-final form of the post-base aa, ii and length signs, chosen by `fina` on the last glyph. */
  override def finaFeature: Option[String] = Some("fina")

  /** The Bengali reph sits between the base and a post-base vowel sign: র্মা renders ma, reph, aa — not with
    * the reph trailing the aa. (Verified against HarfBuzz with the bundled Noto Serif Bengali.) */
  override def rephBeforePostBase: Boolean = true

  /** The post-base signs the reph is inserted in front of: aa, ii and the au length mark — the signs that
    * stay after the base as spacing glyphs (and that `fina` gives word-final forms). */
  override def postBaseMatras: Set[Int] = Set(0x09BE, 0x09C0, 0x09D7)
