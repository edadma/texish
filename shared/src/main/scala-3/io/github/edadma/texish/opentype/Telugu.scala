package io.github.edadma.texish.opentype

/** The Indic-shaping front end for the Telugu script: it supplies the character knowledge the shared
  * [[IndicScript]] segmentation and reordering build on. Telugu is written left to right, so like the other
  * Indic scripts texish sets it needs no bidirectional reordering, only cluster-scoped work.
  *
  * Telugu builds downward. A consonant joined by the virama is drawn as a subscript beneath the base rather
  * than as a half-form beside it, which inverts the base: where Devanagari's base is the *last* consonant of a
  * conjunct, Telugu's is the *first*, and everything after it hangs below (see [[baseIndex]]). A consonant and
  * its vowel sign commonly fuse into a single glyph as well — `కి` is one glyph, not two. The font's
  * below-base and post-base features do that work, and GPOS places the subscripts.
  *
  * Nothing is drawn to the left of its consonant, so unlike the short-i of Devanagari or the e/ai of Bengali
  * no sign is ever lifted to the front of a cluster. What does move is the vowel sign that belongs to the
  * base: typed after the whole conjunct, it is drawn on the base itself and so must be moved back across the
  * subjoined consonants before the font can fuse the two (see [[preSubjoinedMatras]]). Nor is there a reph —
  * a word-initial ra with a virama does not rise as a mark over the syllable, as `ర్క` shows, where the ra
  * stays a base and the following consonant subjoins under it.
  */
object Telugu extends IndicScript:

  import IndicCategory.*

  /** The OpenType Indic script tags, the modern `tel2` preferred over the legacy `telu`. */
  def scriptTags: Seq[String] = Seq("tel2", "telu")

  /** Whether a run contains Telugu letters or signs worth shaping (a digit alone does not need the Indic
    * path). A named alias for [[IndicScript.has]]. */
  def hasTelugu(text: String): Boolean = has(text)

  /** The category of a codepoint in the Telugu block (U+0C00–U+0C7F). Characters outside the block, and
    * block members that play no part in cluster shaping, are [[IndicCategory.Other]]. */
  def category(cp: Int): IndicCategory =
    cp match
      case 0x0c4d                          => Virama
      case 0x0c3c                          => Nukta
      case c if c >= 0x0c00 && c <= 0x0c04 => SyllableModifier // candrabindu, anusvara, visarga
      case c if c >= 0x0c15 && c <= 0x0c28 => Consonant        // క ka … న na
      case c if c >= 0x0c2a && c <= 0x0c39 => Consonant        // ప pa … హ ha
      case c if c >= 0x0c58 && c <= 0x0c5a => Consonant        // tsa, dza, rrra
      case 0x0c5d                          => Consonant        // nakaara pollu
      case c if c >= 0x0c05 && c <= 0x0c0c => IndependentVowel // అ a … vocalic L
      case c if c >= 0x0c0e && c <= 0x0c10 => IndependentVowel // e, ee, ai
      case c if c >= 0x0c12 && c <= 0x0c14 => IndependentVowel // o, oo, au
      case c if c >= 0x0c60 && c <= 0x0c61 => IndependentVowel // vocalic RR, LL
      case c if isDependentSign(c)         => Matra
      case _                               => Other

  // The dependent vowel signs. All of them follow the base in logical order and are drawn above it or after
  // it — Telugu has none of the pre-base signs that make Devanagari and Bengali reorder.
  private def isDependentSign(cp: Int): Boolean =
    (cp >= 0x0c3e && cp <= 0x0c44) || // aa, i, ii, u, uu, vocalic r, vocalic rr
      (cp >= 0x0c46 && cp <= 0x0c48) || // e, ee, ai
      (cp >= 0x0c4a && cp <= 0x0c4c) || // o, oo, au
      cp == 0x0c55 || cp == 0x0c56 ||   // length mark, ai length mark
      (cp >= 0x0c62 && cp <= 0x0c63)    // vocalic L, LL signs

  /** Telugu has no pre-base vowel signs: nothing is drawn to the left of the consonant it follows, so no
    * sign is ever lifted to the front of its cluster. */
  def preBaseMatras: Set[Int] = Set.empty

  /** Almost every Telugu vowel sign is drawn on the base consonant, so it belongs beside the base and ahead
    * of whatever is subjoined beneath the cluster: `స్త్రే` sets sa, its e sign, then the subjoined ta and ra.
    * The exceptions are the vocalic r and rr signs (U+0C43, U+0C44), which are themselves drawn below and
    * stay after the subjoined forms. */
  override def preSubjoinedMatras: Set[Int] =
    (0x0c00 to 0x0c7f).filter(cp => isDependentSign(cp) && cp != 0x0c43 && cp != 0x0c44).toSet

  /** The ai sign is written as the e sign followed by the ai length mark, both drawn on the base. The font
    * carries the pair, not the single sign, so it is split before the cluster is shaped; every other Telugu
    * vowel sign is one part. */
  override def decompose(cp: Int): Option[(Int, Int)] =
    if cp == 0x0c48 then Some((0x0c46, 0x0c56)) else None

  /** The consonant ra and the virama. Telugu forms no reph from them (see [[startsWithReph]]), but the
    * shared trait still needs them named. */
  def ra: Int     = 0x0c30
  def halant: Int = 0x0c4d

  // The post-base form feature `pstf` earns its place here: a virama-joined consonant becomes the subscript
  // Telugu draws beneath the base, which is what `క్క` sets. The rest are applied when the font has them and
  // do nothing on a cluster with no use for them.
  def basicFeatures: Seq[String] = Seq("nukt", "akhn", "rkrf", "blwf", "half", "pstf", "vatu", "cjct")

  def presFeatures: Seq[String] = Seq("pres", "abvs", "blws", "psts", "haln", "calt")

  /** Telugu forms no reph: a word-initial ra followed by a virama stays an ordinary base and the consonant
    * after it subjoins beneath, rather than the ra rising as a mark over the syllable. This overrides the
    * shared detection, which would otherwise lift the ra out of the cluster. */
  override def startsWithReph(clusterCps: Array[Int]): Boolean = false

  /** The base of a Telugu cluster is its *first* consonant, not its last. A virama-joined consonant is drawn
    * as a subscript hanging beneath the base, so the syllable is built downward from the consonant that opens
    * it; in Devanagari and Bengali, where the joined consonants become half-forms standing to the left, the
    * base is the last one instead. A cluster with no consonant reports its first character. */
  override def baseIndex(cps: Array[Int], start: Int, end: Int): Int =
    var i = start
    while i < end do
      if category(cps(i)) == Consonant then return i
      i += 1
    start
