package io.github.edadma.texish.opentype

/** The Indic-shaping front end for the Kannada script: it supplies the character knowledge the shared
  * [[IndicScript]] segmentation and reordering build on. Kannada is written left to right, so like the other
  * Indic scripts texish sets it needs no bidirectional reordering, only cluster-scoped work.
  *
  * Kannada builds downward, as Telugu does. A consonant joined by the virama is drawn as a subscript — the
  * *ottakshara* — beneath and to the right of the base rather than as a half-form beside it, which inverts the
  * base: where Devanagari's base is the *last* consonant of a conjunct, Kannada's is the *first*, and
  * everything after it hangs below (see [[baseIndex]]). A consonant and its vowel sign commonly fuse into a
  * single glyph as well — `ಕಿ` is one glyph, not two.
  *
  * Where it parts company with Telugu is the reph. A syllable-opening ra joined by the virama does not stay a
  * base here: it is lifted out as the *arkavattu* and set after the syllable, so `ರ್ಕ` sets *ka* and then the
  * arkavattu, and `ರ್ಕಾ` sets *ka*, its aa sign, and the arkavattu after both. That is Devanagari's reph
  * position rather than Bengali's, so [[rephBeforePostBase]] stays off — but not Devanagari's reph *metrics*:
  * Devanagari's rides above its base with no advance of its own and is carried through GPOS as a mark, while
  * Kannada's is written to the upper right and takes its own room on the line, so it comes out an ordinary
  * spacing glyph.
  *
  * Nothing is drawn to the left of its consonant, so no sign is ever lifted to the front of a cluster. What
  * does move is the vowel sign that belongs to the base: typed after the whole conjunct, it is drawn on the
  * base itself and so must be moved back across the subjoined consonants (see [[preSubjoinedMatras]]). And
  * Kannada writes more of its vowel signs as combinations than any other script here — five of them split, one
  * into three parts. */
object Kannada extends IndicScript:

  /** The broad category of a Kannada character. A re-export of the shared [[IndicCategory]] so callers can name
    * the cases as `Kannada.Category.*`. */
  type Category = IndicCategory
  val Category: IndicCategory.type = IndicCategory

  import IndicCategory.*

  /** The OpenType Indic script tags, the modern `knd2` preferred over the legacy `knda`. */
  def scriptTags: Seq[String] = Seq("knd2", "knda")

  /** Whether a run contains Kannada letters or signs worth shaping (a digit alone does not need the Indic
    * path). A named alias for [[IndicScript.has]]. */
  def hasKannada(text: String): Boolean = has(text)

  /** The category of a codepoint in the Kannada block (U+0C80–U+0CFF). Characters outside the block, and block
    * members that play no part in cluster shaping — the digits, the avagraha and the siddham sign — are
    * [[IndicCategory.Other]]. */
  def category(cp: Int): IndicCategory =
    cp match
      case 0x0ccd                          => Virama
      case 0x0cbc                          => Nukta
      case c if c >= 0x0c80 && c <= 0x0c83 => SyllableModifier // spacing candrabindu, candrabindu, anusvara, visarga
      case 0x0cf3                          => SyllableModifier // combining anusvara above right
      case c if c >= 0x0c95 && c <= 0x0cb9 => Consonant        // ಕ ka … ಹ ha (0CA9 and 0CB4 are unassigned)
      case 0x0cdd | 0x0cde                 => Consonant        // nakaara pollu, and the old fa
      case c if c >= 0x0c85 && c <= 0x0c8c => IndependentVowel // ಅ a … vocalic L
      case c if c >= 0x0c8e && c <= 0x0c90 => IndependentVowel // e, ee, ai
      case c if c >= 0x0c92 && c <= 0x0c94 => IndependentVowel // o, oo, au
      case c if c >= 0x0ce0 && c <= 0x0ce1 => IndependentVowel // vocalic RR, LL
      case c if isDependentSign(c)         => Matra
      case _                               => Other

  // The dependent vowel signs. All of them follow the base in logical order and are drawn on it or after it —
  // Kannada has none of the pre-base signs that make Devanagari and Bengali reorder, since even the i sign is
  // written as a hook rising from the top of its consonant rather than to its left.
  private def isDependentSign(cp: Int): Boolean =
    (cp >= 0x0cbe && cp <= 0x0cc4) || // aa, i, ii, u, uu, vocalic r, vocalic rr
      (cp >= 0x0cc6 && cp <= 0x0cc8) || // e, ee, ai
      (cp >= 0x0cca && cp <= 0x0ccc) || // o, oo, au
      cp == 0x0cd5 || cp == 0x0cd6 ||   // length mark, ai length mark
      (cp >= 0x0ce2 && cp <= 0x0ce3)    // vocalic L, LL signs

  /** Kannada has no pre-base vowel signs: nothing is drawn to the left of the consonant it follows, so no sign
    * is ever lifted to the front of its cluster. */
  def preBaseMatras: Set[Int] = Set.empty

  /** The signs drawn on the base consonant itself, which must therefore be moved back across whatever is
    * subjoined beneath the cluster before the font can fuse the base with its vowel: `ಕ್ಕಾ` sets ka, its aa
    * sign, then the subjoined ka. These are the *atomic* signs — the composed ones are split first (see
    * [[decompose]]) and each part is placed on its own. The ones left out are the four drawn below the syllable
    * rather than on the base — the vocalic r and rr and vocalic L and LL signs — and the two length marks,
    * which trail the subjoined forms: `ಕ್ಕೀ` sets ka, the subjoined ka, then the length mark. */
  override def preSubjoinedMatras: Set[Int] =
    Set(0x0cbe, 0x0cbf, 0x0cc1, 0x0cc2, 0x0cc6, 0x0ccc) // aa, i, u, uu, e, au

  /** Kannada writes five of its vowel signs as combinations of others, and the font carries the parts rather
    * than the composed sign. These are Unicode's own canonical decompositions, and they nest: oo is the o sign
    * plus a length mark, and o is itself an e sign plus a uu sign, so `ಕೋ` reaches the font as the fused ke,
    * the uu sign and the length mark. [[IndicShaper]] applies the split until nothing decomposes further, so
    * each sign is declared in terms of the parts it is directly written as. The au sign is the one composite
    * Kannada draws with a glyph of its own, so it does not split. */
  override def decompose(cp: Int): Option[(Int, Int)] =
    cp match
      case 0x0cc0 => Some((0x0cbf, 0x0cd5)) // ii  = i  + length
      case 0x0cc7 => Some((0x0cc6, 0x0cd5)) // ee  = e  + length
      case 0x0cc8 => Some((0x0cc6, 0x0cd6)) // ai  = e  + ai length
      case 0x0cca => Some((0x0cc6, 0x0cc2)) // o   = e  + uu
      case 0x0ccb => Some((0x0cca, 0x0cd5)) // oo  = o  + length, and the o splits again
      case _      => None

  /** The consonant ra and the virama — together, at the head of a syllable, they form the arkavattu, the mark
    * Kannada sets above and after the syllable. `Halant` names the virama codepoint to avoid clashing with the
    * [[IndicCategory.Virama]] enum case. */
  val Ra: Int     = 0x0cb0
  val Halant: Int = 0x0ccd

  def ra: Int     = Ra
  def halant: Int = Halant

  // The below-base form feature `blwf` earns its place here: a virama-joined consonant becomes the ottakshara
  // Kannada draws beneath the base, which is what `ಕ್ಕ` sets. The rest are applied when the font has them and
  // do nothing on a cluster with no use for them.
  def basicFeatures: Seq[String] = Seq("nukt", "akhn", "rkrf", "blwf", "half", "pstf", "vatu", "cjct")

  def presFeatures: Seq[String] = Seq("pres", "abvs", "blws", "psts", "haln", "calt")

  /** The base of a Kannada cluster is its *first* consonant, not its last. A virama-joined consonant is drawn
    * as a subscript hanging beneath the base, so the syllable is built downward from the consonant that opens
    * it; in Devanagari and Gujarati, where the joined consonants become half-forms standing to the left, the
    * base is the last one instead. A cluster with no consonant reports its first character. */
  override def baseIndex(cps: Array[Int], start: Int, end: Int): Int =
    var i = start
    while i < end do
      if category(cps(i)) == Consonant then return i
      i += 1
    start
