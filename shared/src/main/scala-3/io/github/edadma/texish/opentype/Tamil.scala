package io.github.edadma.texish.opentype

/** The Indic-shaping front end for the Tamil script: it supplies the character knowledge the shared
  * [[IndicScript]] segmentation and reordering build on. Tamil is written left to right, so like the other
  * Indic scripts texish sets it needs no bidirectional reordering, only cluster-scoped work.
  *
  * Tamil asks less of a shaper than the scripts to its north, and what it does ask is asked differently. It
  * has no half-forms, no subjoined consonants and no reph: a consonant silenced by the virama keeps the virama
  * as a visible dot, the pulli, and stands as a complete letter of its own — `க்` — rather than fusing with
  * what follows. Only two conjuncts survive from Sanskrit, `க்ஷ` and the sacred `ஸ்ரீ`, and the font draws each
  * as a single ligature. The consequence for the shaper is that a pre-base vowel sign leaps over its own
  * consonant and no further, so `க்கே` sets ka, pulli, the ee sign, ka — the placement
  * [[preBaseMatraBeforeBase]] selects. Where the ligature has swallowed the pulli there is nothing left to
  * stand in the way and the sign reaches the front, as in `க்ஷே`.
  *
  * Three vowel signs are drawn before their consonant (e, ee and ai) and three are written as two parts (o, oo
  * and au), each an e or ee sign before the base and an aa sign or length mark after it. The short-i and
  * long-i signs, which reorder in Bengali, stay after the base in Tamil; the font gives them width-matched
  * variants and fuses the u and uu signs into the consonant outright, both through its post-base
  * substitutions.
  */
object Tamil extends IndicScript:

  import IndicCategory.*

  /** The OpenType Indic script tags, the modern `tml2` preferred over the legacy `taml`. */
  def scriptTags: Seq[String] = Seq("tml2", "taml")

  /** Whether a run contains Tamil letters or signs worth shaping (a digit or a numeral sign alone does not
    * need the Indic path). A named alias for [[IndicScript.has]]. */
  def hasTamil(text: String): Boolean = has(text)

  /** The category of a codepoint in the Tamil block (U+0B80–U+0BFF). The block is a sparse one — Tamil writes
    * one letter where Sanskrit writes a voiced, aspirated and unaspirated set, so the consonant rows are full
    * of holes — and the unassigned codepoints in those holes, like every character outside the block, are
    * [[IndicCategory.Other]]. */
  def category(cp: Int): IndicCategory =
    cp match
      case 0x0bcd                                              => Virama
      case 0x0bc6 | 0x0bc7 | 0x0bc8                            => PreBaseMatra     // ெ e, ே ee, ை ai
      case 0x0b82 | 0x0b83                                     => SyllableModifier // anusvara, ஃ aytham
      case 0x0b95 | 0x0b99 | 0x0b9a | 0x0b9c | 0x0b9e | 0x0b9f => Consonant        // க, ங, ச, ஜ, ஞ, ட
      case 0x0ba3 | 0x0ba4                                     => Consonant        // ண, த
      case c if c >= 0x0ba8 && c <= 0x0baa                     => Consonant        // ந, ன, ப
      case c if c >= 0x0bae && c <= 0x0bb9                     => Consonant        // ம ma … ஹ ha
      case c if c >= 0x0b85 && c <= 0x0b8a                     => IndependentVowel // அ a … ஊ uu
      case c if c >= 0x0b8e && c <= 0x0b90                     => IndependentVowel // எ e, ஏ ee, ஐ ai
      case c if c >= 0x0b92 && c <= 0x0b94                     => IndependentVowel // ஒ o, ஓ oo, ஔ au
      case c if isDependentSign(c)                             => Matra
      case _                                                   => Other

  // The dependent vowel signs other than the pre-base e, ee and ai: the aa sign, the two i signs and the two
  // u signs, the two-part o, oo and au, and the au length mark one of them decomposes to. All of them follow
  // the base in logical order — nothing but the pre-base three is drawn to its left.
  private def isDependentSign(cp: Int): Boolean =
    (cp >= 0x0bbe && cp <= 0x0bc2) || // ா aa, ி i, ீ ii, ு u, ூ uu
      (cp >= 0x0bca && cp <= 0x0bcc) || // ொ o, ோ oo, ௌ au (two-part, split before shaping)
      cp == 0x0bd7                      // ௗ au length mark

  /** The vowel signs drawn before the base: e, ee and ai. The e and ee signs are also the pre-base parts the
    * two-part o, oo and au signs decompose to. Tamil's short-i sign is not among them — unlike Bengali's, it
    * is drawn above and to the right of the consonant and stays after it. */
  def preBaseMatras: Set[Int] = Set(0x0bc6, 0x0bc7, 0x0bc8)

  /** Split the two-part vowel signs into their pre-base and post-base parts, per Unicode canonical
    * decomposition: o into the e sign before the base and the aa sign after it, oo into the ee sign and the aa
    * sign, au into the e sign and the au length mark. Every other character is one part. */
  override def decompose(cp: Int): Option[(Int, Int)] =
    cp match
      case 0x0bca => Some((0x0bc6, 0x0bbe)) // ொ o  = e sign  (pre-base) + aa sign (post-base)
      case 0x0bcb => Some((0x0bc7, 0x0bbe)) // ோ oo = ee sign (pre-base) + aa sign (post-base)
      case 0x0bcc => Some((0x0bc6, 0x0bd7)) // ௌ au = e sign  (pre-base) + au length mark
      case _      => None

  /** The consonant ra and the virama. Tamil forms no reph from them (see [[startsWithReph]]), but the shared
    * trait still needs them named — and the virama does the second job of locating the base, since Tamil is a
    * script that leaves it standing (see [[preBaseMatraBeforeBase]]). */
  def ra: Int     = 0x0bb0
  def halant: Int = 0x0bcd

  // Of these the bundled face carries only `akhn`, which forms the two surviving conjunct ligatures. The rest
  // are applied for the sake of a font that has them and do nothing on a cluster with no use for them.
  def basicFeatures: Seq[String] = Seq("nukt", "akhn", "rkrf", "blwf", "half", "pstf", "vatu", "cjct")

  // `psts` earns its place here: it both fuses a consonant with its u or uu sign into one drawn glyph and
  // picks the width-matched variant of the short-i and long-i signs.
  def presFeatures: Seq[String] = Seq("pres", "abvs", "blws", "psts", "haln", "calt")

  /** Tamil forms no reph: a word-initial ra followed by a virama keeps its pulli and stays an ordinary letter,
    * as `ர்க` shows, rather than rising as a mark over the syllable that follows. This overrides the shared
    * detection, which would otherwise lift the ra out of the cluster. */
  override def startsWithReph(clusterCps: Array[Int]): Boolean = false

  /** A pre-base vowel sign is drawn before the consonant it belongs to, not before the whole cluster. Tamil
    * writes a silenced consonant with a visible pulli rather than folding it into a half-form, so a consonant
    * ahead of the base is a letter in its own right and the sign does not leap over it. */
  override def preBaseMatraBeforeBase: Boolean = true
