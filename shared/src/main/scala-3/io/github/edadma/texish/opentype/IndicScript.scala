package io.github.edadma.texish.opentype

import scala.collection.mutable.ArrayBuffer

/** The broad category of an Indic character, enough to segment orthographic clusters and place the base and
  * the pre-base vowel sign. Finer distinctions (which matras sit above, below or after the base) are made by
  * the font's positioning tables, not here. Shared by every Indic script texish sets ([[Devanagari]],
  * [[Bengali]], …); the codepoint-to-category mapping is what each script supplies. */
enum IndicCategory:
  case Consonant        // the base letters and the nukta-composed consonants
  case IndependentVowel // a vowel that stands on its own
  case Virama           // the halant that joins consonants into a conjunct
  case Nukta            // a dot that modifies the preceding consonant
  case PreBaseMatra     // a dependent vowel sign typed after its consonant but drawn before it
  case Matra            // every other dependent vowel sign (above, below or after the base)
  case SyllableModifier // anusvara, candrabindu, visarga — bind to the cluster after the base
  case Other            // digits, punctuation, standalone letters — not part of a cluster

/** The character knowledge and glyph-level reordering shared by texish's Indic scripts. An Indic script is
  * written left to right, so unlike Hebrew and Arabic it needs no bidirectional reordering; what it needs
  * instead is cluster-scoped segmentation and reordering, which no other script texish sets requires.
  *
  * A concrete script supplies the codepoint knowledge — its OpenType script tags, the category of each
  * character, the consonant ra and the virama, the set of pre-base vowel signs, any two-part matras that
  * decompose, and the GSUB feature order — and this trait provides the segmentation and reordering built on
  * it. The GSUB/GPOS shaping runs on top through [[IndicShaper]].
  *
  * An orthographic cluster is a consonant (optionally joined to earlier consonants through the virama into a
  * conjunct) carrying at most one vowel sign and any syllable modifiers, or a standalone independent vowel
  * with its modifiers. The base is the consonant the vowel sign attaches to — the last consonant of the
  * cluster. A pre-base vowel sign is typed after its consonant but drawn before it, and so is moved to the
  * front of the cluster once the glyphs are formed. */
trait IndicScript:
  import IndicCategory.*

  /** The OpenType script tags this script binds to, most-preferred first — the modern tag before the legacy
    * one (Devanagari `dev2`, `deva`; Bengali `bng2`, `beng`). */
  def scriptTags: Seq[String]

  /** The category of a codepoint. Characters outside the script's block, and block members that play no part
    * in cluster shaping, are [[IndicCategory.Other]]. */
  def category(cp: Int): IndicCategory

  /** The consonant ra — with the virama at the head of a syllable it forms a reph. */
  def ra: Int

  /** The virama (halant) that joins consonants into a conjunct. */
  def halant: Int

  /** The dependent vowel signs that reorder to the front of their cluster — typed after the base consonant in
    * memory but drawn before it. A two-part matra contributes its pre-base part here after decomposition. */
  def preBaseMatras: Set[Int]

  /** The basic-form GSUB features, applied in the OpenType Indic order before reordering: nukta composition,
    * the akhand ligatures, the rakaar and below-base forms, the half- and post-base forms, the vattu variant
    * and the general conjunct. The reph feature `rphf` is not listed here — it must fire only on a
    * cluster-initial ra, so [[IndicShaper]] applies it on its own. */
  def basicFeatures: Seq[String]

  /** The presentation GSUB features, applied after reordering: the pre-, above-, below- and post-base
    * substitutions that select contextual glyph variants, and the halant and contextual-alternate cleanups. */
  def presFeatures: Seq[String]

  /** The two parts of a two-part matra — a pre-base part and a post-base part — or None for every other
    * character. A two-part vowel sign is one codepoint in memory that renders as a sign before the base and a
    * sign after it (Bengali o and au); it is split into its parts, per Unicode canonical decomposition, before
    * the cluster is shaped. Scripts with no two-part matras (Devanagari) leave this at its default. */
  def decompose(cp: Int): Option[(Int, Int)] = None

  /** The GSUB feature that selects a word-initial form of the pre-base vowel sign, or None when the script has
    * none. Applied to the first glyph of a word (see [[IndicShaper]]), matching the OpenType word-position
    * semantics of `init`. */
  def initFeature: Option[String] = None

  /** The GSUB feature that selects a word-final form of a post-base vowel sign, or None. Applied to the last
    * glyph of a word, matching the word-position semantics of `fina`. */
  def finaFeature: Option[String] = None

  /** Whether a cluster's reph is inserted before its post-base vowel signs rather than at the cluster's very
    * end. Reph position is a per-script convention: Devanagari draws the reph after everything (র্মা-style
    * words don't arise there with a trailing sign the reph must precede), while Bengali sets it between the
    * base and a post-base sign — র্মা renders ma, reph, aa. A script that turns this on must also supply
    * [[postBaseMatras]], the signs the reph slots in front of. */
  def rephBeforePostBase: Boolean = false

  /** The dependent vowel signs that stay after the base — where a [[rephBeforePostBase]] script's reph is
    * inserted in front of. Unused (and empty) for a script whose reph closes the cluster. */
  def postBaseMatras: Set[Int] = Set.empty

  /** Whether a run contains letters or signs of this script worth shaping (a digit or punctuation mark alone
    * does not need the Indic path). */
  def has(text: String): Boolean =
    var i     = 0
    var found = false
    while !found && i < text.length do
      category(text.charAt(i).toInt) match
        case Consonant | IndependentVowel | Virama | Nukta | PreBaseMatra | Matra | SyllableModifier =>
          found = true
        case Other =>
      i += 1
    found

  /** Split a run into orthographic clusters, each a `[start, end)` range. A new cluster begins at a consonant
    * or independent vowel that is not held to the previous one by a virama; viramas, vowel signs, nuktas,
    * syllable modifiers and virama-joined consonants all attach to the cluster in progress. Characters outside
    * the script ([[IndicCategory.Other]]) each stand alone, so surrounding text is untouched. */
  def clusters(cps: Array[Int]): Array[(Int, Int)] =
    if cps.isEmpty then Array.empty
    else
      val out   = ArrayBuffer.empty[(Int, Int)]
      var start = 0
      var i     = 1
      while i < cps.length do
        val cat  = category(cps(i))
        val prev = category(cps(i - 1))
        val startsCluster = cat match
          case Consonant | IndependentVowel => prev != Virama // a virama binds this consonant to the cluster
          case Other                        => true           // punctuation/digits break the run
          case _                            => false
        val breaksAfterPrev = prev == Other                    // Other never absorbs what follows
        if startsCluster || breaksAfterPrev then
          out += ((start, i))
          start = i
        i += 1
      out += ((start, cps.length))
      out.toArray

  /** The index (within a `[start, end)` cluster range) of the base consonant — the one a vowel sign attaches
    * to. This is the last consonant of the cluster; a cluster with no consonant (a bare independent vowel)
    * reports its first character. Returns an absolute index into `cps`. */
  def baseIndex(cps: Array[Int], start: Int, end: Int): Int =
    var base = -1
    var i    = start
    while i < end do
      if category(cps(i)) == Consonant then base = i
      i += 1
    if base >= 0 then base else start

  /** Whether a cluster opens with a reph: a word-initial ra and virama followed by a base consonant. A reph's
    * ra+virama is not drawn in place but as a mark above the syllable's base, so it is lifted out and shaped
    * apart from the rest of the cluster. A ra+virama with nothing after it is a dead ra, not a reph. */
  def startsWithReph(clusterCps: Array[Int]): Boolean =
    clusterCps.length >= 3 && clusterCps(0) == ra && clusterCps(1) == halant &&
      (2 until clusterCps.length).exists(i => category(clusterCps(i)) == Consonant)

  /** Move a cluster's pre-base vowel sign to the front of its shaped glyphs. A pre-base sign is typed after
    * its base consonant but rendered before the whole cluster — ahead of any half-forms or conjuncts. Because
    * no basic-form substitution consumes it, its glyph survives the earlier GSUB pass unchanged and is found
    * by `preBaseMatraGlyph`, the glyph the font maps the pre-base sign to; it is lifted out and prepended.
    * When the cluster has no pre-base sign, or its glyph is already first, the run is returned unchanged.
    * `clusterCps` is the cluster's source codepoints (after any two-part decomposition), used only to tell
    * whether a pre-base sign is present. */
  def reorderPreBaseMatra(clusterCps: Array[Int], glyphs: Array[Int], preBaseMatraGlyph: Int): Array[Int] =
    if !clusterCps.exists(preBaseMatras.contains) then glyphs
    else
      val j = glyphs.indexOf(preBaseMatraGlyph)
      if j <= 0 then glyphs
      else Array(preBaseMatraGlyph) ++ glyphs.slice(0, j) ++ glyphs.slice(j + 1, glyphs.length)
