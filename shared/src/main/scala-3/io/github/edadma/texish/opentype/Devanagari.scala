package io.github.edadma.texish.opentype

/** The Indic-shaping front end for Devanagari: it turns a run of characters into orthographic syllable
  * clusters and identifies, within each, the base consonant and the pre-base vowel sign that must be
  * reordered. Devanagari is written left to right, so unlike Hebrew and Arabic it needs no bidirectional
  * reordering; what it needs instead is cluster-scoped glyph reordering, which no other script texish sets
  * requires. This object supplies the character knowledge and the segmentation; the GSUB/GPOS shaping and
  * the glyph-level reordering build on it.
  *
  * An orthographic cluster is a consonant (optionally joined to earlier consonants through the virama into
  * a conjunct) carrying at most one vowel sign and any syllable modifiers, or a standalone independent
  * vowel with its modifiers. The base is the consonant the vowel sign attaches to — for Hindi the last
  * consonant of the cluster. A pre-base vowel sign (the short-i matra) is typed after its consonant but
  * drawn before it, and so is moved to the front of the cluster once the glyphs are formed.
  */
object Devanagari:

  /** The broad category of a Devanagari character, enough to segment clusters and place the base and the
    * pre-base vowel sign. Finer distinctions (which matras sit above, below or after the base) are made by
    * the font's positioning tables, not here. */
  enum Category:
    case Consonant       // क–ह and the nukta-composed consonants
    case IndependentVowel // अ, आ, इ … — a vowel that stands on its own
    case Virama          // ्  the halant that joins consonants into a conjunct
    case Nukta           // ़  a dot that modifies the preceding consonant
    case PreBaseMatra    // ि  the short-i sign: typed after its consonant, drawn before it
    case Matra           // every other dependent vowel sign (above, below or after the base)
    case SyllableModifier // anusvara, candrabindu, visarga — bind to the cluster after the base
    case Other           // digits, danda punctuation, standalone letters — not part of a cluster

  import Category.*

  /** Whether a run contains Devanagari letters or signs worth shaping (a digit or danda alone does not
    * need the Indic path). */
  def hasDevanagari(text: String): Boolean =
    var i     = 0
    var found = false
    while !found && i < text.length do
      category(text.charAt(i).toInt) match
        case Consonant | IndependentVowel | Virama | Nukta | PreBaseMatra | Matra | SyllableModifier =>
          found = true
        case Other =>
      i += 1
    found

  /** The category of a codepoint in the Devanagari block (U+0900–U+097F). Characters outside the block, and
    * block members that play no part in cluster shaping, are [[Category.Other]]. */
  def category(cp: Int): Category =
    cp match
      case 0x094D                       => Virama
      case 0x093C                       => Nukta
      case 0x093F                       => PreBaseMatra
      case 0x0901 | 0x0902 | 0x0903     => SyllableModifier         // candrabindu, anusvara, visarga
      case c if c >= 0x0915 && c <= 0x0939 => Consonant             // क … ह
      case c if c >= 0x0958 && c <= 0x095F => Consonant             // nukta-composed consonants
      case 0x0950 | 0x097F                 => Consonant             // OM and BdotN behave as letters
      case c if c >= 0x0904 && c <= 0x0914 => IndependentVowel      // short-A … AU
      case c if c >= 0x0960 && c <= 0x0961 => IndependentVowel      // vocalic RR, LL
      case c if isDependentSign(c)         => Matra
      case _                               => Other

  // The dependent vowel signs other than the pre-base short-i: they follow the base in logical order and
  // are positioned above, below or after it by the font.
  private def isDependentSign(cp: Int): Boolean =
    (cp >= 0x093A && cp <= 0x093E) ||          // OE, OOE, nukta handled above, avagraha, AA
      (cp >= 0x0940 && cp <= 0x094C) ||        // II, U, UU, R, RR, candra-E … AU (short-i excluded above)
      cp == 0x094E || cp == 0x094F ||          // prishthamatra E, AW
      (cp >= 0x0955 && cp <= 0x0957) ||        // candra long E and the below-base signs
      (cp >= 0x0962 && cp <= 0x0963)           // vocalic L, LL signs

  /** Split a run into orthographic clusters, each a `[start, end)` range. A new cluster begins at a
    * consonant or independent vowel that is not held to the previous one by a virama; viramas, vowel
    * signs, nuktas, syllable modifiers and virama-joined consonants all attach to the cluster in progress.
    * Characters outside the script ([[Category.Other]]) each stand alone, so surrounding text is untouched. */
  def clusters(cps: Array[Int]): Array[(Int, Int)] =
    if cps.isEmpty then Array.empty
    else
      val out   = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
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

  /** The index (within a `[start, end)` cluster range) of the base consonant — the one a vowel sign
    * attaches to. For Hindi this is the last consonant of the cluster; a cluster with no consonant (a bare
    * independent vowel) reports its first character. Returns an absolute index into `cps`. */
  def baseIndex(cps: Array[Int], start: Int, end: Int): Int =
    var base = -1
    var i    = start
    while i < end do
      if category(cps(i)) == Consonant then base = i
      i += 1
    if base >= 0 then base else start
