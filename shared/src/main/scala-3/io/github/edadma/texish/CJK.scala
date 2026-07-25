package io.github.edadma.texish

/** Character classification for CJK (Chinese / Japanese / Korean) line breaking.
  *
  * texish breaks paragraphs only at glue, penalties and discretionaries, exactly as Knuth TeX does. CJK
  * text carries no interword spaces, so a run of it would arrive at the line breaker as one unbreakable box
  * and overflow the measure. Following the approach xeCJK and luatexja take on the Unicode TeX engines —
  * preprocessing the character stream rather than forking the breaker — the paragraph builder inserts a
  * break opportunity between adjacent CJK characters and suppresses the breaks that kinsoku (line-break
  * prohibition) forbids: a closing mark never begins a line, an opening mark never ends one. Knuth–Plass
  * itself is untouched; this object only decides where those break opportunities go.
  */
object CJK:

  /** Whether `cp` is a CJK codepoint that breaks *freely* between characters — the Han ideographs and their
    * extensions, the Japanese kana, and the CJK-symbol and fullwidth-form punctuation blocks. A run of these
    * has no spaces of its own, so each adjacency is a candidate break.
    *
    * The Hangul syllables are deliberately excluded. Korean, unlike Chinese and Japanese, is written with
    * interword spaces (between eojeol), and its lines break at those spaces the way Latin does — breaking
    * between arbitrary syllables would split words. A break inside a Korean word is instead offered as a last
    * resort ([[isHangul]], [[lastResortBetween]]), so an unspaced run still wraps rather than overflowing.
    * (A word of Han among Korean text still breaks per character, as that Han is not spaced.)
    */
  def isCJK(cp: Int): Boolean =
    (cp >= 0x4e00 && cp <= 0x9fff) ||   // CJK Unified Ideographs
      (cp >= 0x3400 && cp <= 0x4dbf) ||   // Unified Ideographs Extension A
      (cp >= 0x20000 && cp <= 0x2a6df) || // Extension B
      (cp >= 0x2a700 && cp <= 0x2ebef) || // Extensions C–F
      (cp >= 0xf900 && cp <= 0xfaff) ||   // CJK Compatibility Ideographs
      (cp >= 0x3040 && cp <= 0x309f) ||   // Hiragana
      (cp >= 0x30a0 && cp <= 0x30ff) ||   // Katakana
      (cp >= 0x3000 && cp <= 0x303f) ||   // CJK Symbols and Punctuation
      (cp >= 0xff00 && cp <= 0xffef)      // Halfwidth and Fullwidth Forms

  // Closing marks that may not start a line (no break *before*): the ideographic and fullwidth stops and
  // commas, closing brackets and quotes, and the kana iteration / small forms that bind leftward.
  private val NoBreakBefore =
    "、。，．！？：；）］｝〕〉》」』】〗〙〟’”々ーヽヾ" +
      "ぁぃぅぇぉっゃゅょゎァィゥェォッャュョヮ"

  // Opening marks that may not end a line (no break *after*): opening brackets and quotes.
  private val NoBreakAfter =
    "（［｛〔〈《「『【〖〘〝‘“"

  /** Whether `cp` is a Hangul syllable. Korean breaks at its interword spaces, so a break between two of these
    * is not a free opportunity but a last resort — see [[lastResortBetween]]. */
  def isHangul(cp: Int): Boolean = cp >= 0xac00 && cp <= 0xd7af

  /** A closing character that kinsoku forbids at the start of a line. */
  def noBreakBefore(cp: Int): Boolean = NoBreakBefore.indexOf(cp) >= 0

  /** An opening character that kinsoku forbids at the end of a line. */
  def noBreakAfter(cp: Int): Boolean = NoBreakAfter.indexOf(cp) >= 0

  /** A line break is permitted between `a` and `b` when at least one is CJK (two adjacent Latin letters keep
    * their existing space-only breaking) and neither kinsoku rule applies. */
  def breakableBetween(a: Int, b: Int): Boolean =
    (isCJK(a) || isCJK(b)) && !noBreakBefore(b) && !noBreakAfter(a)

  /** A break between `a` and `b` that is legal but costly: inside a Korean word, between two Hangul syllables.
    * Korean prose breaks at its interword spaces, so the breaker takes one of these only when no break at a
    * space can be made to fit. It is the escape hatch that keeps a long unspaced run — a compound, a title set
    * without spaces, a narrow column — wrapping instead of running off the page. */
  def lastResortBetween(a: Int, b: Int): Boolean =
    isHangul(a) && isHangul(b) && !noBreakBefore(b) && !noBreakAfter(a)

  /** Whether `s` contains a freely-breaking CJK codepoint. */
  def hasCJK(s: String): Boolean = existsCodepoint(s, isCJK)

  /** Whether `s` needs the per-character break pass at all — it carries either CJK that breaks freely or
    * Hangul that can break as a last resort. A pure-Latin run skips the pass and keeps Liang hyphenation
    * unchanged. */
  def needsCharacterBreaks(s: String): Boolean = existsCodepoint(s, cp => isCJK(cp) || isHangul(cp))

  private def existsCodepoint(s: String, p: Int => Boolean): Boolean =
    var i = 0
    while i < s.length do
      val cp = s.codePointAt(i)
      if p(cp) then return true
      i += Character.charCount(cp)
    false
