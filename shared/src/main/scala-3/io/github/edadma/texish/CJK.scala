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

  /** Whether `cp` is a CJK codepoint that takes part in inter-character breaking — the Han ideographs and
    * their extensions, the Japanese kana, the Hangul syllables, and the CJK-symbol and fullwidth-form
    * punctuation blocks. A run of these has no spaces of its own, so each adjacency is a candidate break.
    */
  def isCJK(cp: Int): Boolean =
    (cp >= 0x4e00 && cp <= 0x9fff) ||   // CJK Unified Ideographs
      (cp >= 0x3400 && cp <= 0x4dbf) ||   // Unified Ideographs Extension A
      (cp >= 0x20000 && cp <= 0x2a6df) || // Extension B
      (cp >= 0x2a700 && cp <= 0x2ebef) || // Extensions C–F
      (cp >= 0xf900 && cp <= 0xfaff) ||   // CJK Compatibility Ideographs
      (cp >= 0x3040 && cp <= 0x309f) ||   // Hiragana
      (cp >= 0x30a0 && cp <= 0x30ff) ||   // Katakana
      (cp >= 0xac00 && cp <= 0xd7af) ||   // Hangul syllables
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

  /** A closing character that kinsoku forbids at the start of a line. */
  def noBreakBefore(cp: Int): Boolean = NoBreakBefore.indexOf(cp) >= 0

  /** An opening character that kinsoku forbids at the end of a line. */
  def noBreakAfter(cp: Int): Boolean = NoBreakAfter.indexOf(cp) >= 0

  /** A line break is permitted between `a` and `b` when at least one is CJK (two adjacent Latin letters keep
    * their existing space-only breaking) and neither kinsoku rule applies. */
  def breakableBetween(a: Int, b: Int): Boolean =
    (isCJK(a) || isCJK(b)) && !noBreakBefore(b) && !noBreakAfter(a)

  /** Whether `s` contains any CJK codepoint, deciding whether a text run needs the per-character break pass
    * at all; a pure-Latin run skips it and keeps Liang hyphenation unchanged. */
  def hasCJK(s: String): Boolean =
    var i = 0
    while i < s.length do
      val cp = s.codePointAt(i)
      if isCJK(cp) then return true
      i += Character.charCount(cp)
    false
