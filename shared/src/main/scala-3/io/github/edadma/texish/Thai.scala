package io.github.edadma.texish

/** Character classification for Thai line breaking.
  *
  * Thai is written without spaces between words, so a run of it reaches the line breaker as one unbreakable
  * box and overflows the measure — the same difficulty CJK presents (see [[CJK]]). What Thai does use spaces
  * for is to separate phrases and sentences, and those are ordinary glue the breaker already prefers; the work
  * here is to offer somewhere to break when a phrase is itself longer than a line.
  *
  * '''This is cluster breaking, not word breaking.''' Thai lines properly break at word boundaries, and
  * finding those requires a dictionary — Thai orthography does not mark where one word ends and the next
  * begins, which is why every system that breaks Thai correctly (ICU, LibThai, the browsers that use them)
  * carries a lexicon. texish has no such dictionary, so a break offered here falls at an orthographic cluster
  * boundary, which may land inside a word. It is offered as a '''last resort''', at a heavy penalty, so the
  * breaker reaches for one only when no break at a phrase space will fit: prose with normal spacing is set
  * from those spaces alone and never sees a cluster break. Should a dictionary be added later, it would
  * replace this rule with word boundaries at no penalty; the seam is [[breakableBetween]].
  *
  * What the cluster rule does guarantee is that a break never falls somewhere visibly wrong. A combining
  * vowel or tone mark is never parted from the consonant it sits on, and a pre-posed vowel — one of the five
  * written to the left of the consonant it is pronounced after — is never left stranded at the end of a line
  * away from that consonant.
  */
object Thai:

  /** Whether `cp` is in the Thai block. */
  def isThai(cp: Int): Boolean = cp >= 0x0e00 && cp <= 0x0e7f

  /** Whether `cp` binds to what precedes it, so a line may not begin with it. Two kinds sit here: the marks
    * that combine onto a base — the above and below vowels, the four tone marks, thanthakhat, nikhahit,
    * phinthu — and the spacing signs that nonetheless belong to the base before them, chiefly the vowels sara
    * a, sara aa and sara am, the repetition mark maiyamok, and the abbreviation and terminating signs.
    */
  def noBreakBefore(cp: Int): Boolean =
    cp == 0x0e31 ||                     // mai han akat, above
      (cp >= 0x0e34 && cp <= 0x0e3a) || // above vowels i–ue, below vowels u–uu, phinthu
      (cp >= 0x0e47 && cp <= 0x0e4e) || // maitaikhu, the tone marks, thanthakhat, nikhahit, yamakkan
      cp == 0x0e30 || cp == 0x0e32 ||   // sara a, sara aa — spacing, but belonging to the base before them
      cp == 0x0e33 ||                   // sara am
      cp == 0x0e45 || cp == 0x0e46 ||   // lakkhangyao, maiyamok
      cp == 0x0e2f ||                   // paiyannoi, the abbreviation sign
      cp == 0x0e4f || cp == 0x0e5a || cp == 0x0e5b // fongman and the terminators

  /** Whether `cp` binds to what follows it, so a line may not end with it: the five pre-posed vowels. Each is
    * typed and drawn to the left of the consonant it is pronounced after, so parting them across a line break
    * would leave a vowel hanging at the end of one line and its consonant opening the next.
    */
  def noBreakAfter(cp: Int): Boolean = cp >= 0x0e40 && cp <= 0x0e44

  /** Whether a line may break between `a` and `b`: both are Thai and neither binds across the boundary. The
    * breaker treats such a break as a last resort (see the note above), so it is taken only where no break at
    * a phrase space fits.
    */
  def breakableBetween(a: Int, b: Int): Boolean =
    isThai(a) && isThai(b) && !noBreakBefore(b) && !noBreakAfter(a)

  /** Whether `s` carries Thai at all, deciding whether a run needs the per-character break pass. */
  def has(s: String): Boolean =
    var i = 0
    while i < s.length do
      val cp = s.codePointAt(i)
      if isThai(cp) then return true
      i += Character.charCount(cp)
    false
