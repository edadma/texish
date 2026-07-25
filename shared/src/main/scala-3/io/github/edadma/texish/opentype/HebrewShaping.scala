package io.github.edadma.texish.opentype

/** Pointed Hebrew, which needs one substitution and nothing else.
  *
  * Hebrew neither joins nor reorders, so it needs none of the machinery Arabic and the Indic scripts do: the
  * letters come straight from the font's character map and the points are placed on them by the GPOS mark
  * shaper. One thing does have to happen first, and only in some faces. A letter and the point drawn inside
  * it — a dagesh, a mapiq, a shin or sin dot — are two characters in memory but one shape on the page, and a
  * font cut for pointed text may carry that shape as a single glyph, reached through the `ccmp` feature. Such
  * a face gives no anchor for placing the point on its own, so unless the pair is combined the dagesh sets
  * beside its letter rather than inside it.
  *
  * Which pairs combine is the font's business, not the engine's: Ezra SIL, the scholarly face for Biblical
  * Hebrew, combines a letter with its dagesh but leaves an alef and its patah to the mark shaper, while Noto
  * Serif Hebrew combines nothing and places every point by anchor. So the feature itself is the test — a pair
  * it turns into one glyph was meant to be combined, and one it leaves alone was not.
  *
  * The point need not sit next to its letter. Text in canonical order sorts the points by combining class, so
  * `דָּ` stores the qamats (18) before the dagesh (21) though both belong to the dalet. The dagesh still
  * combines, because a point is kept from its letter only by an earlier point of at least its own class —
  * the standard blocking rule for canonical composition — and the qamats, ranking lower, does not block it.
  *
  * A face that combines nothing is left entirely alone, points and order alike. Its output then differs from
  * HarfBuzz's, which moves a letter's own point to sit beside it whether or not anything combines; that
  * ordering was checked against the Noto Hebrew face and makes no difference to the page, each point being
  * placed on the letter by its own anchor.
  */
object HebrewShaping:

  private val Lo = 0x0591 // first Hebrew combining mark
  private val Hi = 0x05c7 // last

  // The canonical combining class of each character from U+0591 to U+05C7 — the accents, then the points from
  // sheva (10) up to the sin dot (25). Zero for the few unassigned slots and for anything outside the range.
  private val cccTable = Array(
    220, 230, 230, 230, 230, 220, 230, 230, 230, 222, 220,
    230, 230, 230, 230, 230, 230, 220, 220, 220, 220, 220,
    220, 230, 230, 220, 230, 230, 222, 228, 230, 10, 11,
    12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21,
    22, 0, 23, 0, 24, 25, 0, 230, 220, 0, 18,
  )

  private def ccc(cp: Int): Int = if cp >= Lo && cp <= Hi then cccTable(cp - Lo) else 0

  /** Whether a run holds Hebrew — the cheap test that keeps every other script off this path. Covers the
    * Hebrew block and the presentation forms a font may map its combined shapes through. */
  def hasHebrew(text: String): Boolean =
    var i = 0
    while i < text.length do
      val cp = text.charAt(i).toInt
      if (cp >= 0x0590 && cp <= 0x05ff) || (cp >= 0xfb1d && cp <= 0xfb4f) then return true
      i += 1
    false

  /** Combine each letter of `cps` with the point drawn inside it, giving the glyphs to draw. `glyphOf` maps a
    * codepoint to the font's nominal glyph, and `ccmp` applies the font's composition feature across a whole
    * run — across the run rather than to a pair, because a rule may look at the letters either side before it
    * fires, as Ezra SIL's does for a lamed and its holam. Returns null when nothing combined, the signal that
    * the run needs no substitution and can take the plain path where the mark shaper places its points. */
  def shape(cps: Array[Int], glyphOf: Int => Int, ccmp: Array[Int] => Array[Int]): Array[Int] =
    val shaped = ccmp(reordered(cps).map(glyphOf))
    if shaped.length != cps.length then shaped else null

  // A letter's points sorted into the order they are drawn in, which is not the order they are stored in.
  // Canonical ordering sorts the points of a syllable by combining class, which interleaves the point drawn
  // inside the letter with the vowels around it — `דָּ` stores its qamats before its dagesh — and a font's
  // composition rule expects the pair adjacent. Sorting by the drawing order below puts a letter's own points
  // first, so the rule matches; the sort is stable, so points of equal rank keep the order they were typed in
  // and every other script's marks, ranking equal at zero, come back untouched.
  private def reordered(cps: Array[Int]): Array[Int] =
    var i   = 0
    var any = false
    while i < cps.length && !any do
      if drawRank(cps(i)) > 0 then any = true
      i += 1
    if !any then cps
    else
      val out = scala.collection.mutable.ArrayBuffer.empty[Int]
      var k   = 0
      while k < cps.length do
        if drawRank(cps(k)) == 0 then
          out += cps(k)             // a letter
          var e = k + 1
          while e < cps.length && drawRank(cps(e)) > 0 do e += 1
          out ++= cps.slice(k + 1, e).sortBy(drawRank) // its points, in drawing order
          k = e
        else
          out += cps(k)             // a point with no letter before it
          k += 1
      out.toArray

  // Where a point sorts among the points of its letter. The order is the one HarfBuzz renders in and differs
  // from the combining classes Unicode stores: the points drawn inside or against the letter — the shin and
  // sin dots, the dagesh, the rafe — come first, then the vowels, and the meteg last. Zero for a letter and
  // for every character of another script, so nothing else is disturbed.
  private def drawRank(cp: Int): Int =
    ccc(cp) match
      case 0  => 0
      case 24 => 10 // shin dot
      case 25 => 11 // sin dot
      case 21 => 12 // dagesh or mapiq
      case 23 => 13 // rafe
      case 19 => 14 // holam
      case 11 => 15 // hataf segol
      case 12 => 16 // hataf patah
      case 13 => 17 // hataf qamats
      case 15 => 18 // tsere
      case 16 => 19 // segol
      case 17 => 20 // patah
      case 18 => 21 // qamats, qamats qatan
      case 10 => 22 // sheva
      case 14 => 23 // hiriq
      case 20 => 24 // qubuts
      case 22 => 25 // meteg
      case c  => c  // the accents, which keep their classes and follow the points
