package io.github.edadma.texish.opentype

/** Arabic cursive joining: deciding which contextual form each letter takes from its neighbours.
  *
  * Arabic (and Persian, Urdu, Pashto, … in the same script) is written in connected forms. A letter is
  * drawn differently depending on whether it joins to the letter before it, after it, both, or neither —
  * its initial, medial, final or isolated form. OpenType fonts carry those forms as glyph substitutions
  * keyed by four features (`init`, `medi`, `fina`, `isol`); this object computes, for each character of a
  * run, which of the four applies, so the GSUB shaper can pick the right substitution.
  *
  * The decision follows the joining rules of the Unicode Standard (Section 9.2): each character has a
  * joining type — Dual-joining, Right-joining, Left-joining, Join-causing, Transparent or Non-joining —
  * and a join forms between two adjacent non-transparent characters when the earlier one joins on its
  * leading (toward-the-next-letter) side and the later one joins on its trailing side. Combining marks
  * are Transparent: they are skipped when looking for a letter's neighbours, so a vowel point between two
  * consonants never breaks their connection.
  *
  * The joining-type table is taken from Unicode's `DerivedJoiningType.txt` (Unicode 17.0.0), restricted to
  * the Arabic blocks: Arabic (U+0600–06FF), Arabic Supplement (U+0750–077F) and Arabic Extended-A/B
  * (U+0870–08FF), plus the zero-width joiner. Any character outside the table is Non-joining, which
  * correctly breaks a connection. The data is pure and font-independent, so it runs and is tested
  * identically on every backend.
  */

/** The contextual form a letter takes from its neighbours, naming the OpenType feature that selects it. */
enum JoiningForm(val feature: String):
  case Isolated extends JoiningForm("isol")
  case Initial  extends JoiningForm("init")
  case Medial   extends JoiningForm("medi")
  case Final    extends JoiningForm("fina")

object ArabicShaping:

  // Joining types, as inclusive codepoint ranges, sorted and non-overlapping. The type letters follow
  // Unicode: D dual, R right, L left, C join-causing, T transparent. Non-joining (U) is the default for
  // anything absent and so is not stored. Generated from DerivedJoiningType.txt 17.0.0, Arabic blocks.
  private val starts: Array[Int] = Array(
    0x0610, 0x061C, 0x0620, 0x0622, 0x0626, 0x0627, 0x0628, 0x0629, 0x062A, 0x062F, 0x0633, 0x0640,
    0x0641, 0x0648, 0x0649, 0x064B, 0x066E, 0x0670, 0x0671, 0x0675, 0x0678, 0x0688, 0x069A, 0x06C0,
    0x06C1, 0x06C3, 0x06CC, 0x06CD, 0x06CE, 0x06CF, 0x06D0, 0x06D2, 0x06D5, 0x06D6, 0x06DF, 0x06E7,
    0x06EA, 0x06EE, 0x06FA, 0x06FF, 0x0750, 0x0759, 0x075C, 0x076B, 0x076D, 0x0771, 0x0772, 0x0773,
    0x0775, 0x0778, 0x077A, 0x0870, 0x0883, 0x0886, 0x0889, 0x088E, 0x088F, 0x0897, 0x08A0, 0x08AA,
    0x08AE, 0x08AF, 0x08B1, 0x08B3, 0x08B9, 0x08BA, 0x08CA, 0x08E3, 0x200D,
  )
  private val ends: Array[Int] = Array(
    0x061A, 0x061C, 0x0620, 0x0625, 0x0626, 0x0627, 0x0628, 0x0629, 0x062E, 0x0632, 0x063F, 0x0640,
    0x0647, 0x0648, 0x064A, 0x065F, 0x066F, 0x0670, 0x0673, 0x0677, 0x0687, 0x0699, 0x06BF, 0x06C0,
    0x06C2, 0x06CB, 0x06CC, 0x06CD, 0x06CE, 0x06CF, 0x06D1, 0x06D3, 0x06D5, 0x06DC, 0x06E4, 0x06E8,
    0x06ED, 0x06EF, 0x06FC, 0x06FF, 0x0758, 0x075B, 0x076A, 0x076C, 0x0770, 0x0771, 0x0772, 0x0774,
    0x0777, 0x0779, 0x077F, 0x0882, 0x0885, 0x0886, 0x088D, 0x088E, 0x088F, 0x089F, 0x08A9, 0x08AC,
    0x08AE, 0x08B0, 0x08B2, 0x08B8, 0x08B9, 0x08C8, 0x08E1, 0x08FF, 0x200D,
  )
  private val types: Array[Char] = Array(
    'T', 'T', 'D', 'R', 'D', 'R', 'D', 'R', 'D', 'R', 'D', 'C',
    'D', 'R', 'D', 'T', 'D', 'T', 'R', 'R', 'D', 'R', 'D', 'R',
    'D', 'R', 'D', 'R', 'D', 'R', 'D', 'R', 'R', 'T', 'T', 'T',
    'T', 'R', 'D', 'D', 'D', 'R', 'D', 'R', 'D', 'R', 'D', 'R',
    'D', 'R', 'D', 'R', 'C', 'D', 'D', 'R', 'D', 'T', 'D', 'R',
    'R', 'D', 'R', 'D', 'R', 'D', 'T', 'T', 'C',
  )

  /** The Unicode joining type of a codepoint as a single letter (D/R/L/C/T), or 'U' (Non-joining) for any
    * character outside the Arabic joining table. A binary search over the sorted ranges. */
  def joiningType(cp: Int): Char =
    var lo = 0
    var hi = starts.length - 1
    while lo <= hi do
      val mid = (lo + hi) >>> 1
      if cp < starts(mid) then hi = mid - 1
      else if cp > ends(mid) then lo = mid + 1
      else return types(mid)
    'U'

  /** A combining mark, transparent to joining — skipped when a letter looks for its neighbours. */
  def isTransparent(cp: Int): Boolean = joiningType(cp) == 'T'

  /** Whether a run contains any letter that participates in cursive joining, and so needs shaping. A run of
    * only non-joining characters (Latin, digits, punctuation) or only marks takes the plain text path. */
  def hasArabic(text: String): Boolean =
    var i = 0
    while i < text.length do
      val cp = text.codePointAt(i)
      joiningType(cp) match
        case 'D' | 'R' | 'L' | 'C' => return true
        case _                     =>
      i += Character.charCount(cp)
    false

  // Whether a joining type connects on its leading side (toward the following letter in memory order) and
  // on its trailing side (toward the preceding letter). Dual joins both ways; right-joining connects only
  // to the preceding letter, left-joining only to the following; a join-causer connects both ways.
  private def joinsLeading(t: Char): Boolean  = t == 'D' || t == 'L' || t == 'C'
  private def joinsTrailing(t: Char): Boolean = t == 'D' || t == 'R' || t == 'C'

  /** Resolve the contextual form of every codepoint in a run, in memory (logical) order. Transparent marks
    * are reported as Isolated — their glyphs are not covered by the form features, so the value is never
    * used; it only keeps the result array parallel to the input. */
  def resolveForms(cps: Array[Int]): Array[JoiningForm] =
    val n     = cps.length
    val jt    = Array.tabulate(n)(i => joiningType(cps(i)))
    val forms = new Array[JoiningForm](n)

    var i = 0
    while i < n do
      if jt(i) == 'T' then forms(i) = JoiningForm.Isolated
      else
        // Nearest non-transparent neighbours on each side.
        var p = i - 1
        while p >= 0 && jt(p) == 'T' do p -= 1
        var q = i + 1
        while q < n && jt(q) == 'T' do q += 1

        val joinsToPrev = p >= 0 && joinsLeading(jt(p)) && joinsTrailing(jt(i))
        val joinsToNext = q < n && joinsLeading(jt(i)) && joinsTrailing(jt(q))

        forms(i) =
          if joinsToPrev && joinsToNext then JoiningForm.Medial
          else if joinsToPrev then JoiningForm.Final
          else if joinsToNext then JoiningForm.Initial
          else JoiningForm.Isolated
      i += 1
    forms
