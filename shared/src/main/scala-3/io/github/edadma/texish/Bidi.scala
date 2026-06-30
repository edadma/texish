package io.github.edadma.texish

/** The Unicode bidirectional character class assigned to a codepoint, restricted to the subset the
  * paragraph algorithm needs. The explicit embedding and isolate initiators (LRE/RLE/LRI/…) are
  * deliberately absent: plain right-to-left prose — Hebrew or Arabic scripture, a name or a number
  * embedded in it — never uses them, and skipping them keeps the resolver small. They can be added
  * when a document genuinely needs nested overrides.
  */
enum BidiClass:
  case L  // strong left-to-right (Latin, Greek, Cyrillic, CJK, …)
  case R  // strong right-to-left (Hebrew letters and punctuation)
  case AL // strong right-to-left Arabic letter
  case EN // European number (ASCII digit)
  case ES // European number separator (+ -)
  case ET // European number terminator (# $ % …)
  case AN // Arabic number (Arabic-Indic digit)
  case CS // common number separator (, . : / …)
  case NSM // non-spacing combining mark
  case BN // boundary-neutral (unused here; reserved)
  case B // paragraph separator
  case S // segment separator (tab)
  case WS // whitespace
  case ON // other neutral (most symbols and brackets)

/** A pure-Scala implementation of the core of the Unicode Bidirectional Algorithm (UAX #9), enough to
  * lay out unpointed Hebrew (and, approximately, Arabic) embedded in left-to-right text. It runs the
  * paragraph-level resolution — base direction (P2/P3), the weak-type rules (W1–W7), neutral
  * resolution including bracket pairs (N0–N2), and implicit levels (I1/I2) — then the per-line reset
  * (L1) and reordering (L2).
  *
  * What it does NOT do is glyph shaping: Hebrew is non-cursive and unpointed Hebrew has no marks, so
  * reordering the characters into visual order and drawing them with the ordinary left-to-right pen is
  * correct. Arabic's joining and Hebrew niqqud positioning are a later, separate concern; the class
  * data for Arabic is carried here only so the number rules behave when an Arabic digit or letter turns
  * up, not as a claim that Arabic is shaped.
  *
  * The algorithm is implemented over a sequence of characters (one [[BidiClass]] each). Explicit
  * embedding and isolate controls are not handled, so the whole paragraph is a single level run: the
  * start-of-sequence and end-of-sequence directions are both the paragraph's base direction, which
  * simplifies every rule that consults a neighbour across the run boundary.
  */
object Bidi:
  import BidiClass.*

  /** Latin, Greek, Cyrillic, CJK and kana are all strong left-to-right; this covers the scripts a
    * mixed Hebrew/Latin document actually puts next to Hebrew. Anything not recognised here and not
    * classified below falls through to a neutral, which the neutral rules resolve from context. */
  private def isStrongLeft(cp: Int): Boolean =
    (cp >= 'A' && cp <= 'Z') || (cp >= 'a' && cp <= 'z') ||
      (cp >= 0x00c0 && cp <= 0x024f) || // Latin-1 letters + Latin Extended-A/B
      (cp >= 0x0370 && cp <= 0x03ff) || // Greek
      (cp >= 0x0400 && cp <= 0x04ff) || // Cyrillic
      (cp >= 0x4e00 && cp <= 0x9fff) || // CJK ideographs
      (cp >= 0x3040 && cp <= 0x30ff)    // kana

  /** The bidirectional class of a codepoint. The Hebrew block (U+0590–05FF) is classified exactly —
    * letters and punctuation as R, the cantillation and vowel points as NSM — because that is the
    * script Phase 1 targets. The Arabic ranges are classified approximately: precise enough for the
    * number and neutral rules, but not a substitute for the real `DerivedBidiClass` table, which a
    * later Arabic phase should fold in. */
  def classify(cp: Int): BidiClass =
    cp match
      case 0x000a | 0x000d | 0x001c | 0x001d | 0x001e | 0x0085 | 0x2029 => B
      case 0x0009 | 0x000b | 0x001f                                     => S
      case 0x000c | 0x0020 | 0x2028                                     => WS
      case _ =>
        // Hebrew block: points and accents are non-spacing marks, the rest is strong R.
        if cp >= 0x0591 && cp <= 0x05bd then NSM
        else if cp == 0x05bf || cp == 0x05c1 || cp == 0x05c2 || cp == 0x05c4 || cp == 0x05c5 || cp == 0x05c7 then NSM
        else if cp >= 0x05be && cp <= 0x05f4 then R
        else if cp >= 0xfb1d && cp <= 0xfb4f then R // Hebrew presentation forms
        // Arabic block, approximate.
        else if cp >= 0x0600 && cp <= 0x0605 then AN
        else if cp == 0x060c then CS
        else if cp == 0x0609 || cp == 0x060a || cp == 0x066a then ET
        else if cp >= 0x0660 && cp <= 0x0669 then AN
        else if cp == 0x066b || cp == 0x066c then AN
        else if cp >= 0x06f0 && cp <= 0x06f9 then EN // extended Arabic-Indic digits
        else if cp >= 0x064b && cp <= 0x065f then NSM
        else if cp == 0x0670 then NSM
        else if cp >= 0x06d6 && cp <= 0x06dc then NSM
        else if cp >= 0x06df && cp <= 0x06e4 then NSM
        else if cp >= 0x06e7 && cp <= 0x06e8 then NSM
        else if cp >= 0x06ea && cp <= 0x06ed then NSM
        else if cp >= 0x0608 && cp <= 0x06ff then AL // remaining Arabic letters/marks lumped as AL
        else if cp >= 0x0750 && cp <= 0x077f then AL // Arabic Supplement
        else if cp >= 0x08a0 && cp <= 0x08ff then AL // Arabic Extended-A
        else if cp >= 0xfb50 && cp <= 0xfdff then AL // Arabic Presentation Forms-A
        else if cp >= 0xfe70 && cp <= 0xfeff then AL // Arabic Presentation Forms-B
        // Common Latin/ASCII numbers, separators and terminators.
        else if cp >= 0x0030 && cp <= 0x0039 then EN
        else if cp == 0x002b || cp == 0x002d then ES
        else if cp == 0x0023 || cp == 0x0024 || cp == 0x0025 || (cp >= 0x00a2 && cp <= 0x00a5) || cp == 0x00b0 || cp == 0x00b1
        then ET
        else if cp == 0x002c || cp == 0x002e || cp == 0x002f || cp == 0x003a || cp == 0x00a0 then CS
        else if isStrongLeft(cp) then L
        else ON

  /** Whether a codepoint is a strong right-to-left character — a Hebrew or Arabic letter. Used to split a
    * word box at a script boundary so each box is a single direction, and to decide whether a paragraph
    * needs bidi processing at all. */
  def isStrongR(cp: Int): Boolean =
    classify(cp) match
      case R | AL => true
      case _      => false

  /** Whether a string contains any strong right-to-left character, the test the paragraph builder uses to
    * skip reordering on the common pure-left-to-right paragraph. */
  def hasRtl(s: String): Boolean =
    var i = 0
    while i < s.length do
      if isStrongR(s.charAt(i).toInt) then return true
      i += 1
    false

  /** Whether a string contains any non-spacing combining mark — Hebrew niqqud or Arabic marks — the test
    * a character box uses to decide whether it must position marks rather than draw the run as a string. */
  def hasMarks(s: String): Boolean =
    var i = 0
    while i < s.length do
      if classify(s.charAt(i).toInt) == NSM then return true
      i += 1
    false

  /** Classify a whole string, one entry per char. Hebrew, Latin and Arabic are all in the Basic
    * Multilingual Plane, so per-char classification aligns one-to-one with the box text the line
    * builder will later hand in; astral codepoints (e.g. math alphanumerics) are not RTL and resolve
    * as neutrals, which is harmless. */
  def classifyString(s: String): Array[BidiClass] =
    val out = new Array[BidiClass](s.length)
    var i   = 0
    while i < s.length do
      out(i) = classify(s.charAt(i).toInt)
      i += 1
    out

  /** The paragraph base level by rules P2/P3: the level of the first strong character — 0 for L, 1 for
    * R or AL — or 0 if the paragraph has no strong character at all. */
  def autoBaseLevel(classes: Array[BidiClass]): Int =
    var i = 0
    while i < classes.length do
      classes(i) match
        case L      => return 0
        case R | AL => return 1
        case _      =>
      i += 1
    0

  // The direction a level reads in: even is left-to-right, odd is right-to-left.
  private def dirOf(level: Int): BidiClass = if (level & 1) == 0 then L else R

  // For the neutral and number rules a resolved character counts as one of two directions; a European
  // or Arabic number counts as R so that digits cling to surrounding right-to-left runs.
  private def neutralDir(t: BidiClass): BidiClass =
    t match
      case L           => L
      case R | EN | AN => R
      case _           => ON // not a directional anchor

  // Neutral-or-isolate types, the characters rules N1/N2 resolve.
  private def isNeutral(t: BidiClass): Boolean =
    t match
      case WS | ON | B | S => true
      case _               => false

  /** Resolve the implicit embedding level of every character: the weak rules W1–W7, the neutral rules
    * N0–N2, then the implicit-level rules I1/I2. The input is the original classes; the returned array
    * holds one level per character. `baseLevel` is the paragraph base (0 or 1); `cps` carries the
    * codepoints so the bracket-pair rule N0 can recognise matching brackets. */
  def resolveLevels(cps: Array[Int], classes: Array[BidiClass], baseLevel: Int): Array[Int] =
    val n     = classes.length
    val types = classes.clone()
    val sos   = dirOf(baseLevel) // single run: start and end directions are the base direction
    val eos   = sos

    // W1: a non-spacing mark takes the type of the previous character, or sos at the start.
    var i = 0
    while i < n do
      if types(i) == NSM then types(i) = (if i == 0 then sos else types(i - 1))
      i += 1

    // W2: a European number becomes Arabic when the last strong type before it is an Arabic letter.
    var lastStrong = sos
    i = 0
    while i < n do
      types(i) match
        case EN if lastStrong == AL    => types(i) = AN
        case t @ (R | L | AL)          => lastStrong = t
        case _                         =>
      i += 1

    // W3: every Arabic letter resolves to plain strong R.
    i = 0
    while i < n do
      if types(i) == AL then types(i) = R
      i += 1

    // W4: a lone separator between two numbers joins them — ES or CS between European numbers, CS
    // between Arabic numbers.
    i = 1
    while i < n - 1 do
      val t = types(i)
      if (t == ES || t == CS) && types(i - 1) == EN && types(i + 1) == EN then types(i) = EN
      else if t == CS && types(i - 1) == AN && types(i + 1) == AN then types(i) = AN
      i += 1

    // W5: a run of European terminators adjacent to a European number becomes European numbers.
    i = 0
    while i < n do
      if types(i) == ET then
        var j = i
        while j < n && types(j) == ET do j += 1
        val before = if i > 0 then types(i - 1) else sos
        val after  = if j < n then types(j) else eos
        if before == EN || after == EN then
          var k = i
          while k < j do { types(k) = EN; k += 1 }
        i = j
      else i += 1

    // W6: any remaining separators and terminators are neutral.
    i = 0
    while i < n do
      types(i) match
        case ES | ET | CS => types(i) = ON
        case _            =>
      i += 1

    // W7: a European number becomes left-to-right when the last strong type before it is L.
    lastStrong = sos
    i = 0
    while i < n do
      types(i) match
        case t @ (L | R) => lastStrong = t
        case EN if lastStrong == L => types(i) = L
        case _ =>
      i += 1

    resolveNeutrals(cps, types, baseLevel, sos, eos)

    // I1/I2: turn resolved directions into levels. At an even base, R steps up one and numbers step up
    // two; at an odd base, L and numbers step up one.
    val levels = Array.fill(n)(baseLevel)
    i = 0
    while i < n do
      val t = types(i)
      if (baseLevel & 1) == 0 then
        t match
          case R           => levels(i) = baseLevel + 1
          case EN | AN     => levels(i) = baseLevel + 2
          case _           =>
      else
        t match
          case L | EN | AN => levels(i) = baseLevel + 1
          case _           =>
      i += 1
    levels

  // N0 (bracket pairs), then N1 (neutrals between equal directions) and N2 (the rest to the embedding
  // direction). Mutates `types` in place.
  private def resolveNeutrals(cps: Array[Int], types: Array[BidiClass], baseLevel: Int, sos: BidiClass, eos: BidiClass): Unit =
    val n = types.length
    val e = dirOf(baseLevel)

    // N0: each matched pair of brackets takes a direction decided by the strong text it encloses. A
    // strong type matching the embedding direction wins outright; otherwise an enclosed opposite
    // strong type wins only when the context before the pair already leans that way.
    for (open, close) <- bracketPairs(cps, types) do
      var hasEmbedding = false
      var hasOpposite  = false
      var k            = open + 1
      while k < close do
        val d = neutralDir(types(k))
        if d == e then hasEmbedding = true
        else if d == L || d == R then hasOpposite = true
        k += 1
      val resolved: Option[BidiClass] =
        if hasEmbedding then Some(e)
        else if hasOpposite then
          // Direction established before the opening bracket (scanning back to the first strong, sos
          // at the start).
          var ctx = sos
          var b   = open - 1
          var done = false
          while b >= 0 && !done do
            val d = neutralDir(types(b))
            if d == L || d == R then { ctx = d; done = true }
            b -= 1
          val opposite = if e == L then R else L
          if ctx == opposite then Some(opposite) else Some(e)
        else None
      resolved.foreach { dir =>
        types(open) = dir
        types(close) = dir
      }

    // N1: a run of neutrals bordered on both sides by the same direction takes that direction. Numbers
    // count as R on either border; the run boundary uses sos/eos.
    var i = 0
    while i < n do
      if isNeutral(types(i)) then
        var j = i
        while j < n && isNeutral(types(j)) do j += 1
        val before = if i > 0 then neutralDir(types(i - 1)) else sos
        val after  = if j < n then neutralDir(types(j)) else eos
        if (before == L || before == R) && before == after then
          var k = i
          while k < j do { types(k) = before; k += 1 }
        i = j
      else i += 1

    // N2: neutrals with no agreeing context take the embedding direction.
    i = 0
    while i < n do
      if isNeutral(types(i)) then types(i) = e
      i += 1

  // The canonical right partner of an opening bracket, for the small set that occurs in running text.
  private def matchingBracket(cp: Int): Int =
    cp match
      case 0x0028 => 0x0029 // ( )
      case 0x005b => 0x005d // [ ]
      case 0x007b => 0x007d // { }
      case 0x2329 => 0x232a // 〈 〉
      case 0x3008 => 0x3009 // 〈 〉 CJK
      case 0x300a => 0x300b // 《 》
      case 0x301a => 0x301b // 〚 〛
      case _      => -1

  // Bracket-pair identification (BD16): a depth-bounded stack matches each closing bracket to the
  // nearest still-open bracket of the right kind. Only characters still typed ON are eligible, so a
  // bracket the weak rules already reclassified is left alone. Pairs are returned ordered by opening
  // position, which is the order N0 wants.
  private def bracketPairs(cps: Array[Int], types: Array[BidiClass]): Seq[(Int, Int)] =
    if cps.length != types.length then return Seq.empty
    val opens  = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)] // (expected-close codepoint, index)
    val pairs  = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
    var i      = 0
    while i < cps.length do
      if types(i) == ON then
        val cp    = cps(i)
        val close = matchingBracket(cp)
        if close >= 0 && opens.length < 63 then opens.append((close, i))
        else
          // Is this a closing bracket that matches something open?
          var s = opens.length - 1
          var found = -1
          while s >= 0 && found < 0 do
            if opens(s)._1 == cp then found = s
            s -= 1
          if found >= 0 then
            pairs.append((opens(found)._2, i))
            opens.remove(found, opens.length - found) // discard the match and everything above it
      i += 1
    pairs.sortBy(_._1).toSeq

  /** Reorder a line whose characters already carry resolved levels into visual order, returning a
    * permutation where entry v is the logical index to draw at visual position v. Applies L1 — trailing
    * whitespace and segment/paragraph separators (and the whitespace before them) snap back to the base
    * level so they sit on the correct edge — then L2's run reversals. `origClasses` are the original,
    * pre-resolution classes L1 requires. */
  def reorderLine(levels: Array[Int], origClasses: Array[BidiClass], baseLevel: Int): Array[Int] =
    val adjusted = levels.clone()
    applyL1(adjusted, origClasses, baseLevel)
    reorderByLevels(adjusted)

  // L1: reset to the base level every segment/paragraph separator, the whitespace preceding one, and
  // the whitespace at the very end of the line. Uses the original classes, per spec.
  private def applyL1(levels: Array[Int], origClasses: Array[BidiClass], baseLevel: Int): Unit =
    val n        = levels.length
    var runStart = -1
    var i        = 0
    while i < n do
      origClasses(i) match
        case WS =>
          if runStart < 0 then runStart = i
        case B | S =>
          if runStart >= 0 then
            var k = runStart
            while k < i do { levels(k) = baseLevel; k += 1 }
          levels(i) = baseLevel
          runStart = -1
        case _ =>
          runStart = -1
      i += 1
    if runStart >= 0 then
      var k = runStart
      while k < n do { levels(k) = baseLevel; k += 1 }

  /** L2 reordering over an array of levels: from the highest level down to the lowest odd level, reverse
    * every contiguous run of positions at that level or above. Returns the visual-to-logical permutation.
    * Exposed for the line builder, which reorders boxes by their assigned levels. */
  def reorderByLevels(levels: Array[Int]): Array[Int] =
    val n     = levels.length
    val order = Array.range(0, n)
    if n == 0 then return order
    var maxLevel  = 0
    var lowestOdd = Int.MaxValue
    var i         = 0
    while i < n do
      val l = levels(i)
      if l > maxLevel then maxLevel = l
      if (l & 1) == 1 && l < lowestOdd then lowestOdd = l
      i += 1
    if lowestOdd == Int.MaxValue then return order // all even: already in visual order
    var level = maxLevel
    while level >= lowestOdd do
      i = 0
      while i < n do
        if levels(i) >= level then
          var j = i
          while j < n && levels(j) >= level do j += 1
          reverseSlice(order, i, j - 1)
          i = j
        else i += 1
      level -= 1
    order

  private def reverseSlice(a: Array[Int], from: Int, to: Int): Unit =
    var lo = from
    var hi = to
    while lo < hi do
      val t = a(lo)
      a(lo) = a(hi)
      a(hi) = t
      lo += 1
      hi -= 1

  /** The resolved embedding level of every character of `s` under base direction `base`, after the
    * paragraph rules and the per-line reset (L1). The paragraph builder reads these to decide which
    * characters are right-to-left (an odd level) for mirroring, and which level a grapheme cluster takes. */
  def resolvedLevels(s: String, base: Int): Array[Int] =
    val classes = classifyString(s)
    val cps     = new Array[Int](s.length)
    var i       = 0
    while i < s.length do { cps(i) = s.charAt(i).toInt; i += 1 }
    val levels = resolveLevels(cps, classes, base)
    applyL1(levels, classes, base)
    levels

  /** Whether `level` reads right-to-left (an odd embedding level). */
  def isRtlLevel(level: Int): Boolean = (level & 1) == 1

  /** The mirror image of a bracket or relation that the Unicode algorithm depicts mirrored in a
    * right-to-left context (UAX #9 rule L4 / the Bidi_Mirroring property): an opening parenthesis is drawn
    * as a closing one, a less-than as a greater-than, and so on. Characters without a mirror return
    * unchanged. The set is the punctuation and relations that occur in running text; it is not the full
    * Bidi_Mirroring table. */
  def mirror(cp: Int): Int =
    cp match
      case 0x0028 => 0x0029; case 0x0029 => 0x0028 // ( )
      case 0x005b => 0x005d; case 0x005d => 0x005b // [ ]
      case 0x007b => 0x007d; case 0x007d => 0x007b // { }
      case 0x003c => 0x003e; case 0x003e => 0x003c // < >
      case 0x00ab => 0x00bb; case 0x00bb => 0x00ab // « »
      case 0x2039 => 0x203a; case 0x203a => 0x2039 // ‹ ›
      case 0x2264 => 0x2265; case 0x2265 => 0x2264 // ≤ ≥
      case 0x2266 => 0x2267; case 0x2267 => 0x2266
      case 0x226a => 0x226b; case 0x226b => 0x226a // ≪ ≫
      case 0x2308 => 0x2309; case 0x2309 => 0x2308 // ⌈ ⌉
      case 0x230a => 0x230b; case 0x230b => 0x230a // ⌊ ⌋
      case 0x2329 => 0x232a; case 0x232a => 0x2329 // 〈 〉
      case 0x3008 => 0x3009; case 0x3009 => 0x3008
      case 0x300a => 0x300b; case 0x300b => 0x300a // 《 》
      case 0x301a => 0x301b; case 0x301b => 0x301a // 〚 〛
      case _      => cp

  /** Convenience for tests and the character-level path: classify `s`, pick or accept a base level,
    * resolve, and reorder, returning the visual-to-logical permutation. A `forcedBase` of Some(0|1)
    * overrides the automatic P2/P3 base direction. */
  def visualOrder(s: String, forcedBase: Option[Int] = None): Array[Int] =
    val classes = classifyString(s)
    val base    = forcedBase.getOrElse(autoBaseLevel(classes))
    val cps     = new Array[Int](s.length)
    var i       = 0
    while i < s.length do { cps(i) = s.charAt(i).toInt; i += 1 }
    val levels = resolveLevels(cps, classes, base)
    reorderLine(levels, classes, base)

  /** The visual string produced by reordering `s` — its characters in display order. A convenience for
    * tests and for sanity-checking; the real layout reorders boxes, not characters. */
  def visualString(s: String, forcedBase: Option[Int] = None): String =
    val order = visualOrder(s, forcedBase)
    val sb    = new StringBuilder(s.length)
    var i     = 0
    while i < order.length do { sb.append(s.charAt(order(i))); i += 1 }
    sb.toString
