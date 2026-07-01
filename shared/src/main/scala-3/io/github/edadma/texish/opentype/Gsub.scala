package io.github.edadma.texish.opentype

/** A reader for the OpenType `GSUB` table's glyph substitutions — enough to shape Arabic by selecting each
  * letter's contextual form. Arabic letters are drawn in initial, medial, final or isolated shapes; an
  * OpenType font carries those shapes as substitution glyphs reached through four features, `init`, `medi`,
  * `fina` and `isol`. [[ArabicShaping]] decides which form each letter takes from its neighbours, and this
  * applies the matching feature to swap the nominal glyph for the shaped one.
  *
  * Four substitution kinds are read. Single substitution (type 1) maps one glyph to one — the form
  * features are built from these. Multiple substitution (type 2) maps one glyph to a sequence — `ccmp`
  * uses it to split a dotted letter into a dotless skeleton plus a separate dot. Context (type 5) and
  * chaining-context (type 6) substitution match a glyph sequence, optionally with surrounding context, and
  * apply nested lookups at chosen positions; the required-ligature feature `rlig` is built from these. In
  * this font the lam-alef ligature is not a one-to-one ligature glyph but a contextual pair: where an
  * initial lam meets a final alef, each is swapped for a specially shaped `.rlig` variant that together
  * draw the ligature. Extension lookups (type 7) are followed to the real subtable. Only the coverage-based
  * format 3 of the contextual kinds is read — the one this font (and modern Arabic fonts generally) uses.
  *
  * Unlike the mark positioning in [[Gpos]], which can run every lookup blindly, form selection must be
  * driven by feature: the same font has separate `init`/`medi`/`fina`/`isol` lookups and only the one
  * matching a letter's resolved form may apply to it. So this parses the ScriptList and FeatureList to map
  * each feature tag to its lookups, choosing the Arabic script's default language system.
  *
  * The parser is pure: it consumes the raw `GSUB` bytes a backend hands back through `sfntTable`, so it
  * runs identically on every platform, including the in-browser build with no system font engine.
  */

/** One parsed substitution subtable. Only the kinds the Arabic shaper needs are represented; other lookup
  * types parse to nothing and are skipped. Single substitution maps one glyph to one; multiple
  * substitution maps one glyph to a sequence — used both for `ccmp` decompositions (a dotted letter splits
  * into a dotless skeleton plus a separate dot mark) and, with a one-glyph sequence, for the contextual
  * form features in fonts that build them as multiple substitutions.
  *
  * Ligature substitution replaces a run of glyphs with one. `ligatures` maps a first-component glyph to the
  * ligatures starting with it, each a (remaining-components, ligature-glyph) pair — the lam-lam-heh and
  * alef-lam-lam-heh forms of the word "Allah", and the shadda+vowel marks the composition step fuses.
  *
  * Context and chaining-context substitution match a run of glyphs by coverage and apply nested lookups at
  * positions within the match. `Context` matches the input run alone; `ChainContext` also requires the
  * `backtrack` glyphs (in reverse, immediately before the input) and the `lookahead` glyphs (immediately
  * after). Each `record` is a (position-within-input, lookup-index) pair: it runs that lookup on the glyph
  * at that position; a nested ligature may shorten the run, which is how the Arabic "Allah" composition
  * fuses an adjacent shadda and dagger-alef into one mark before the outer ligature fires. */
private sealed trait SubstSubtable
private final case class SingleSubst(map: Map[Int, Int])          extends SubstSubtable
private final case class MultipleSubst(map: Map[Int, Array[Int]]) extends SubstSubtable
private final case class LigatureSubst(ligatures: Map[Int, Array[(Array[Int], Int)]]) extends SubstSubtable
private final case class ContextSubst(input: Array[Set[Int]], records: Array[(Int, Int)]) extends SubstSubtable
private final case class ChainContextSubst(
    backtrack: Array[Set[Int]],
    input: Array[Set[Int]],
    lookahead: Array[Set[Int]],
    records: Array[(Int, Int)],
) extends SubstSubtable

object Gsub:
  // The contextual-form features, one per JoiningForm.
  private val FormFeatures = Set("init", "medi", "fina", "isol")

  // Composition features applied before form selection, in order: glyph composition/decomposition (which
  // splits a dotted letter into skeleton + dot) and localized forms. Run ahead of init/medi/fina so the
  // form applies to the resulting skeleton.
  private val PreFeatures = Seq("ccmp", "locl")

  // Features applied after form selection, in order: required ligatures, then standard ligatures. `rlig`
  // forms the lam-alef pair by substituting the connected lam and alef for their `.rlig` variants; `liga`
  // forms the calligraphic "Allah" ligature (and any other standard ligatures) from the already-shaped
  // glyphs, so its lookups match the contextual forms init/medi/fina produced. Discretionary ligatures
  // (`dlig`) are left off, as in conventional typesetting.
  private val PostFeatures = Seq("rlig", "liga")

  /** Build an Arabic shaper from a font's raw `GSUB` bytes, or None when the font has no Arabic form
    * features (so the caller keeps the plain text path). */
  def from(gsub: Option[Array[Byte]]): Option[Gsub] =
    gsub.flatMap { data =>
      val g = new Gsub(data, Seq("arab"))
      if g.hasFormSubstitution then Some(g) else None
    }

  /** Build an Indic shaper from a font's raw `GSUB` bytes for the script whose OpenType tags are `scriptTags`
    * (Devanagari passes `dev2`, `deva`; Bengali passes `bng2`, `beng`), or None when the font carries none of
    * those script tables — a font that does not shape the script keeps the plain text path. The same subtable
    * parsing and lookup machinery the Arabic shaper uses is reused here; the Indic reordering and feature order
    * live in `io.github.edadma.texish.opentype.IndicShaper`, which drives this by feature name. */
  def fromIndic(gsub: Option[Array[Byte]], scriptTags: Seq[String]): Option[Gsub] =
    gsub.flatMap { data =>
      val g = new Gsub(data, scriptTags)
      if g.boundToRequestedScript then Some(g) else None
    }

/** Parses the language-system feature lookups of `data` (a `GSUB` table) on construction, binding to the
  * first of `scriptTags` the font carries (Arabic passes `arab`; the Indic shaper passes `dev2`, `deva`). */
final class Gsub(data: Array[Byte], scriptTags: Seq[String]):

  // Lookups parsed by their index in the LookupList; an unparsed/irrelevant lookup yields an empty vector.
  private val lookups: Array[Vector[SubstSubtable]] =
    if data.length < 10 then Array.empty
    else
      val c = ByteCursor(data, 0)
      c.u16; c.u16 // major, minor version
      c.u16        // scriptList offset (read below)
      c.u16        // featureList offset (read below)
      val lookupListOff = c.u16
      parseLookupList(lookupListOff)

  // The GSUB script table this shaper bound to (the first of `scriptTags` present, else a default script),
  // recorded so a caller can tell an Indic font from one that only matched a default script. Set while
  // parseFeatureMap runs during construction, so it must be declared before the feature map it feeds.
  private var chosenScriptTag: Option[String] = None

  // Feature tag → the lookup-list indices that feature triggers, for the chosen script's default language
  // system.
  private val featureLookups: Map[String, Array[Int]] = parseFeatureMap()

  /** Whether the font carries at least one of the Arabic form features worth running. */
  def hasFormSubstitution: Boolean = Gsub.FormFeatures.exists(featureLookups.contains)

  /** Whether the shaper bound to one of its requested script tables, rather than falling back to a default
    * script — the signal that this font actually shapes the script it was built for (Indic text). */
  def boundToRequestedScript: Boolean = chosenScriptTag.exists(scriptTags.contains)

  /** Whether the chosen script's language system enables `tag`. */
  def hasFeature(tag: String): Boolean = featureLookups.contains(tag)

  /** Apply the lookups of feature `tag`, in order, across the whole glyph buffer, returning the substituted
    * run (or the input unchanged when the font lacks the feature). This is how the Indic shaper runs the
    * Devanagari basic-form and presentation features by name; Arabic drives its features through `shape`. */
  def applyFeatureByTag(glyphs: Array[Int], tag: String): Array[Int] =
    featureLookups.get(tag).map(idxs => applyFeature(glyphs, idxs)).getOrElse(glyphs)

  /** Apply feature `tag` to a single glyph position, returning the run with only that glyph possibly
    * substituted. This is how the Indic shaper runs the word-position features `init` and `fina`: those select
    * a distinct form of a pre-base or post-base vowel sign only when it is the first or last glyph of the word,
    * so the feature must not fire on the same sign elsewhere in the word. The features are single
    * substitutions, so the position is shaped on its own; a feature the font lacks, or one that does not cover
    * the glyph, leaves the run unchanged. */
  def applyFeatureByTagAt(glyphs: Array[Int], tag: String, index: Int): Array[Int] =
    if index < 0 || index >= glyphs.length then glyphs
    else
      val sub = applyFeatureByTag(Array(glyphs(index)), tag)
      if sub.length == 1 && sub(0) != glyphs(index) then glyphs.updated(index, sub(0)) else glyphs

  /** Shape a run of nominal glyphs (one per character, the font's cmap result) into the glyphs to draw,
    * given each character's resolved joining form. Composition substitutions run first — `ccmp` (and
    * `locl`): a dotted letter splits into a dotless skeleton plus a separate dot mark, and adjacent marks
    * such as a shadda and a dagger-alef fuse into one — each produced glyph inheriting a form. Then every
    * glyph is swapped for its contextual form (init/medi/fina/isol). Finally the ligature features run:
    * `rlig` forms the lam-alef pair, and `liga` the calligraphic "Allah". Marks fall through the form pass
    * unchanged (no form feature covers them) and are positioned afterwards by the GPOS mark shaper. */
  def shape(glyphs: Array[Int], forms: Array[JoiningForm]): Array[Int] =
    var gs = glyphs
    var fs = forms
    for tag <- Gsub.PreFeatures do
      featureLookups.get(tag).foreach { idxs =>
        val (g2, f2) = applyComposition(gs, fs, idxs)
        gs = g2; fs = f2
      }
    var out = Array.tabulate(gs.length)(k => substituteForm(gs(k), fs(k)))
    for tag <- Gsub.PostFeatures do
      featureLookups.get(tag).foreach(idxs => out = applyFeature(out, idxs))
    out

  /** Substitute a glyph for its shaped form, applying the feature named by `form`. Each lookup the feature
    * triggers is tried in order; a substitution subtable that covers the glyph replaces it (a form feature
    * substitutes one glyph for one, whether the font builds it as a single or a one-element multiple). A
    * glyph no lookup covers (or a feature the font lacks) is returned unchanged. */
  def substituteForm(glyph: Int, form: JoiningForm): Int =
    featureLookups.get(form.feature) match
      case None => glyph
      case Some(idxs) =>
        var g = glyph
        var k = 0
        while k < idxs.length do
          applyOne(idxs(k), g).foreach(outs => if outs.nonEmpty then g = outs(0))
          k += 1
        g

  // Apply the composition features (`ccmp`, `locl`) to a run carrying each character's joining form, using
  // the same per-position lookup machinery as the post-form features so contextual mark composition and
  // ligatures run too. Each produced glyph inherits a form: a one-to-many split (a dotted letter into a
  // skeleton plus a dot) and a single substitution keep the source form; a context match keeps each matched
  // glyph's own form; a ligature's one glyph takes the first component's form. Forms only steer the form
  // features that follow, which act on letters — and letters here flow only through single/multiple
  // substitution, so the marks a composition fuses or rewrites never need an exact form.
  private def applyComposition(
      glyphs: Array[Int],
      forms: Array[JoiningForm],
      idxs: Array[Int],
  ): (Array[Int], Array[JoiningForm]) =
    var gs = glyphs
    var fs = forms
    for li <- idxs do
      val og = scala.collection.mutable.ArrayBuffer.empty[Int]
      val of = scala.collection.mutable.ArrayBuffer.empty[JoiningForm]
      var i  = 0
      while i < gs.length do
        applyLookupAt(gs, i, li) match
          case Some((rep, consumed, perInput)) =>
            var k = 0
            while k < rep.length do
              og += rep(k)
              of += (if perInput && i + k < gs.length then fs(i + k) else fs(i))
              k += 1
            i += consumed
          case None =>
            og += gs(i); of += fs(i); i += 1
      gs = og.toArray; fs = of.toArray
    (gs, fs)

  // The substitution one lookup makes to a single glyph: the replacement glyph sequence, or None if no
  // subtable of the lookup covers the glyph. Used by the form features, which substitute one glyph for one
  // (or one-element many) and never need a ligature or the surrounding run.
  private def applyOne(li: Int, g: Int): Option[Array[Int]] =
    var res: Option[Array[Int]] = None
    val sts = lookups(li)
    var i   = 0
    while res.isEmpty && i < sts.length do
      sts(i) match
        case SingleSubst(m)   => m.get(g).foreach(x => res = Some(Array(x)))
        case MultipleSubst(m) => m.get(g).foreach(x => res = Some(x))
        case _                => // ligature/contextual subtables are not used by the form features
      i += 1
    res

  // Apply a feature's lookups, in order, across the whole glyph buffer. Unlike the form features (driven
  // per glyph by its resolved joining form) and the composition pass (a glyph at a time), a feature like
  // `rlig` can match a run of glyphs in context, so each lookup scans the buffer left to right.
  private def applyFeature(glyphs: Array[Int], idxs: Array[Int]): Array[Int] =
    var cur = glyphs
    for li <- idxs do cur = applyLookupOverBuffer(cur, li)
    cur

  // Run one lookup across the buffer: at each position try to apply it; on a match emit the replacement and
  // skip past the glyphs it consumed, otherwise copy the glyph through and advance by one.
  private def applyLookupOverBuffer(glyphs: Array[Int], li: Int): Array[Int] =
    val out = scala.collection.mutable.ArrayBuffer.empty[Int]
    var i   = 0
    while i < glyphs.length do
      applyLookupAt(glyphs, i, li) match
        case Some((rep, consumed, _)) => out ++= rep; i += consumed
        case None                     => out += glyphs(i); i += 1
    out.toArray

  // Try a lookup at one buffer position. Single, multiple and ligature substitution act on the glyph (and,
  // for a ligature, the glyphs that follow); context and chaining-context substitution match a run (with
  // backtrack/lookahead for the chaining kind) and return the run with their nested lookups applied. The
  // result is the replacement glyphs, the number of input glyphs consumed, and whether each replacement
  // glyph carries its own input position's form (true for a context run, false when one source glyph drives
  // the whole replacement); or None if no subtable matches here.
  private def applyLookupAt(glyphs: Array[Int], i: Int, li: Int): Option[(Array[Int], Int, Boolean)] =
    val sts = lookups(li)
    var res: Option[(Array[Int], Int, Boolean)] = None
    var s   = 0
    while res.isEmpty && s < sts.length do
      sts(s) match
        case SingleSubst(m)   => m.get(glyphs(i)).foreach(x => res = Some((Array(x), 1, false)))
        case MultipleSubst(m) => m.get(glyphs(i)).foreach(x => res = Some((x, 1, false)))
        case LigatureSubst(l) => tryLigature(glyphs, i, l).foreach((lig, n) => res = Some((Array(lig), n, false)))
        case ContextSubst(input, records) =>
          if matchRun(glyphs, i, input) then
            res = Some((applyRecords(glyphs, i, input.length, records), input.length, true))
        case ChainContextSubst(bt, input, la, records) =>
          if matchBacktrack(glyphs, i, bt) && matchRun(glyphs, i, input) &&
            matchRun(glyphs, i + input.length, la)
          then res = Some((applyRecords(glyphs, i, input.length, records), input.length, true))
      s += 1
    res

  // Whether `covs` matches the glyphs starting at `start`, one coverage set per glyph in order.
  private def matchRun(glyphs: Array[Int], start: Int, covs: Array[Set[Int]]): Boolean =
    if start < 0 || start + covs.length > glyphs.length then false
    else
      var j  = 0
      var ok = true
      while ok && j < covs.length do
        if !covs(j).contains(glyphs(start + j)) then ok = false
        j += 1
      ok

  // Whether the backtrack coverages match the glyphs immediately before `i`. Backtrack is given in reverse
  // text order: `bt(0)` is the glyph just before the input, `bt(1)` the one before that, and so on.
  private def matchBacktrack(glyphs: Array[Int], i: Int, bt: Array[Set[Int]]): Boolean =
    if i - bt.length < 0 then false
    else
      var j  = 0
      var ok = true
      while ok && j < bt.length do
        if !bt(j).contains(glyphs(i - 1 - j)) then ok = false
        j += 1
      ok

  // Apply a context match's nested lookups: copy the matched input run, then for each (position, lookup)
  // record run that lookup on the glyph at that position. A record may substitute one glyph for one (the lam
  // and the alef each take their `.rlig` form) or ligate adjacent glyphs (a shadda and a following
  // dagger-alef into one composed mark), so the run can shorten; later records read the run as it then
  // stands. Returns the resulting run; the caller still advances by the original input length.
  private def applyRecords(glyphs: Array[Int], i: Int, inputLen: Int, records: Array[(Int, Int)]): Array[Int] =
    var run = Array.tabulate(inputLen)(k => glyphs(i + k))
    for (seqIdx, lookupIdx) <- records if seqIdx >= 0 && seqIdx < run.length do
      applyNestedAt(run, seqIdx, lookupIdx) match
        case Some((rep, consumed)) =>
          run = run.slice(0, seqIdx) ++ rep ++ run.slice(seqIdx + consumed, run.length)
        case None =>
    run

  // The substitution a nested lookup makes at one position in a run: a single (one glyph), multiple (one to
  // many) or ligature (many to one) replacement and the count of glyphs it consumed, or None if the lookup
  // does not apply. Nested context lookups are not followed (none of the Arabic lookups nest one).
  private def applyNestedAt(run: Array[Int], pos: Int, li: Int): Option[(Array[Int], Int)] =
    val sts = lookups(li)
    var res: Option[(Array[Int], Int)] = None
    var s   = 0
    while res.isEmpty && s < sts.length do
      sts(s) match
        case SingleSubst(m)   => m.get(run(pos)).foreach(x => res = Some((Array(x), 1)))
        case MultipleSubst(m) => m.get(run(pos)).foreach(x => res = Some((x, 1)))
        case LigatureSubst(l) => tryLigature(run, pos, l).foreach((lig, n) => res = Some((Array(lig), n)))
        case _                =>
      s += 1
    res

  // The ligature a lookup forms starting at `i`, if any: the first matching ligature whose remaining
  // components equal the glyphs that follow, as the ligature glyph and the number of glyphs it consumes
  // (including the first). Components match exactly and in order — the Arabic ligatures include a composed
  // mark as a component, so marks are not skipped.
  private def tryLigature(glyphs: Array[Int], i: Int, ligatures: Map[Int, Array[(Array[Int], Int)]]): Option[(Int, Int)] =
    ligatures.get(glyphs(i)) match
      case None => None
      case Some(set) =>
        var res: Option[(Int, Int)] = None
        var k   = 0
        while res.isEmpty && k < set.length do
          val (tail, lig) = set(k)
          if matchExact(glyphs, i + 1, tail) then res = Some((lig, tail.length + 1))
          k += 1
        res

  // Whether `comps` equals the glyphs starting at `start`, exactly and in order.
  private def matchExact(glyphs: Array[Int], start: Int, comps: Array[Int]): Boolean =
    if start + comps.length > glyphs.length then false
    else
      var j  = 0
      var ok = true
      while ok && j < comps.length do
        if glyphs(start + j) != comps(j) then ok = false
        j += 1
      ok

  // ─── parsing ────────────────────────────────────────────────────────────────

  // A four-character OpenType tag (e.g. "init") from a big-endian u32.
  private def tag4(t: Long): String =
    String(Array(((t >> 24) & 0xff).toChar, ((t >> 16) & 0xff).toChar, ((t >> 8) & 0xff).toChar, (t & 0xff).toChar))

  // Map every feature tag used by the chosen script's default language system to the lookups it triggers.
  // The featureList holds (tag, lookups) for every feature; a language system selects a subset of them by
  // index. The script is chosen from the shaper's `scriptTags` (see langSysFeatureIndices).
  private def parseFeatureMap(): Map[String, Array[Int]] =
    if data.length < 10 then return Map.empty
    val c = ByteCursor(data, 0)
    c.u16; c.u16 // version
    val scriptListOff  = c.u16
    val featureListOff = c.u16

    val featureIndices = langSysFeatureIndices(scriptListOff)
    if featureIndices.isEmpty || featureListOff == 0 then return Map.empty

    val fc        = ByteCursor(data, featureListOff)
    val featCount = fc.u16
    val records   = Array.fill(featCount) { val tag = tag4(fc.u32); val off = fc.u16; (tag, off) }

    val out = scala.collection.mutable.Map.empty[String, scala.collection.mutable.ArrayBuffer[Int]]
    for fi <- featureIndices if fi < records.length do
      val (tag, off) = records(fi)
      val l          = ByteCursor(data, featureListOff + off)
      l.u16 // featureParams offset
      val n          = l.u16
      val idxs       = out.getOrElseUpdate(tag, scala.collection.mutable.ArrayBuffer.empty)
      for _ <- 0 until n do idxs += l.u16
    out.map((k, v) => k -> v.toArray).toMap

  // The feature indices the chosen script's default language system enables, including its required
  // feature if it names one. The shaper's own script tags are preferred in order, then the default scripts,
  // then whatever the font lists first; the chosen tag is recorded so callers can identify the script.
  private def langSysFeatureIndices(scriptListOff: Int): Array[Int] =
    if scriptListOff == 0 then return Array.empty
    val c           = ByteCursor(data, scriptListOff)
    val scriptCount = c.u16
    val records     = Array.fill(scriptCount) { val tag = tag4(c.u32); val off = c.u16; (tag, off) }

    val prefs  = scriptTags ++ Seq("DFLT", "dflt")
    val chosen = prefs.iterator.map(t => records.find(_._1 == t)).collectFirst { case Some(r) => r }
      .orElse(records.headOption)
    chosenScriptTag = chosen.map(_._1)

    chosen match
      case None => Array.empty
      case Some((_, scriptOff)) =>
        val s                 = ByteCursor(data, scriptListOff + scriptOff)
        val defaultLangSysOff = s.u16
        if defaultLangSysOff == 0 then Array.empty
        else
          val ls = ByteCursor(data, scriptListOff + scriptOff + defaultLangSysOff)
          ls.u16 // lookupOrder (reserved)
          val required = ls.u16
          val n        = ls.u16
          val idxs     = Array.fill(n)(ls.u16)
          if required != 0xffff then idxs :+ required else idxs

  // Parse the whole LookupList, keeping the substitution kinds the shaper understands so the array stays
  // index-aligned with the lookup indices the feature map refers to.
  private def parseLookupList(lookupListOff: Int): Array[Vector[SubstSubtable]] =
    if lookupListOff == 0 then return Array.empty
    val lc        = ByteCursor(data, lookupListOff)
    val lookupCnt = lc.u16
    val offsets   = Array.fill(lookupCnt)(lookupListOff + lc.u16)
    offsets.map(parseLookup)

  private def parseLookup(off: Int): Vector[SubstSubtable] =
    val l          = ByteCursor(data, off)
    val lookupType = l.u16
    l.u16 // lookupFlag
    val subCount = l.u16
    val subs     = Array.fill(subCount)(off + l.u16)
    subs.toVector.flatMap(so => parseSubtable(lookupType, so))

  // Dispatch one subtable by lookup type, following an extension (type 7) to the wrapped subtable. Single
  // (type 1), multiple (type 2), ligature (type 4), context (type 5) and chaining-context (type 6)
  // substitution are materialised — together they cover Arabic form selection, ccmp composition, the rlig
  // lam-alef pair and the liga "Allah" ligature; other types, and the older formats 1/2 of the contextual
  // kinds, are skipped.
  private def parseSubtable(lookupType: Int, off: Int): Option[SubstSubtable] =
    lookupType match
      case 1 => Some(parseSingle(off))
      case 2 => Some(parseMultiple(off))
      case 4 => Some(parseLigature(off))
      case 5 => parseContext(off)
      case 6 => parseChainContext(off)
      case 7 =>
        val c       = ByteCursor(data, off)
        c.u16 // substFormat (1)
        val extType = c.u16
        val extOff  = off + c.u32.toInt
        parseSubtable(extType, extOff)
      case _ => None

  private def parseSingle(off: Int): SingleSubst =
    val c      = ByteCursor(data, off)
    val format = c.u16
    val covOff = off + c.u16
    val cov    = Coverage.parse(data, covOff)
    format match
      case 1 =>
        val delta = c.i16
        SingleSubst(cov.map((g, _) => g -> ((g + delta) & 0xffff)))
      case 2 =>
        val count = c.u16
        val subst = Array.fill(count)(c.u16)
        SingleSubst(cov.collect { case (g, i) if i < subst.length => g -> subst(i) })
      case _ => SingleSubst(Map.empty)

  // MultipleSubstFormat1: a coverage and, per covered glyph, the sequence of glyphs it expands to.
  private def parseMultiple(off: Int): MultipleSubst =
    val c        = ByteCursor(data, off)
    c.u16 // substFormat (1)
    val covOff   = off + c.u16
    val seqCount = c.u16
    val seqOffs  = Array.fill(seqCount)(off + c.u16)
    val cov      = Coverage.parse(data, covOff)
    val seqs = seqOffs.map { so =>
      val s = ByteCursor(data, so)
      val n = s.u16
      Array.fill(n)(s.u16)
    }
    MultipleSubst(cov.collect { case (g, i) if i < seqs.length => g -> seqs(i) })

  // LigatureSubstFormat1: a coverage of first components and, per covered glyph, the ligatures starting with
  // it. Each ligature carries its glyph and its components; the first component is the coverage glyph, so
  // only the rest (the "tail") is stored alongside the count needed to match it.
  private def parseLigature(off: Int): LigatureSubst =
    val c        = ByteCursor(data, off)
    c.u16 // substFormat (1)
    val covOff   = off + c.u16
    val setCount = c.u16
    val setOffs  = Array.fill(setCount)(off + c.u16)
    val covered  = Coverage.ordered(data, covOff)
    val sets: Array[Array[(Array[Int], Int)]] = setOffs.map { so =>
      val s        = ByteCursor(data, so)
      val ligCount = s.u16
      val ligOffs  = Array.fill(ligCount)(so + s.u16)
      ligOffs.map { lo =>
        val lc       = ByteCursor(data, lo)
        val ligGlyph = lc.u16
        val compCount = lc.u16
        val tail     = Array.fill(compCount - 1)(lc.u16)
        (tail, ligGlyph)
      }
    }
    LigatureSubst(covered.iterator.zipWithIndex.collect { case (g, i) if i < sets.length => g -> sets(i) }.toMap)

  // ContextSubstFormat3: a coverage per input glyph, then the nested-lookup records. Formats 1 and 2 (rule
  // and class based) are not read — modern Arabic fonts use format 3 — so they parse to nothing.
  private def parseContext(off: Int): Option[SubstSubtable] =
    val c = ByteCursor(data, off)
    if c.u16 != 3 then None
    else
      val glyphCount = c.u16
      val substCount = c.u16
      val covOffs    = Array.fill(glyphCount)(off + c.u16)
      val records    = Array.fill(substCount) { val seq = c.u16; (seq, c.u16) }
      Some(ContextSubst(covOffs.map(coverageSet), records))

  // ChainContextSubstFormat3: backtrack, input and lookahead coverages, then the nested-lookup records.
  // Backtrack coverages are stored in reverse text order. Formats 1 and 2 are not read.
  private def parseChainContext(off: Int): Option[SubstSubtable] =
    val c = ByteCursor(data, off)
    if c.u16 != 3 then None
    else
      val backtrackOffs = Array.fill(c.u16)(off + c.u16)
      val inputOffs     = Array.fill(c.u16)(off + c.u16)
      val lookaheadOffs = Array.fill(c.u16)(off + c.u16)
      val records       = Array.fill(c.u16) { val seq = c.u16; (seq, c.u16) }
      Some(
        ChainContextSubst(
          backtrackOffs.map(coverageSet),
          inputOffs.map(coverageSet),
          lookaheadOffs.map(coverageSet),
          records,
        ),
      )

  // The set of glyphs a coverage table at `off` covers (the membership is all a contextual match needs).
  private def coverageSet(off: Int): Set[Int] = Coverage.parse(data, off).keySet
