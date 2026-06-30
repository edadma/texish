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
  * Context and chaining-context substitution match a run of glyphs by coverage and apply nested lookups at
  * positions within the match. `Context` matches the input run alone; `ChainContext` also requires the
  * `backtrack` glyphs (in reverse, immediately before the input) and the `lookahead` glyphs (immediately
  * after). Each `record` is a (position-within-input, lookup-index) pair: it runs that lookup on the glyph
  * at that position, leaving the rest of the matched run in place. */
private sealed trait SubstSubtable
private final case class SingleSubst(map: Map[Int, Int])          extends SubstSubtable
private final case class MultipleSubst(map: Map[Int, Array[Int]]) extends SubstSubtable
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

  // Features applied after form selection, in order. Required ligatures — the lam-alef pair, formed here by
  // contextual substitution of the connected lam and alef for their `.rlig` variants — run on the shaped
  // glyphs, so their lookups match the contextual forms init/medi/fina already produced.
  private val PostFeatures = Seq("rlig")

  /** Build a shaper from a font's raw `GSUB` bytes, or None when the font has no Arabic form features (so
    * the caller keeps the plain text path). */
  def from(gsub: Option[Array[Byte]]): Option[Gsub] =
    gsub.flatMap { data =>
      val g = new Gsub(data)
      if g.hasFormSubstitution then Some(g) else None
    }

/** Parses the form-feature lookups of `data` (a `GSUB` table) on construction. */
final class Gsub(data: Array[Byte]):

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

  // Feature tag → the lookup-list indices that feature triggers, for the Arabic default language system.
  private val featureLookups: Map[String, Array[Int]] = parseFeatureMap()

  /** Whether the font carries at least one of the Arabic form features worth running. */
  def hasFormSubstitution: Boolean = Gsub.FormFeatures.exists(featureLookups.contains)

  /** Shape a run of nominal glyphs (one per character, the font's cmap result) into the glyphs to draw,
    * given each character's resolved joining form. Glyph-composition substitutions run first — `ccmp` (and
    * `locl`), which may split a letter into a dotless skeleton plus a separate dot mark — and the run grows
    * accordingly, every output glyph inheriting the form of the character it came from. Then each glyph is
    * swapped for its contextual form. The dot marks fall through both passes unchanged (no feature covers
    * them) and are positioned afterwards by the GPOS mark shaper. */
  def shape(glyphs: Array[Int], forms: Array[JoiningForm]): Array[Int] =
    var buf = glyphs.zip(forms)
    for tag <- Gsub.PreFeatures do
      featureLookups.get(tag).foreach(idxs => buf = applyExpanding(buf, idxs))
    var out = buf.map((g, f) => substituteForm(g, f))
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

  // Apply a sequence of lookups to a buffer of (glyph, form) pairs, letting a glyph expand into several
  // (each keeping the source's form), for the composition features that precede form selection.
  private def applyExpanding(buf: Array[(Int, JoiningForm)], idxs: Array[Int]): Array[(Int, JoiningForm)] =
    var cur = buf
    for li <- idxs do
      val next = scala.collection.mutable.ArrayBuffer.empty[(Int, JoiningForm)]
      for (g, f) <- cur do
        applyOne(li, g) match
          case Some(outs) => outs.foreach(o => next += (o -> f))
          case None       => next += (g -> f)
      cur = next.toArray
    cur

  // The substitution one lookup makes to a single glyph: the replacement glyph sequence, or None if no
  // subtable of the lookup covers the glyph. Used by the composition pass, which never carries the context a
  // contextual subtable needs — those are skipped here and handled by the buffer pass below.
  private def applyOne(li: Int, g: Int): Option[Array[Int]] =
    var res: Option[Array[Int]] = None
    val sts = lookups(li)
    var i   = 0
    while res.isEmpty && i < sts.length do
      sts(i) match
        case SingleSubst(m)   => m.get(g).foreach(x => res = Some(Array(x)))
        case MultipleSubst(m) => m.get(g).foreach(x => res = Some(x))
        case _                => // contextual subtables need the surrounding run, not a single glyph
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
        case Some((rep, consumed)) => out ++= rep; i += consumed
        case None                  => out += glyphs(i); i += 1
    out.toArray

  // Try a lookup at one buffer position. Single and multiple substitution act on the glyph itself; context
  // and chaining-context substitution match a run (with backtrack/lookahead for the chaining kind) and
  // return the run with their nested lookups applied. The result is the replacement glyphs and the number
  // of input glyphs consumed, or None if no subtable matches here.
  private def applyLookupAt(glyphs: Array[Int], i: Int, li: Int): Option[(Array[Int], Int)] =
    val sts = lookups(li)
    var res: Option[(Array[Int], Int)] = None
    var s   = 0
    while res.isEmpty && s < sts.length do
      sts(s) match
        case SingleSubst(m)   => m.get(glyphs(i)).foreach(x => res = Some((Array(x), 1)))
        case MultipleSubst(m) => m.get(glyphs(i)).foreach(x => res = Some((x, 1)))
        case ContextSubst(input, records) =>
          if matchRun(glyphs, i, input) then res = Some((applyRecords(glyphs, i, input.length, records), input.length))
        case ChainContextSubst(bt, input, la, records) =>
          if matchBacktrack(glyphs, i, bt) && matchRun(glyphs, i, input) &&
            matchRun(glyphs, i + input.length, la)
          then res = Some((applyRecords(glyphs, i, input.length, records), input.length))
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
  // record run that lookup on the glyph at that position. The records here substitute one glyph for one
  // (the lam and the alef each take their `.rlig` form), so the run keeps its length.
  private def applyRecords(glyphs: Array[Int], i: Int, inputLen: Int, records: Array[(Int, Int)]): Array[Int] =
    val run = Array.tabulate(inputLen)(k => glyphs(i + k))
    for (seqIdx, lookupIdx) <- records if seqIdx >= 0 && seqIdx < inputLen do
      run(seqIdx) = substOne(run(seqIdx), lookupIdx)
    run

  // The single-substitution result a nested lookup gives for one glyph, or the glyph unchanged. Nested
  // records in the Arabic required-ligature lookups select contextual `.rlig` variants, all single subs.
  private def substOne(g: Int, li: Int): Int =
    val sts  = lookups(li)
    var out  = g
    var s    = 0
    var done = false
    while !done && s < sts.length do
      sts(s) match
        case SingleSubst(m) => m.get(g).foreach { x => out = x; done = true }
        case _              =>
      s += 1
    out

  // ─── parsing ────────────────────────────────────────────────────────────────

  // A four-character OpenType tag (e.g. "init") from a big-endian u32.
  private def tag4(t: Long): String =
    String(Array(((t >> 24) & 0xff).toChar, ((t >> 16) & 0xff).toChar, ((t >> 8) & 0xff).toChar, (t & 0xff).toChar))

  // Map every feature tag used by the Arabic script's default language system to the lookups it triggers.
  // The featureList holds (tag, lookups) for every feature; a language system selects a subset of them by
  // index. The Arabic script ('arab') is preferred, then the default script, then the first one present.
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
  // feature if it names one.
  private def langSysFeatureIndices(scriptListOff: Int): Array[Int] =
    if scriptListOff == 0 then return Array.empty
    val c           = ByteCursor(data, scriptListOff)
    val scriptCount = c.u16
    val records     = Array.fill(scriptCount) { val tag = tag4(c.u32); val off = c.u16; (tag, off) }

    def scriptOffset(tag: String): Option[Int] = records.find(_._1 == tag).map(_._2)
    val chosen = scriptOffset("arab")
      .orElse(scriptOffset("DFLT"))
      .orElse(scriptOffset("dflt"))
      .orElse(records.headOption.map(_._2))

    chosen match
      case None => Array.empty
      case Some(scriptOff) =>
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
  // (type 1), multiple (type 2), context (type 5) and chaining-context (type 6) substitution are
  // materialised — together they cover Arabic form selection, ccmp composition and the rlig ligature;
  // other types, and the older formats 1/2 of the contextual kinds, are skipped.
  private def parseSubtable(lookupType: Int, off: Int): Option[SubstSubtable] =
    lookupType match
      case 1 => Some(parseSingle(off))
      case 2 => Some(parseMultiple(off))
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
