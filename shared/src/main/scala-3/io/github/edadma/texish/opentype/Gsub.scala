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
  * draw the ligature. Extension lookups (type 7) are followed to the real subtable.
  *
  * All three formats of the contextual kinds are read. Arabic fonts generally use the coverage-based format
  * 3, but Indic fonts write a great deal of their behaviour as the rule-based format 1 and the class-based
  * format 2 — Noto Serif Devanagari builds the conjunct of a word as ordinary as विद्या that way. Reading
  * only format 3, as this once did, skipped those lookups in silence and left such words unshaped.
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
// A position in a contextual match is a predicate over glyphs rather than a glyph set, so the three ways
// OpenType writes such a rule all reduce to one representation: format 3 gives a coverage table (a set is
// already a predicate), format 1 a specific glyph (equality), and format 2 a class (the class the glyph is
// assigned). The class form is why a set will not do — class 0 means "every glyph not named in the ClassDef",
// which no enumerable set can express without knowing the whole font.
private final case class ContextSubst(input: Array[Int => Boolean], records: Array[(Int, Int)])
    extends SubstSubtable
private final case class ChainContextSubst(
    backtrack: Array[Int => Boolean],
    input: Array[Int => Boolean],
    lookahead: Array[Int => Boolean],
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

  // How deep a contextual lookup may nest inside another before the font is taken to be nesting in a cycle.
  // Two levels is what the Indic faces need; the limit only has to keep a malformed table from recursing
  // without end.
  private val MaxNestDepth = 8

  /** Build an Arabic shaper from a font's raw `GSUB` (and `GDEF`) bytes, or None when the font has no Arabic
    * form features (so the caller keeps the plain text path). */
  def from(gsub: Option[Array[Byte]], gdef: Option[Array[Byte]] = None): Option[Gsub] =
    gsub.flatMap { data =>
      val g = new Gsub(data, Seq("arab"), Gdef.from(gdef))
      if g.hasFormSubstitution then Some(g) else None
    }

  /** Build a shaper bound to the Latin (or default) script for a font's small-capitals substitution, or None
    * when the font carries no `smcp` feature there. This is the signal that an ordinary text font can turn its
    * lowercase letters into small capitals — the engine applies the feature itself (see the small-caps path in
    * `io.github.edadma.texish.CharBox`) rather than relying on a separately drawn small-caps font. Unlike the
    * Arabic and Indic factories this gates on the case feature itself, since small caps is a feature of an
    * ordinary roman, not a script of its own. */
  def fromSmallCaps(gsub: Option[Array[Byte]], gdef: Option[Array[Byte]] = None): Option[Gsub] =
    gsub.flatMap { data =>
      val g = new Gsub(data, Seq("latn"), Gdef.from(gdef))
      if g.hasFeature("smcp") then Some(g) else None
    }

  /** Build an Indic shaper from a font's raw `GSUB` (and `GDEF`) bytes for the script whose OpenType tags are
    * `scriptTags` (Devanagari passes `dev2`, `deva`; Bengali passes `bng2`, `beng`), or None when the font
    * carries none of those script tables — a font that does not shape the script keeps the plain text path.
    * The same subtable parsing and lookup machinery the Arabic shaper uses is reused here; the Indic
    * reordering and feature order live in `io.github.edadma.texish.opentype.IndicShaper`, which drives this
    * by feature name. */
  def fromIndic(gsub: Option[Array[Byte]], gdef: Option[Array[Byte]], scriptTags: Seq[String]): Option[Gsub] =
    gsub.flatMap { data =>
      val g = new Gsub(data, scriptTags, Gdef.from(gdef))
      if g.boundToRequestedScript then Some(g) else None
    }

// One lookup: its subtables plus the flag that governs which glyphs its matching skips over. `markFilterSet`
// names the GDEF mark glyph set a USE_MARK_FILTERING_SET lookup filters by (meaningful only when that flag
// bit is on).
private final case class SubstLookup(flag: Int, markFilterSet: Int, subtables: Vector[SubstSubtable])

/** Parses the language-system feature lookups of `data` (a `GSUB` table) on construction, binding to the
  * first of `scriptTags` the font carries (Arabic passes `arab`; the Indic shaper passes `dev2`, `deva`).
  * `gdef` supplies the glyph classes lookup flags filter matching by — with [[Gdef.empty]] nothing is
  * filtered and matching is plain adjacency. */
final class Gsub(data: Array[Byte], scriptTags: Seq[String], gdef: Gdef = Gdef.empty):

  // Lookups parsed by their index in the LookupList; an unparsed/irrelevant lookup yields an empty vector.
  private val lookups: Array[SubstLookup] =
    if data.length < 10 then Array.empty
    else
      val c = ByteCursor(data, 0)
      c.u16; c.u16 // major, minor version
      c.u16        // scriptList offset (read below)
      c.u16        // featureList offset (read below)
      val lookupListOff = c.u16
      parseLookupList(lookupListOff)

  // ─── lookup-flag glyph filtering ─────────────────────────────────────────────
  //
  // A lookup's flag names glyph classes its matching is blind to: an IGNORE_MARKS ligature matches its
  // components across an intervening vowel point exactly as if the point were not there (the point survives,
  // attached to the result). This is how Noto Naskh forms lam-alef in vocalized text — the rlig contextual
  // pair matches across the fatha — and how the Indic presentation ligatures match across a nukta.

  /** Whether lookup `lk`'s matching skips over glyph `g`. */
  private def skips(lk: SubstLookup, g: Int): Boolean =
    val flag = lk.flag
    if (flag & 0xff1e) == 0 then false // no filtering bits set — the common case
    else
      gdef.glyphClass(g) match
        case 1 => (flag & 0x0002) != 0 // IGNORE_BASE_GLYPHS
        case 2 => (flag & 0x0004) != 0 // IGNORE_LIGATURES
        case 3 =>
          if (flag & 0x0008) != 0 then true // IGNORE_MARKS
          else if (flag & 0x0010) != 0 then // USE_MARK_FILTERING_SET: skip marks NOT in the named set
            !gdef.markGlyphSets.lift(lk.markFilterSet).exists(_.contains(g))
          else
            val attachType = (flag >> 8) & 0xff // MarkAttachmentType: keep only marks of this attach class
            attachType != 0 && gdef.markAttachClass(g) != attachType
        case _ => false

  // The first non-skipped position at or after `from` (glyphs.length when none).
  private def nextAt(glyphs: Array[Int], from: Int, lk: SubstLookup): Int =
    var p = from
    while p < glyphs.length && skips(lk, glyphs(p)) do p += 1
    p

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
        applyLookupAt(gs, i, li, og) match
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
    val sts = lookups(li).subtables
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
  // skip past the glyphs it consumed, otherwise copy the glyph through and advance by one. The output built
  // so far is what a chaining match's backtrack is checked against — the glyphs already produced, as the
  // spec has it — so an earlier match in the same pass is seen in its substituted form.
  private def applyLookupOverBuffer(glyphs: Array[Int], li: Int): Array[Int] =
    val out = scala.collection.mutable.ArrayBuffer.empty[Int]
    var i   = 0
    while i < glyphs.length do
      applyLookupAt(glyphs, i, li, out) match
        case Some((rep, consumed, _)) => out ++= rep; i += consumed
        case None                     => out += glyphs(i); i += 1
    out.toArray

  // Try a lookup at one buffer position. Single, multiple and ligature substitution act on the glyph (and,
  // for a ligature, the glyphs that follow); context and chaining-context substitution match a run (with
  // backtrack/lookahead for the chaining kind) and return the run with their nested lookups applied. All
  // matching skips the glyphs the lookup's flag ignores; a lookup never *begins* at a glyph it skips. The
  // result is the replacement glyphs, the number of input glyphs consumed (the whole span, including any
  // skipped glyphs matched across — they survive inside the replacement), and whether each replacement
  // glyph carries its own input position's form (true for a context run, false when one source glyph drives
  // the whole replacement); or None if no subtable matches here. `prior` is the output already emitted for
  // the glyphs before `i` — what a chaining match's backtrack is checked against.
  private def applyLookupAt(
      glyphs: Array[Int],
      i: Int,
      li: Int,
      prior: collection.IndexedSeq[Int],
  ): Option[(Array[Int], Int, Boolean)] =
    val lk = lookups(li)
    if skips(lk, glyphs(i)) then return None
    val sts = lk.subtables
    var res: Option[(Array[Int], Int, Boolean)] = None
    var s   = 0
    while res.isEmpty && s < sts.length do
      sts(s) match
        case SingleSubst(m)   => m.get(glyphs(i)).foreach(x => res = Some((Array(x), 1, false)))
        case MultipleSubst(m) => m.get(glyphs(i)).foreach(x => res = Some((x, 1, false)))
        case LigatureSubst(l) => tryLigature(glyphs, i, l, lk).foreach((rep, n) => res = Some((rep, n, false)))
        case ContextSubst(input, records) =>
          matchInput(glyphs, i, input, lk).foreach { pos =>
            res = Some((applyRecords(glyphs, i, pos, records, prior), pos.last - i + 1, true))
          }
        case ChainContextSubst(bt, input, la, records) =>
          matchInput(glyphs, i, input, lk).foreach { pos =>
            if matchBacktrack(prior, bt, lk) && matchLookahead(glyphs, pos.last, la, lk) then
              res = Some((applyRecords(glyphs, i, pos, records, prior), pos.last - i + 1, true))
          }
      s += 1
    res

  // The buffer positions the input coverages match starting at `i` — the first coverage must cover the
  // glyph at `i` itself, each later coverage the next non-skipped glyph — or None when the run does not
  // match.
  private def matchInput(
      glyphs: Array[Int],
      i: Int,
      covs: Array[Int => Boolean],
      lk: SubstLookup,
  ): Option[Array[Int]] =
    if covs.isEmpty || !covs(0)(glyphs(i)) then None
    else
      val pos = new Array[Int](covs.length)
      pos(0) = i
      var j  = 1
      var p  = i + 1
      var ok = true
      while ok && j < covs.length do
        p = nextAt(glyphs, p, lk)
        if p >= glyphs.length || !covs(j)(glyphs(p)) then ok = false
        else
          pos(j) = p
          p += 1
          j += 1
      if ok then Some(pos) else None

  // Whether the backtrack coverages match the non-skipped glyphs at the end of `prior` — the output already
  // produced before the match position, so a pair this same pass substituted is seen in its substituted
  // form. Backtrack is given in reverse text order: `bt(0)` matches the closest preceding glyph, `bt(1)` the
  // one before that, and so on.
  private def matchBacktrack(
      prior: collection.IndexedSeq[Int],
      bt: Array[Int => Boolean],
      lk: SubstLookup,
  ): Boolean =
    var p  = prior.length - 1
    var j  = 0
    var ok = true
    while ok && j < bt.length do
      while p >= 0 && skips(lk, prior(p)) do p -= 1
      if p < 0 || !bt(j)(prior(p)) then ok = false
      else
        p -= 1
        j += 1
    ok

  // Whether the lookahead coverages match the non-skipped glyphs after the input's last matched position.
  private def matchLookahead(
      glyphs: Array[Int],
      lastInput: Int,
      la: Array[Int => Boolean],
      lk: SubstLookup,
  ): Boolean =
    var p  = lastInput + 1
    var j  = 0
    var ok = true
    while ok && j < la.length do
      p = nextAt(glyphs, p, lk)
      if p >= glyphs.length || !la(j)(glyphs(p)) then ok = false
      else
        p += 1
        j += 1
    ok

  // Apply a context match's nested lookups over the matched span. The span runs from `i` to the last
  // matched input position; skipped glyphs inside it pass through unchanged. Each element is tagged with
  // its logical input index (-1 for a skipped pass-through), and each (position, lookup) record runs its
  // lookup on the element still carrying that logical index — a record may substitute one glyph for one
  // (the lam and the alef each take their `.rlig` form) or ligate glyphs (a shadda and a following
  // dagger-alef into one composed mark), so the span can shorten; later records read it as it then stands.
  // Returns the resulting glyphs; the caller advances by the original span length. `prior` is the output
  // already emitted before the span and `glyphs` holds what follows it — the context a nested contextual
  // lookup matches its own backtrack and lookahead against, which reach outside the span.
  private def applyRecords(
      glyphs: Array[Int],
      i: Int,
      positions: Array[Int],
      records: Array[(Int, Int)],
      prior: collection.IndexedSeq[Int],
  ): Array[Int] =
    val buf     = scala.collection.mutable.ArrayBuffer.empty[Int]
    val logical = scala.collection.mutable.ArrayBuffer.empty[Int]
    var k = i
    while k <= positions.last do
      buf += glyphs(k)
      logical += positions.indexOf(k)
      k += 1
    val after = glyphs.view.slice(positions.last + 1, glyphs.length).toIndexedSeq
    for (seqIdx, lookupIdx) <- records do
      val pos = logical.indexOf(seqIdx)
      if pos >= 0 then applyNestedAt(prior, buf, logical, pos, after, lookupIdx, 0)
    buf.toArray

  // The substitution a nested lookup makes at one element of a working span: a single (one glyph), multiple
  // (one to many) or ligature (many to one) replacement, spliced in place. A nested ligature matches its
  // components with the nested lookup's own flag, so it too can reach across skipped glyphs, which stay in
  // the span after the ligature. A nested lookup may itself be contextual — Noto Serif Telugu reaches its
  // subjoined ra through two levels of chaining context — so those are followed too, up to
  // [[Gsub.MaxNestDepth]], the depth beyond which a font is taken to be nesting in a cycle.
  private def applyNestedAt(
      before: collection.IndexedSeq[Int],
      buf: scala.collection.mutable.ArrayBuffer[Int],
      logical: scala.collection.mutable.ArrayBuffer[Int],
      pos: Int,
      after: collection.IndexedSeq[Int],
      li: Int,
      depth: Int,
  ): Unit =
    val lk = lookups(li)
    if skips(lk, buf(pos)) then return
    val sts  = lk.subtables
    var done = false
    var s    = 0
    while !done && s < sts.length do
      sts(s) match
        case SingleSubst(m) =>
          m.get(buf(pos)).foreach { x => buf(pos) = x; done = true }
        case MultipleSubst(m) =>
          m.get(buf(pos)).foreach { x =>
            val keep = logical(pos)
            buf.remove(pos); logical.remove(pos)
            buf.insertAll(pos, x)
            logical.insertAll(pos, Array.tabulate(x.length)(k => if k == 0 then keep else -1))
            done = true
          }
        case LigatureSubst(l) =>
          l.get(buf(pos)).foreach { set =>
            var k = 0
            while !done && k < set.length do
              val (tail, lig) = set(k)
              // match the tail against the following non-skipped elements, collecting their indices
              val matched = scala.collection.mutable.ArrayBuffer.empty[Int]
              var p  = pos + 1
              var j  = 0
              var ok = true
              while ok && j < tail.length do
                while p < buf.length && skips(lk, buf(p)) do p += 1
                if p >= buf.length || buf(p) != tail(j) then ok = false
                else
                  matched += p
                  p += 1
                  j += 1
              if ok then
                buf(pos) = lig
                for m <- matched.reverseIterator do { buf.remove(m); logical.remove(m) }
                done = true
              k += 1
          }
        case ContextSubst(input, records) =>
          if applyNestedContext(before, buf, logical, pos, after, lk, Array.empty, input, Array.empty, records, depth)
          then done = true
        case ChainContextSubst(bt, input, la, records) =>
          if applyNestedContext(before, buf, logical, pos, after, lk, bt, input, la, records, depth) then done = true
      s += 1

  // A nested contextual lookup, matched against the whole run rather than the working span alone: its
  // backtrack and lookahead routinely reach outside the span the outer match carved out, so the span is
  // rejoined to the output already emitted (`before`) and the glyphs still to come (`after`) and the match
  // made against that. Only records landing inside the span are applied — a nested lookup that would
  // substitute outside the outer match is not the caller's to make. Returns whether the subtable matched.
  private def applyNestedContext(
      before: collection.IndexedSeq[Int],
      buf: scala.collection.mutable.ArrayBuffer[Int],
      logical: scala.collection.mutable.ArrayBuffer[Int],
      pos: Int,
      after: collection.IndexedSeq[Int],
      lk: SubstLookup,
      bt: Array[Int => Boolean],
      input: Array[Int => Boolean],
      la: Array[Int => Boolean],
      records: Array[(Int, Int)],
      depth: Int,
  ): Boolean =
    if depth >= Gsub.MaxNestDepth then false
    else
      val seq = (before ++ buf ++ after).toArray
      val at  = before.length + pos
      matchInput(seq, at, input, lk) match
        case Some(ps) if matchBacktrack(seq.view.slice(0, at).toIndexedSeq, bt, lk) &&
            matchLookahead(seq, ps.last, la, lk) =>
          // the span's own indices, kept in step as a record's substitution lengthens or shortens the buffer
          val targets = Array.tabulate(ps.length)(k => ps(k) - before.length)
          for (seqIdx, lookupIdx) <- records do
            if seqIdx < targets.length then
              val q = targets(seqIdx)
              if q >= 0 && q < buf.length then
                val len = buf.length
                applyNestedAt(before, buf, logical, q, after, lookupIdx, depth + 1)
                val delta = buf.length - len
                if delta != 0 then
                  for k <- targets.indices do if targets(k) > q then targets(k) += delta
          true
        case _ => false

  // The ligature a lookup forms starting at `i`, if any: the first ligature whose remaining components
  // match the following non-skipped glyphs. Returns the replacement — the ligature glyph followed by the
  // skipped glyphs the match reached across (a vowel point inside the span survives, drawn attached to the
  // ligature) — and the whole span consumed, components and skipped glyphs alike.
  private def tryLigature(
      glyphs: Array[Int],
      i: Int,
      ligatures: Map[Int, Array[(Array[Int], Int)]],
      lk: SubstLookup,
  ): Option[(Array[Int], Int)] =
    ligatures.get(glyphs(i)) match
      case None => None
      case Some(set) =>
        var res: Option[(Array[Int], Int)] = None
        var k   = 0
        while res.isEmpty && k < set.length do
          val (tail, lig) = set(k)
          var p    = i + 1
          var j    = 0
          var last = i
          var ok   = true
          while ok && j < tail.length do
            p = nextAt(glyphs, p, lk)
            if p >= glyphs.length || glyphs(p) != tail(j) then ok = false
            else
              last = p
              p += 1
              j += 1
          if ok then
            val retained = (i + 1 to last).iterator.filter(x => skips(lk, glyphs(x))).map(glyphs).toArray
            res = Some((Array(lig) ++ retained, last - i + 1))
          k += 1
        res

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
  private def parseLookupList(lookupListOff: Int): Array[SubstLookup] =
    if lookupListOff == 0 then return Array.empty
    val lc        = ByteCursor(data, lookupListOff)
    val lookupCnt = lc.u16
    val offsets   = Array.fill(lookupCnt)(lookupListOff + lc.u16)
    offsets.map(parseLookup)

  private def parseLookup(off: Int): SubstLookup =
    val l          = ByteCursor(data, off)
    val lookupType = l.u16
    val flag       = l.u16
    val subCount   = l.u16
    val subs       = Array.fill(subCount)(off + l.u16)
    // when USE_MARK_FILTERING_SET is on, the set index follows the subtable offsets
    val markFilterSet = if (flag & 0x0010) != 0 then l.u16 else 0
    SubstLookup(flag, markFilterSet, subs.toVector.flatMap(so => parseSubtable(lookupType, so)))

  // Dispatch one subtable by lookup type, following an extension (type 7) to the wrapped subtable. Single
  // (type 1), multiple (type 2), ligature (type 4), context (type 5) and chaining-context (type 6)
  // substitution are materialised — together they cover Arabic form selection, ccmp composition, the rlig
  // lam-alef pair and the liga "Allah" ligature; other types, and the older formats 1/2 of the contextual
  // kinds, are skipped.
  // One subtable can yield several parsed rules: the rule- and class-based context formats each hold a set of
  // alternative sequences, and every one becomes a match of its own.
  private def parseSubtable(lookupType: Int, off: Int): Seq[SubstSubtable] =
    lookupType match
      case 1 => Seq(parseSingle(off))
      case 2 => Seq(parseMultiple(off))
      case 4 => Seq(parseLigature(off))
      case 5 => parseContext(off)
      case 6 => parseChainContext(off)
      case 7 =>
        val c       = ByteCursor(data, off)
        c.u16 // substFormat (1)
        val extType = c.u16
        val extOff  = off + c.u32.toInt
        parseSubtable(extType, extOff)
      case _ => Nil

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

  /** Context substitution, in all three of the ways OpenType writes it. Format 3 states a coverage per input
    * position and is one rule; formats 1 and 2 instead hold a *set* of alternative sequences, selected by the
    * first glyph — by its coverage index in format 1, by its class in format 2 — so each alternative is
    * returned as a rule of its own. Reading only format 3, as this once did, silently dropped every rule an
    * Indic font writes the other two ways: Noto Serif Telugu builds its above-base substitutions that way, so
    * a subjoined ra never took its final form.
    */
  private def parseContext(off: Int): Seq[SubstSubtable] =
    ByteCursor(data, off).u16 match
      case 1 =>
        val c        = ByteCursor(data, off)
        c.u16
        val byIndex  = Coverage.parse(data, off + c.u16).map((g, i) => i -> g)
        val setOffs  = Array.fill(c.u16)(c.u16)
        for
          i     <- setOffs.indices if setOffs(i) != 0
          first <- byIndex.get(i).toSeq
          ro    <- ruleOffsets(off + setOffs(i))
        yield
          val (seq, records) = sequenceRule(ro)
          ContextSubst(is(first) +: seq.map(is), records)
      case 2 =>
        val c       = ByteCursor(data, off)
        c.u16
        val covers  = coverageSet(off + c.u16)
        val classOf = classDef(off + c.u16)
        val setOffs = Array.fill(c.u16)(c.u16)
        for
          cls <- setOffs.indices if setOffs(cls) != 0
          ro  <- ruleOffsets(off + setOffs(cls))
        yield
          val (seq, records) = sequenceRule(ro)
          val head: Int => Boolean = g => covers(g) && classOf(g) == cls
          ContextSubst(head +: seq.map(inClass(classOf, _)), records)
      case 3 =>
        val c          = ByteCursor(data, off)
        c.u16
        val glyphCount = c.u16
        val substCount = c.u16
        val covOffs    = Array.fill(glyphCount)(off + c.u16)
        val records    = Array.fill(substCount) { val seq = c.u16; (seq, c.u16) }
        Seq(ContextSubst(covOffs.map(covers), records))
      case _ => Nil

  /** Chaining-context substitution, in all three formats. As with [[parseContext]], formats 1 and 2 hold
    * alternative sequences and each becomes its own rule; the backtrack sequence is stored in reverse text
    * order in every format, which is the order the matcher wants.
    */
  private def parseChainContext(off: Int): Seq[SubstSubtable] =
    ByteCursor(data, off).u16 match
      case 1 =>
        val c       = ByteCursor(data, off)
        c.u16
        val byIndex = Coverage.parse(data, off + c.u16).map((g, i) => i -> g)
        val setOffs = Array.fill(c.u16)(c.u16)
        for
          i     <- setOffs.indices if setOffs(i) != 0
          first <- byIndex.get(i).toSeq
          ro    <- ruleOffsets(off + setOffs(i))
        yield
          val (bt, seq, la, records) = chainRule(ro)
          ChainContextSubst(bt.map(is), is(first) +: seq.map(is), la.map(is), records)
      case 2 =>
        val c        = ByteCursor(data, off)
        c.u16
        val covers   = coverageSet(off + c.u16)
        val btClass  = classDef(off + c.u16)
        val inClassD = classDef(off + c.u16)
        val laClass  = classDef(off + c.u16)
        val setOffs  = Array.fill(c.u16)(c.u16)
        for
          cls <- setOffs.indices if setOffs(cls) != 0
          ro  <- ruleOffsets(off + setOffs(cls))
        yield
          val (bt, seq, la, records) = chainRule(ro)
          val head: Int => Boolean   = g => covers(g) && inClassD(g) == cls
          ChainContextSubst(
            bt.map(inClass(btClass, _)),
            head +: seq.map(inClass(inClassD, _)),
            la.map(inClass(laClass, _)),
            records,
          )
      case 3 =>
        val c             = ByteCursor(data, off)
        c.u16
        val backtrackOffs = Array.fill(c.u16)(off + c.u16)
        val inputOffs     = Array.fill(c.u16)(off + c.u16)
        val lookaheadOffs = Array.fill(c.u16)(off + c.u16)
        val records       = Array.fill(c.u16) { val seq = c.u16; (seq, c.u16) }
        Seq(
          ChainContextSubst(
            backtrackOffs.map(covers),
            inputOffs.map(covers),
            lookaheadOffs.map(covers),
            records,
          ),
        )
      case _ => Nil

  // A position matching one particular glyph (the rule-based formats), and one matching a class value (the
  // class-based formats). Class 0 is every glyph the ClassDef does not name, which is why these are
  // predicates rather than sets.
  private def is(glyph: Int): Int => Boolean                         = _ == glyph
  private def inClass(classOf: Int => Int, cls: Int): Int => Boolean = g => classOf(g) == cls

  // A position matching any glyph a coverage table covers (the format 3 case). A Set[Int] is already a
  // predicate; this only fixes the type where an array of them is wanted.
  private def covers(off: Int): Int => Boolean = coverageSet(off)

  // The rule offsets of a RuleSet (a count then that many offsets, relative to the set).
  private def ruleOffsets(setOff: Int): Array[Int] =
    val c = ByteCursor(data, setOff)
    Array.fill(c.u16)(setOff + c.u16)

  // A SequenceRule: the input glyphs or classes after the first (which the rule set itself selected), then
  // the nested-lookup records.
  private def sequenceRule(off: Int): (Array[Int], Array[(Int, Int)]) =
    val c       = ByteCursor(data, off)
    val count   = c.u16
    val lookups = c.u16
    val seq     = Array.fill(math.max(count - 1, 0))(c.u16)
    (seq, Array.fill(lookups) { val s = c.u16; (s, c.u16) })

  // A ChainedSequenceRule: backtrack (already in reverse text order), the input after the first, lookahead,
  // then the nested-lookup records.
  private def chainRule(off: Int): (Array[Int], Array[Int], Array[Int], Array[(Int, Int)]) =
    val c     = ByteCursor(data, off)
    val bt    = Array.fill(c.u16)(c.u16)
    val count = c.u16
    val seq   = Array.fill(math.max(count - 1, 0))(c.u16)
    val la    = Array.fill(c.u16)(c.u16)
    (bt, seq, la, Array.fill(c.u16) { val s = c.u16; (s, c.u16) })

  // A ClassDef table: the class a glyph is assigned, 0 for any glyph it does not name.
  private def classDef(off: Int): Int => Int =
    val c = ByteCursor(data, off)
    c.u16 match
      case 1 =>
        val start  = c.u16
        val values = Array.fill(c.u16)(c.u16)
        g => if g >= start && g < start + values.length then values(g - start) else 0
      case 2 =>
        val ranges = Array.fill(c.u16) { val s = c.u16; val e = c.u16; (s, e, c.u16) }
        g =>
          var lo  = 0
          var hi  = ranges.length - 1
          var out = 0
          while lo <= hi do
            val mid          = (lo + hi) >>> 1
            val (s, e, cls)  = ranges(mid)
            if g < s then hi = mid - 1
            else if g > e then lo = mid + 1
            else { out = cls; lo = hi + 1 }
          out
      case _ => _ => 0

  // The set of glyphs a coverage table at `off` covers (the membership is all a contextual match needs).
  private def coverageSet(off: Int): Set[Int] = Coverage.parse(data, off).keySet
