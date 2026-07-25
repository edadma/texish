package io.github.edadma.texish.opentype

import scala.collection.mutable.ArrayBuffer

/** Shapes Indic text by turning a run of codepoints into the glyphs to draw. An Indic script is written left
  * to right, so unlike Hebrew and Arabic it needs no bidirectional reordering; what it needs is cluster-scoped
  * work no other script texish sets requires. Within each orthographic syllable (see [[IndicScript]]) the
  * shaper follows the model OpenType Indic fonts are built for:
  *
  *   1. lift off the reph, if the cluster opens with one, and split any two-part vowel sign into its parts;
  *   2. map the cluster's characters to their nominal glyphs;
  *   3. run the basic-form features, which fuse consonant + virama sequences into conjuncts, half-forms and
  *      post-base forms;
  *   4. reorder the vowel signs — the pre-base sign to the front of the cluster (it is typed after its
  *      consonant but drawn before it), and, in a script that subjoins its joined consonants, the sign drawn
  *      on the base back across them to the base's side — and append the reph mark after the base;
  *   5. run the presentation features, which pick the width-matched contextual variants and stack the above-
  *      and below-base vowel signs.
  *
  * The features run in this order because a font's pre-base variants (`pres`) are chosen from the base that
  * now follows the reordered sign, so the reordering must come first. After the clusters are assembled the
  * word-position features — a script's `init` and `fina` forms — are applied to the first and last glyph of
  * the run, which for a word set as one box is exactly the word boundary the font expects. Positioning of the
  * marks that remain — the reph, the below-base u signs, the anusvara — is left to the GPOS shaper
  * ([[Gpos]]), which runs over the whole assembled run afterwards, exactly as it does for Hebrew and Arabic
  * marks.
  *
  * The GSUB lookup machinery is shared with the Arabic shaper ([[Gsub]]); this only supplies the Indic
  * feature order and the reordering, so it runs identically on every platform, including the in-browser build
  * with no system font engine. */
object IndicShaper:

  // The Indic scripts texish can shape, tried in turn against a font's GSUB to see which script table it
  // carries. A font is built for one script (Noto Serif Devanagari carries the Devanagari tables, Noto Serif
  // Bengali the Bengali ones), so at most one matches.
  private val scripts: Seq[IndicScript] = Seq(Devanagari, Bengali, Gurmukhi, Telugu)

  /** Build an Indic shaper from a font's raw `GSUB` (and `GDEF`) bytes by finding which Indic script the font
    * shapes, or None when the font carries no Indic script table — the caller then leaves the text on the
    * plain path. */
  def from(gsub: Option[Array[Byte]], gdef: Option[Array[Byte]] = None): Option[IndicShaper] =
    gsub.flatMap { data =>
      scripts.iterator
        .flatMap(sc => Gsub.fromIndic(Some(data), gdef, sc.scriptTags).map(g => new IndicShaper(sc, g)))
        .nextOption()
    }

/** Shapes with the GSUB table `gsub` bound to `script`'s OpenType script. */
final class IndicShaper(val script: IndicScript, gsub: Gsub):

  /** Whether this shaper's script is the one a run of text is written in — the test the caller uses to route
    * the text here rather than down the plain path. */
  def handles(text: String): Boolean = script.has(text)

  /** Shape a run of Indic codepoints into the glyphs to draw, given `cmap`, the font's mapping from a
    * codepoint to its nominal glyph. Each orthographic cluster is shaped on its own — Indic shaping is
    * syllable-local — and the results concatenated in reading order; the word-position `init`/`fina` forms are
    * then applied to the ends of the assembled run, and the caller positions any marks with the GPOS shaper. */
  def shape(cps: Array[Int], cmap: Int => Int): Array[Int] =
    val out = ArrayBuffer.empty[Int]
    for (s, e) <- script.clusters(cps) do out ++= shapeCluster(cps.slice(s, e), cmap)
    var glyphs = out.toArray
    for tag <- script.initFeature do glyphs = gsub.applyFeatureByTagAt(glyphs, tag, 0)
    for tag <- script.finaFeature do glyphs = gsub.applyFeatureByTagAt(glyphs, tag, glyphs.length - 1)
    glyphs

  // Shape one cluster: the reph, if the cluster opens with one, is lifted off and its ra+virama shaped into
  // the reph mark; any two-part vowel sign is split into its pre- and post-base parts; then the rest is taken
  // through the nominal glyphs, the basic-form features and the pre-base reordering; the reph mark is appended
  // after the base so it sits above the syllable; then the presentation features run over the whole cluster —
  // selecting variants and, where the font has one, ligating the reph with an adjacent post-base sign. Each
  // feature acts on the small cluster buffer; the font's lookups are context-gated, so one with nothing to do
  // here leaves the run untouched. GPOS positions the reph and any other marks afterwards, in the caller.
  private def shapeCluster(clusterCps: Array[Int], cmap: Int => Int): Array[Int] =
    val reph    = script.startsWithReph(clusterCps)
    val body    = if reph then clusterCps.drop(2) else clusterCps
    val coreCps = body.flatMap(cp => script.decompose(cp) match
      case Some((pre, post)) => Array(pre, post)
      case None              => Array(cp))
    var glyphs = coreCps.map(cmap)
    for tag <- script.basicFeatures do glyphs = gsub.applyFeatureByTag(glyphs, tag)
    val preBaseGlyph = coreCps.find(script.preBaseMatras.contains).map(cmap).getOrElse(-1)
    glyphs = script.reorderPreBaseMatra(coreCps, glyphs, preBaseGlyph)
    glyphs = script.reorderPreSubjoinedMatra(glyphs, coreCps.filter(script.preSubjoinedMatras.contains).map(cmap))
    if reph then
      val rg = rephGlyphs(cmap)
      // where the reph lands is a per-script convention: at the cluster's very end, or — Bengali — between
      // the base and a post-base vowel sign, so র্মা sets ma, reph, aa rather than trailing the reph
      val at =
        if script.rephBeforePostBase then
          val postGlyphs = coreCps.filter(script.postBaseMatras.contains).map(cmap).toSet
          if postGlyphs.isEmpty then -1 else glyphs.indexWhere(postGlyphs.contains)
        else -1
      glyphs = if at >= 0 then glyphs.patch(at, rg, 0) else glyphs ++ rg
    for tag <- script.presFeatures do glyphs = gsub.applyFeatureByTag(glyphs, tag)
    glyphs

  // The reph mark for a cluster-initial ra + virama: the `rphf` feature ligates the two into the single reph
  // glyph. A font without `rphf` leaves the two glyphs unchanged, which the caller appends as a graceful
  // fallback rather than dropping the ra.
  private def rephGlyphs(cmap: Int => Int): Array[Int] =
    gsub.applyFeatureByTag(Array(cmap(script.ra), cmap(script.halant)), "rphf")
