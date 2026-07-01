package io.github.edadma.texish.opentype

import scala.collection.mutable.ArrayBuffer

/** Shapes Devanagari text by turning a run of codepoints into the glyphs to draw. Devanagari is written left
  * to right, so unlike Hebrew and Arabic it needs no bidirectional reordering; what it needs is cluster-scoped
  * work no other script texish sets requires. Within each orthographic syllable (see [[Devanagari]]) the
  * shaper follows the model OpenType Indic fonts are built for:
  *
  *   1. map the cluster's characters to their nominal glyphs;
  *   2. run the basic-form features, which fuse consonant + virama sequences into conjuncts and half-forms;
  *   3. reorder the pre-base short-i sign to the front of the cluster (it is typed after its consonant but
  *      drawn before it);
  *   4. run the presentation features, which pick the width-matched contextual variants and stack the
  *      above- and below-base vowel signs.
  *
  * The features run in this order because a font's short-i variants (`pres`) are chosen from the base that
  * now follows the reordered sign, so the reordering must come first. Positioning of the marks that remain —
  * the below-base u signs, the anusvara — is left to the GPOS shaper ([[Gpos]]), which runs over the whole
  * assembled run afterwards, exactly as it does for Hebrew and Arabic marks.
  *
  * The GSUB lookup machinery is shared with the Arabic shaper ([[Gsub]]); this only supplies the Indic
  * feature order and the reordering, so it runs identically on every platform, including the in-browser
  * build with no system font engine.
  */
object IndicShaper:

  // The basic-form features, applied in the order the OpenType Indic model prescribes: nukta composition,
  // the akhand ligatures (ka-ssa, ja-nya), the reph and below-base forms of ra, the half-form, the vattu
  // variant, and the general conjunct. Half-forms and conjuncts are the ones Hindi leans on; the others are
  // applied when present and simply do nothing on a font or cluster that has no use for them.
  private val BasicFeatures = Seq("nukt", "akhn", "rphf", "rkrf", "blwf", "half", "vatu", "cjct")

  // The presentation features, applied after reordering: the pre-, above-, below- and post-base substitutions
  // that select the contextual glyph variants (including the width-matched short-i) and the halant and
  // contextual-alternate cleanups.
  private val PresFeatures = Seq("pres", "abvs", "blws", "psts", "haln", "calt")

  /** Build a Devanagari shaper from a font's raw `GSUB` bytes, or None when the font does not carry a
    * Devanagari script table — the caller then leaves the text on the plain path. */
  def from(gsub: Option[Array[Byte]]): Option[IndicShaper] =
    Gsub.fromIndic(gsub).map(new IndicShaper(_))

/** Shapes with the GSUB table `gsub` bound to the font's Devanagari script. */
final class IndicShaper(gsub: Gsub):

  /** Shape a run of Devanagari codepoints into the glyphs to draw, given `cmap`, the font's mapping from a
    * codepoint to its nominal glyph. Each orthographic cluster is shaped on its own — Devanagari shaping is
    * syllable-local — and the results concatenated in reading order; the caller positions any marks with the
    * GPOS shaper over the whole run. */
  def shape(cps: Array[Int], cmap: Int => Int): Array[Int] =
    val out = ArrayBuffer.empty[Int]
    for (s, e) <- Devanagari.clusters(cps) do out ++= shapeCluster(cps.slice(s, e), cmap)
    out.toArray

  // Shape one cluster: nominal glyphs, then the basic-form features, then the pre-base short-i reordering,
  // then the presentation features. Each feature acts on the small cluster buffer; the font's lookups are
  // context-gated, so one that has nothing to do here leaves the run untouched.
  private def shapeCluster(clusterCps: Array[Int], cmap: Int => Int): Array[Int] =
    var glyphs = clusterCps.map(cmap)
    for tag <- IndicShaper.BasicFeatures do glyphs = gsub.applyFeatureByTag(glyphs, tag)
    glyphs = Devanagari.reorderPreBaseMatra(clusterCps, glyphs, cmap(Devanagari.ShortISign))
    for tag <- IndicShaper.PresFeatures do glyphs = gsub.applyFeatureByTag(glyphs, tag)
    glyphs
