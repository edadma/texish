package io.github.edadma.texish

import io.github.edadma.texish.opentype.MathTable

/** The font math mode sets type in, paired with its parsed OpenType `MATH` table. A genuine math font
  * (Latin Modern Math, STIX Two, …) supplies the table; an ordinary text font has none, in which case
  * TeX's traditional defaults stand in for the few constants Stage 2 needs. Font-relative `MATH` constants
  * are returned already scaled to points by the font size, so callers work in points throughout.
  */
class MathFont(val t: Typesetter, val font: Font, val math: Option[MathTable]):
  private def renderFont: t.RenderFont = font.renderFont.asInstanceOf[t.RenderFont]

  /** The math font's size in points — one em. */
  def size: Double = font.size

  /** The glyph index for a Unicode codepoint in this font (0, the .notdef glyph, when the font lacks it). */
  def glyphIndex(codepoint: Int): Int = t.glyphIndex(renderFont, codepoint)

  /** How far script style scales the base size down, from the MATH table — TeX's 70% default otherwise. */
  def scriptPercentScaleDown: Double = math.map(_.constants.scriptPercentScaleDown).getOrElse(0.7)

  /** How far scriptscript style scales the base size down, from the MATH table — TeX's 50% default. */
  def scriptScriptPercentScaleDown: Double = math.map(_.constants.scriptScriptPercentScaleDown).getOrElse(0.5)

  /** The same math font at a fraction of this size — how a script sub-formula is set smaller. The MATH table
    * is size-independent (its constants are in em, scaled by the font size on use), so it carries over. */
  def atScale(factor: Double): MathFont =
    if factor == 1.0 then this
    else new MathFont(t, t.makeFont(font.typeface, font.size * factor, font.style), math)

  /** The italic correction for a codepoint in this font, in points: the small rightward kern a slanted glyph
    * wants after it before an upright neighbour or a superscript. Zero without a MATH table. */
  def italicCorrection(codepoint: Int): Double =
    math.map(_.glyphInfo.italicsCorrection.getOrElse(glyphIndex(codepoint), 0.0) * font.size).getOrElse(0.0)

  /** The script-placement parameters at this font's size: from the MATH constants when present, otherwise
    * TeX's Computer Modern defaults. */
  def scriptParams: MathScriptParams =
    math match
      case Some(m) =>
        val c = m.constants
        MathScriptParams(
          superscriptShiftUp                = c.value("superscriptShiftUp") * size,
          superscriptShiftUpCramped         = c.value("superscriptShiftUpCramped") * size,
          superscriptBottomMin              = c.value("superscriptBottomMin") * size,
          superscriptBaselineDropMax        = c.value("superscriptBaselineDropMax") * size,
          subscriptShiftDown                = c.value("subscriptShiftDown") * size,
          subscriptTopMax                   = c.value("subscriptTopMax") * size,
          subscriptBaselineDropMin          = c.value("subscriptBaselineDropMin") * size,
          subSuperscriptGapMin              = c.value("subSuperscriptGapMin") * size,
          superscriptBottomMaxWithSubscript = c.value("superscriptBottomMaxWithSubscript") * size,
          spaceAfterScript                  = c.value("spaceAfterScript") * size,
        )
      case None => MathScriptParams.texDefaults(size)

  /** The fraction-layout parameters at this font's size, for text or display style: from the MATH constants
    * (the display-style variants when `display`) when present, otherwise TeX's Computer Modern defaults. */
  def fractionParams(display: Boolean): FractionParams =
    math match
      case Some(m) =>
        val c = m.constants
        def v(name: String): Double = c.value(name) * size
        FractionParams(
          axisHeight     = c.axisHeight * size,
          ruleThickness  = v("fractionRuleThickness"),
          numGapMin      = v(if display then "fractionNumDisplayStyleGapMin" else "fractionNumeratorGapMin"),
          denomGapMin    = v(if display then "fractionDenomDisplayStyleGapMin" else "fractionDenominatorGapMin"),
          numShiftUp     = v(if display then "fractionNumeratorDisplayStyleShiftUp" else "fractionNumeratorShiftUp"),
          denomShiftDown =
            v(if display then "fractionDenominatorDisplayStyleShiftDown" else "fractionDenominatorShiftDown"),
        )
      case None => FractionParams.texDefaults(size, display)

  /** The radical-layout parameters at this font's size, for text or display style. */
  def radicalParams(display: Boolean): RadicalParams =
    math match
      case Some(m) =>
        val c = m.constants
        RadicalParams(
          ruleThickness    = c.value("radicalRuleThickness") * size,
          verticalGap      = c.value(if display then "radicalDisplayStyleVerticalGap" else "radicalVerticalGap") * size,
          extraAscender    = c.value("radicalExtraAscender") * size,
          // The MATH table specifies no horizontal overhang for the overbar, so the bar would end flush with the
          // radicand; give it the same small em-proportional overshoot the TeX defaults use.
          vinculumOvershoot = 0.05 * size,
          degreeRaise      = c.radicalDegreeBottomRaisePercent,
          kernBeforeDegree = c.value("radicalKernBeforeDegree") * size,
          kernAfterDegree  = c.value("radicalKernAfterDegree") * size,
        )
      case None => RadicalParams.texDefaults(size, display)

  /** The least an assembly's parts may overlap at a joint, in points. Zero without a MATH table. */
  def minConnectorOverlap: Double = math.map(_.variants.minConnectorOverlap).getOrElse(0.0) * size

  /** A box at least `targetHeight` points tall for a base glyph that grows vertically (a delimiter, a surd, a
    * big operator): the smallest precomposed size variant that reaches the target; or, past the largest
    * variant, the parts assembled to span it ([[GlyphAssemblyBox]]); or the largest variant, or the base
    * glyph, when the font supplies no taller form. Falls back to the base glyph with no MATH table. */
  def verticalVariant(codepoint: Int, targetHeight: Double): Box =
    val baseIdx = glyphIndex(codepoint)

    math.flatMap(_.variants.vertical.get(baseIdx)) match
      case Some(gc) =>
        gc.variants.find(_.advance * size >= targetHeight) match
          case Some(v) => new GlyphBox(t, v.glyph, font, t.currentColor)
          case None =>
            gc.assembly match
              case Some(asm) =>
                new GlyphAssemblyBox(t, font, t.currentColor, asm, targetHeight, minConnectorOverlap)
              case None =>
                new GlyphBox(t, gc.variants.lastOption.map(_.glyph).getOrElse(baseIdx), font, t.currentColor)
      case None => new GlyphBox(t, baseIdx, font, t.currentColor)

  /** A surd glyph (√) sized for a `targetHeight`-tall radicand. Unlike a delimiter — which must reach its
    * target — a surd looks best at the size *nearest* the target: the precomposed variants step coarsely, and
    * rounding up to the smallest one that reaches the target can land on a glyph far taller than needed, whose
    * point then dives well below the radicand. So this picks the variant closest to the target, preferring a
    * slightly short one (its point simply sits a little higher) over a grossly oversized one. Past the largest
    * variant it assembles the extensible surd to span the target; with no MATH table it is the base glyph. */
  def radicalGlyph(targetHeight: Double): Box =
    val baseIdx = glyphIndex(0x221A)

    math.flatMap(_.variants.vertical.get(baseIdx)) match
      case Some(gc) if gc.variants.nonEmpty =>
        val sized = gc.variants.map(v => (v.glyph, v.advance * size))
        val over  = sized.find(_._2 >= targetHeight)
        val under = sized.filter(_._2 < targetHeight).lastOption

        (over, under) match
          case (Some((g, h)), Some((ug, uh))) =>
            val glyph = if targetHeight - uh <= h - targetHeight then ug else g // the nearer of the two
            new GlyphBox(t, glyph, font, t.currentColor)
          case (Some((g, _)), None) => new GlyphBox(t, g, font, t.currentColor)
          case (None, _) =>
            gc.assembly match
              case Some(asm) => new GlyphAssemblyBox(t, font, t.currentColor, asm, targetHeight, minConnectorOverlap)
              case None      => new GlyphBox(t, sized.last._1, font, t.currentColor)
      case _ => new GlyphBox(t, baseIdx, font, t.currentColor)

  /** A delimiter at a chosen size rather than a chosen height: `size` 0 is the base glyph and each step up takes
    * the next precomposed variant, stopping at the largest the font supplies. This is what a fence sized by the
    * author needs (`\fence [size:2] {(}`), as against [[verticalVariant]], which is driven by the height of what
    * the fence must cover. Without a MATH table there are no variants, so every size gives the base glyph. */
  def variantAt(codepoint: Int, size: Int): Box =
    val baseIdx = glyphIndex(codepoint)

    if size <= 0 then new GlyphBox(t, baseIdx, font, t.currentColor)
    else
      math.flatMap(_.variants.vertical.get(baseIdx)) match
        case Some(gc) if gc.variants.nonEmpty =>
          // the variant list runs smallest-first from the base glyph, so step `size` places along it and clamp
          // (`math` names this font's MATH table in here, so the clamp is written out rather than min'd)
          val step = if size < gc.variants.length then size else gc.variants.length - 1

          new GlyphBox(t, gc.variants(step).glyph, font, t.currentColor)
        case _ => new GlyphBox(t, baseIdx, font, t.currentColor)

  /** A stretchy delimiter at least `targetHeight` points tall, centred on the math axis so it brackets a
    * formula symmetrically — the box `\left`/`\right` set the fence with. */
  def delimiter(codepoint: Int, targetHeight: Double): Box =
    new AxisCenteredBox(verticalVariant(codepoint, targetHeight), axisHeight)

  /** A box at least `targetWidth` points wide for a glyph that grows horizontally — a wide accent (`\widehat`)
    * over a several-character nucleus: the smallest precomposed width variant that reaches the target, else
    * the widest available, else the base glyph. (Horizontal assemblies, for accents wider than any variant,
    * are rare and not built.) */
  def horizontalVariant(codepoint: Int, targetWidth: Double): Box =
    val baseIdx = glyphIndex(codepoint)

    math.flatMap(_.variants.horizontal.get(baseIdx)) match
      case Some(gc) =>
        val chosen = gc.variants.find(_.advance * size >= targetWidth).orElse(gc.variants.lastOption).map(_.glyph)
        new GlyphBox(t, chosen.getOrElse(baseIdx), font, t.currentColor)
      case None => new GlyphBox(t, baseIdx, font, t.currentColor)

  /** The height an accent is designed to sit at over an x-height base, in points: the `MATH` accent base
    * height, or the font's x-height when it carries no MATH table. A nucleus taller than this lifts its
    * accent by the difference; a shorter one lowers it. */
  def accentBaseHeight: Double = math.map(_.constants.accentBaseHeight * size).getOrElse(font.xHeight)

  /** The horizontal point on a glyph an accent centres over, measured from the glyph's left edge in points —
    * the `MATH` top-accent attachment for the glyph index, or `None` when the font supplies none. */
  def topAccentAttachmentGlyph(glyph: Int): Option[Double] =
    math.flatMap(_.glyphInfo.topAccentAttachment.get(glyph)).map(_ * size)

  /** The top-accent attachment for a codepoint (resolved to its glyph), in points. */
  def topAccentAttachment(codepoint: Int): Option[Double] = topAccentAttachmentGlyph(glyphIndex(codepoint))

  /** A large operator glyph (`\sum`, `\int`). In display style it grows to a vertical variant at least the
    * font's `displayOperatorMinHeight` tall and is centred on the math axis; inline it is the base glyph,
    * sitting on the baseline like any other symbol. */
  def largeOperator(codepoint: Int, display: Boolean): Box =
    if !display then glyphBox(codepoint)
    else
      val minHeight = math.map(_.constants.displayOperatorMinHeight * size).getOrElse(0.0)
      new AxisCenteredBox(verticalVariant(codepoint, minHeight), axisHeight)

  /** The placement parameters for limits set above and below a large operator, at this font's size. */
  def limitParams: LimitsParams =
    math match
      case Some(m) =>
        val c = m.constants
        LimitsParams(
          upperGapMin          = c.value("upperLimitGapMin") * size,
          upperBaselineRiseMin = c.value("upperLimitBaselineRiseMin") * size,
          lowerGapMin          = c.value("lowerLimitGapMin") * size,
          lowerBaselineDropMin = c.value("lowerLimitBaselineDropMin") * size,
        )
      case None => LimitsParams.texDefaults(size)

  /** The math axis height in points: the line relations, binary operators, fraction bars and fences centre
    * on. Comes from the `MATH` table when present, otherwise a quarter em — TeX's `axis_height` for
    * Computer Modern at text size. */
  def axisHeight: Double = math.map(_.constants.axisHeight).getOrElse(0.25) * font.size

  /** A single-glyph box for a codepoint, set in this font in the current colour. */
  def glyphBox(codepoint: Int): GlyphBox = new GlyphBox(t, glyphIndex(codepoint), font, t.currentColor)
