package io.github.edadma.typesetter

import io.github.edadma.typesetter.opentype.MathTable

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
          ruleThickness = c.value("radicalRuleThickness") * size,
          verticalGap   = c.value(if display then "radicalDisplayStyleVerticalGap" else "radicalVerticalGap") * size,
          extraAscender = c.value("radicalExtraAscender") * size,
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

  /** A surd glyph (√) at least `targetHeight` points tall — the vertical variant, or assembly, of √. */
  def radicalGlyph(targetHeight: Double): Box = verticalVariant(0x221A, targetHeight)

  /** A stretchy delimiter at least `targetHeight` points tall, centred on the math axis so it brackets a
    * formula symmetrically — the box `\left`/`\right` set the fence with. */
  def delimiter(codepoint: Int, targetHeight: Double): Box =
    new AxisCenteredBox(verticalVariant(codepoint, targetHeight), axisHeight)

  /** The math axis height in points: the line relations, binary operators, fraction bars and fences centre
    * on. Comes from the `MATH` table when present, otherwise a quarter em — TeX's `axis_height` for
    * Computer Modern at text size. */
  def axisHeight: Double = math.map(_.constants.axisHeight).getOrElse(0.25) * font.size

  /** A single-glyph box for a codepoint, set in this font in the current colour. */
  def glyphBox(codepoint: Int): GlyphBox = new GlyphBox(t, glyphIndex(codepoint), font, t.currentColor)
