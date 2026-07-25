package io.github.edadma.texish

import scala.collection.mutable.ArrayBuffer

/** Inline math mode: collects the symbols between `$`s as a math list, and on exit lays it out — with the
  * binary-operator normalization and inter-atom spacing of [[MathList]] — into a single horizontal box that
  * drops back into the surrounding text. Symbols are placed through the glyph seam in the math font, while
  * the surrounding text keeps using the string seam, so the two paths never interfere.
  *
  * A math list is set in a [[MathStyle]]: the outermost `$…$` opens in text style, and each super/subscript
  * is set by a nested mode one style smaller (see [[scriptStyle]] and [[addScript]]). The mode's own symbols
  * are set in [[mathFont]], the base math font scaled to its style; the unscaled [[baseMathFont]] is handed
  * to the nested script modes so each scales from the base, not compounded. Fractions, radicals and stretchy
  * delimiters arrive in later stages.
  */
class MathMode(
    val t: Typesetter,
    val baseMathFont: MathFont,
    val style: MathStyle = MathStyle.Text,
    val grouped: Boolean = false,
) extends Mode:
  private val nodes = ArrayBuffer[MathNode]()

  /** Set by an infix `\over` or `\atop`: the kind of fraction to build from this list at exit (`true` = a
    * barred fraction, `false` = the bar-less `\atop`), and the nodes gathered before the operator, which
    * become the numerator. After the operator fires, the mode keeps collecting the denominator into `nodes`. */
  private var fractionBar:    Option[Boolean]  = None
  private var numeratorNodes: Vector[MathNode] = Vector.empty

  /** The equation number set by `\eqno` on a display, laid out in this mode's font; placed flush right on the
    * display line when the math is shipped. `None` for an unnumbered display or any inline formula. */
  var eqno: Option[Box] = None

  /** The base math font scaled to a given style's size level — the font a sub-list set in that style uses. */
  private def fontForStyle(s: MathStyle): MathFont =
    baseMathFont.atScale(s.scale(baseMathFont.scriptPercentScaleDown, baseMathFont.scriptScriptPercentScaleDown))

  /** The font this list's symbols are set in: the base math font scaled to this style's size level. */
  val mathFont: MathFont = fontForStyle(style)

  def init(): Unit = ()

  /** Append a pre-classified node — the path tests use, and the one the language layer's char/command
    * helpers funnel into. */
  def addNode(node: MathNode): Unit = nodes += node

  /** A box built outside the symbol tables (an `\hbox` embedded in math, say) enters as an ordinary atom. */
  infix def add(box: Box): Unit = nodes += MathAtom(MathClass.Ord, box)

  /** Add one input character, classified by [[MathSymbols]]; whitespace is ignored, as in TeX math. */
  def addChar(codepoint: Int): Unit =
    if !codepoint.toChar.isWhitespace then MathSymbols.charNode(mathFont, codepoint).foreach(nodes += _)

  /** Add a control sequence's symbol (`\alpha`, `\leq`, `\,`, …). Returns false when the name is not a
    * known math symbol, so the caller can report it at the right source position. */
  def addCommand(name: String): Boolean =
    MathSymbols.commandNode(mathFont, name) match
      case Some(node) => nodes += node; true
      case None       => false

  /** The style a super- or subscript of this list's atoms is set in. */
  def scriptStyle(superscript: Boolean): MathStyle = if superscript then style.sup else style.sub

  /** Begin a fraction at an infix `\over` (barred) or `\atop` (bar-less): everything collected so far in this
    * list becomes the numerator, and what follows becomes the denominator. As in TeX a group may hold only one
    * such operator, so a second is an error — `{a \over b \over c}` is ambiguous and must be parenthesised. */
  def setFraction(bar: Boolean): Unit =
    if fractionBar.isDefined then
      throw TexishException("ambiguous fraction: more than one \\over or \\atop in the same group")
    fractionBar = Some(bar)
    numeratorNodes = nodes.toVector
    nodes.clear()

  /** Set (or clear) limit placement on the most recent atom, for `\limits` / `\nolimits`. The control must
    * follow a large operator, as in TeX; otherwise it is an error. Scripts attached afterward are carried
    * over the flag, so `\sum\limits_a^b` sets its bounds above and below. */
  def setLimits(on: Boolean): Unit =
    nodes.lastOption match
      case Some(a: MathAtom) if a.cls == MathClass.Op =>
        nodes(nodes.length - 1) = a.copy(limits = Some(on))
      case _ => throw TexishException("limit controls must follow a math operator")

  /** Attach an already-laid-out script box to the most recent atom. When no atom precedes — a leading `^`
    * or `_`, or one right after an explicit space — an empty-nucleus Ord atom is created to carry it, as in
    * TeX. A second script of the same kind on one atom is a double-script error. */
  def addScript(superscript: Boolean, scriptBox: Box): Unit =
    val atom = nodes.lastOption match
      case Some(a: MathAtom) => nodes.remove(nodes.length - 1); a
      case _                 => MathAtom(MathClass.Ord, HBox(Vector.empty))

    val updated =
      if superscript then
        if atom.sup.isDefined then throw TexishException("double superscript")
        atom.copy(sup = Some(scriptBox))
      else
        if atom.sub.isDefined then throw TexishException("double subscript")
        atom.copy(sub = Some(scriptBox))

    nodes += updated

  /** Build a fraction box from an already-laid-out numerator and denominator, using this list's font and
    * style (its size sets the bar thickness, axis and shifts; display style opens the gaps). The numerator
    * and denominator are expected to have been set in [[style]]`.num`/`.denom`. */
  def makeFraction(numerator: Box, denominator: Box): Box =
    new FractionBox(t, numerator, denominator, mathFont.fractionParams(style.isDisplay))

  /** Build a fraction-like stack at an explicitly chosen display-ness, rather than inheriting it from this
    * list's style: `\dfrac`/`\tfrac` force the display (or text) gaps and shifts on, regardless of where the
    * fraction sits, and `\binom` stacks the operands with no rule (the parentheses are added by the caller).
    * The numerator and denominator are expected to have been set at the matching `num`/`denom` styles for the
    * forced style. The bar thickness, axis and shifts come from the font at the *forced* style's size — the
    * operands are full-size even when the fraction sits in a script, so the constants must match them, just
    * as TeX's `{\displaystyle\frac…}` takes everything from the forced style. */
  def makeFractionAt(numerator: Box, denominator: Box, display: Boolean, bar: Boolean): Box =
    val font   = fontForStyle(if display then MathStyle.Display else MathStyle.Text)
    val params = font.fractionParams(display)
    new FractionBox(t, numerator, denominator, if bar then params else params.copy(ruleThickness = 0.0))

  /** Build a radical (`\sqrt`) over an already-laid-out radicand, using this list's font and style. The
    * radicand is expected to have been set in [[style]]`.cramp`. The surd glyph is chosen tall enough to
    * span the radicand plus the bar and its clearance. An optional `degree` (the index of a higher root,
    * `\sqrt[3]{…}`) is expected to have been set in [[style]]`.rootDegree`. */
  def makeRadical(radicand: Box, degree: Option[Box] = None): Box =
    val p      = mathFont.radicalParams(style.isDisplay)
    val target = p.verticalGap + p.ruleThickness + radicand.ascent + radicand.descent

    new RadicalBox(t, mathFont.radicalGlyph(target), radicand, p, degree)

  /** Build an accented atom (`\hat x`, `\vec v`, `\widehat{AB}`, …): the accent glyph set over an
    * already-laid-out nucleus. The accent centres on the nucleus's top-accent attachment point (its visual
    * middle) and is raised so it rests just above the nucleus — at a uniform height over short bases, lifted
    * over tall ones. A `wide` accent grows to a horizontal variant spanning the nucleus. */
  def makeAccent(accentCodepoint: Int, nucleus: Box, wide: Boolean): Box =
    val accent =
      if wide then mathFont.horizontalVariant(accentCodepoint, nucleus.width)
      else mathFont.glyphBox(accentCodepoint)

    // The base's excess over the accent's design height — how far the accent must rise above where the font
    // designed it to sit. Never negative: over a base *shorter* than the accent base height the accent stays
    // at its design height rather than sinking into the nucleus (OpenType's AccentBaseHeight only ever raises).
    val shift = math.max(0.0, nucleus.ascent - mathFont.accentBaseHeight)

    val nucleusAttach = nucleus match
      case g: GlyphBox => mathFont.topAccentAttachmentGlyph(g.glyph).getOrElse(nucleus.width / 2)
      case _           => nucleus.width / 2
    val accentAttach =
      if wide then accent.width / 2
      else mathFont.topAccentAttachment(accentCodepoint).getOrElse(accent.width / 2)

    new AccentBox(nucleus, accent, shift, nucleusAttach - accentAttach)

  /** Build an `\overline` (`over` = true) or `\underline` (`over` = false): a rule across the full width of an
    * already-laid-out sub-formula, set off from it by a gap. The rule is the font's default (fraction) rule
    * thickness and the gap is three times that, as in plain TeX's `\overline`/`\underline`. The radicand of an
    * overline is expected to have been set cramped (nothing rises above the bar). */
  def makeBar(inner: Box, over: Boolean): Box =
    val thickness = mathFont.fractionParams(style.isDisplay).ruleThickness
    new MathBarBox(t, inner, gap = 3 * thickness, thickness, over)

  /** The style a matrix's cells are set in: a matrix sets its entries in text (or smaller) style, never the
    * enlarged display style, so a matrix inside a display does not blow its entries up. Crampedness carries
    * through — a matrix under a radical keeps its cells cramped. */
  def cellStyle: MathStyle = if style.isDisplay then MathStyle(MathSize.Text, style.cramped) else style

  /** Build a math array (`\matrix`, and the bracketed `\pmatrix`/`\bmatrix`/`\cases`): a grid of already-
    * laid-out cells, columns aligned and rows baseline-spaced, centred on the math axis so a delimiter set
    * around it spans it symmetrically. Columns are centred for a matrix, flush left for `\cases`. */
  def makeMatrix(rows: Vector[Vector[Box]], leftAlign: Boolean): Box =
    makeArray(rows, if leftAlign then MathArrayAlign.Left else MathArrayAlign.Center)

  /** Build a math array under a named alignment pattern — the engine behind both the brace forms (`\matrix`,
    * `\cases`) and the `\begin{…}` array environments (`aligned`, `pmatrix`, `smallmatrix`, …). A centred or
    * flush-left pattern gives every column the same alignment and a uniform inter-column gap; the aligned
    * pattern alternates right/left columns, butting each right→left pair together at the relation seam and
    * opening a wide gap between pairs. `tight` (for `\smallmatrix` and `\substack`) shrinks both the row and
    * column gaps for a compact stack. The cells are expected to have been set at this list's cell style. */
  def makeArray(rows: Vector[Vector[Box]], align: MathArrayAlign, tight: Boolean = false): Box =
    val em     = mathFont.size
    val nCols  = if rows.isEmpty then 0 else rows.map(_.size).max
    val colGap = if tight then 0.3 * em else 0.9 * em
    val rSep   = if tight then 0.2 * em else 0.45 * em
    val seams  = math.max(0, nCols - 1)

    val (aligns, seps) = align match
      case MathArrayAlign.Center =>
        (Vector.fill(nCols)(ColumnAlign.Center), Vector.fill(seams)(colGap))
      case MathArrayAlign.Left =>
        (Vector.fill(nCols)(ColumnAlign.Left), Vector.fill(seams)(colGap))
      case MathArrayAlign.Aligned =>
        // columns run right, left, right, left, …; within a right→left pair the seam carries just the relation's
        // own left space (the left column ends in a term and the right column opens with `=`, so the alignment
        // column reads `a = b` with proper spacing), and a wide quad opens between successive pairs
        val relSpace = 5.0 / 18.0 * em // TeX's thickmuskip, the space a relation keeps beside an operand
        val a = Vector.tabulate(nCols)(j => if j % 2 == 0 then ColumnAlign.Right else ColumnAlign.Left)
        val s = Vector.tabulate(seams)(k => if k % 2 == 0 then relSpace else 2.0 * em)
        (a, s)

    new MatrixBox(rows, mathFont.axisHeight, rSep, aligns, seps)

  /** Build a `\left…\right` delimited sub-formula: the already-laid-out inner formula flanked by stretchy
    * fences tall enough to span it about the math axis. Either delimiter may be absent (a null delimiter,
    * `\left.` or `\right.`). The fences are sized to cover the inner box's reach above and below the axis,
    * and centred on it, exactly as TeX sizes `\left`/`\right`. */
  def makeDelimited(left: Option[Int], inner: Box, right: Option[Int]): Box =
    val axis  = mathFont.axisHeight
    val delta = math.max(inner.ascent - axis, inner.descent + axis) // the formula's larger reach from the axis
    val target = 2 * delta                                          // a fence centred on the axis spans ±delta

    val pieces = Vector.newBuilder[Box]
    left.foreach(cp => pieces += mathFont.delimiter(cp, target))
    pieces += inner
    right.foreach(cp => pieces += mathFont.delimiter(cp, target))

    HBox(pieces.result())

  /** Re-set a list's single-glyph atoms in a different font — what a `\over`/`\atop` operand needs when it
    * moves from the list's own style down to fraction style (a size step smaller, except in display). Only an
    * atom whose nucleus is one glyph (and which carries no script) can be cheaply rebuilt from its codepoint;
    * an atom holding an already-built sub-box (a script, a nested fraction or radical, an operator name) keeps
    * its box, set at the list's style. In practice `\over` operands are simple symbol runs, so this restyles
    * them exactly; the rare nested construct in an inline `\over` is left at the surrounding size. */
  private def restyle(ns: Vector[MathNode], mf: MathFont): Vector[MathNode] =
    ns.map {
      case a: MathAtom if a.sup.isEmpty && a.sub.isEmpty =>
        a.nucleusCp match
          case Some(cp) => a.copy(nucleus = mf.glyphBox(cp), italicCorrection = mf.italicCorrection(cp))
          case None     => a
      case other => other
    }

  /** Lay the list out. Ordinarily this is the horizontal box of its atoms with the inter-atom spacing applied.
    * When an infix `\over`/`\atop` split the list, it is instead a fraction: the numerator (the atoms before
    * the operator) over the denominator (those after), each re-set one style smaller — script style inline,
    * text style in display — exactly as TeX styles `\over`. A barred `\over` draws the rule; `\atop` omits it. */
  def result: Box | Null =
    fractionBar match
      case Some(bar) =>
        val numStyle = style.num
        val denStyle = style.denom
        val numFont  = fontForStyle(numStyle)
        val denFont  = fontForStyle(denStyle)
        val num = HBox(
          MathList.translate(restyle(numeratorNodes, numFont), numFont, numStyle.cramped, numStyle.isDisplay, numStyle.isScript),
        )
        val den = HBox(
          MathList.translate(restyle(nodes.toVector, denFont), denFont, denStyle.cramped, denStyle.isDisplay, denStyle.isScript),
        )
        val params = mathFont.fractionParams(style.isDisplay)

        new FractionBox(t, num, den, if bar then params else params.copy(ruleThickness = 0.0))
      case None =>
        HBox(MathList.translate(nodes.toVector, mathFont, style.cramped, style.isDisplay, style.isScript))
