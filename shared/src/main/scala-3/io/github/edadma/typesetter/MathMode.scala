package io.github.edadma.typesetter

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
class MathMode(val t: Typesetter, val baseMathFont: MathFont, val style: MathStyle = MathStyle.Text) extends Mode:
  private val nodes = ArrayBuffer[MathNode]()

  /** The font this list's symbols are set in: the base math font scaled to this style's size level. */
  val mathFont: MathFont =
    baseMathFont.atScale(style.scale(baseMathFont.scriptPercentScaleDown, baseMathFont.scriptScriptPercentScaleDown))

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

  /** Attach an already-laid-out script box to the most recent atom. When no atom precedes — a leading `^`
    * or `_`, or one right after an explicit space — an empty-nucleus Ord atom is created to carry it, as in
    * TeX. A second script of the same kind on one atom is a double-script error. */
  def addScript(superscript: Boolean, scriptBox: Box): Unit =
    val atom = nodes.lastOption match
      case Some(a: MathAtom) => nodes.remove(nodes.length - 1); a
      case _                 => MathAtom(MathClass.Ord, HBox(Vector.empty))

    val updated =
      if superscript then
        if atom.sup.isDefined then sys.error("double superscript")
        atom.copy(sup = Some(scriptBox))
      else
        if atom.sub.isDefined then sys.error("double subscript")
        atom.copy(sub = Some(scriptBox))

    nodes += updated

  /** Build a fraction box from an already-laid-out numerator and denominator, using this list's font and
    * style (its size sets the bar thickness, axis and shifts; display style opens the gaps). The numerator
    * and denominator are expected to have been set in [[style]]`.num`/`.denom`. */
  def makeFraction(numerator: Box, denominator: Box): Box =
    new FractionBox(t, numerator, denominator, mathFont.fractionParams(style.isDisplay))

  def result: Box | Null = HBox(MathList.translate(nodes.toVector, mathFont, style.cramped))
