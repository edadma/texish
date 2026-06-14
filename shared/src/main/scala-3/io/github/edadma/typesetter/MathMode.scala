package io.github.edadma.typesetter

import scala.collection.mutable.ArrayBuffer

/** Inline math mode: collects the symbols between `$`s as a math list, and on exit lays it out — with the
  * binary-operator normalization and inter-atom spacing of [[MathList]] — into a single horizontal box
  * that drops back into the surrounding text. Symbols are placed through the glyph seam in the math font,
  * while the surrounding text keeps using the string seam, so the two paths never interfere. Scripts,
  * fractions, radicals and stretchy delimiters arrive in later stages; Stage 2 is symbols and spacing only.
  */
class MathMode(val t: Typesetter, val mathFont: MathFont) extends Mode:
  private val nodes = ArrayBuffer[MathNode]()

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

  def result: Box | Null = HBox(MathList.translate(nodes.toVector, mathFont.size))
