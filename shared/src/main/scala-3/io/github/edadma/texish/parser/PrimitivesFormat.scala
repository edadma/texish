package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader

/** Number-to-string formatters — the representations a document format needs for numbered things (sections,
  * lists, footnotes). Each is a pure `Int => String`, exhaustively unit-testable on its own, and exposed as a
  * primitive (`\arabic`/`\roman`/…) that both outputs its text and sets a result, so it composes inside `\set`
  * and the counters layer. These are the formatting half of roadmap #6; the counter package builds on them plus
  * the keyed store (`\mapset`/`\mapget`) and `\global`.
  */
object NumberFormat:

  /** Decimal — the identity formatter, here for symmetry with the others. */
  def arabic(n: Int): String = n.toString

  private val romanUnits = Vector(
    1000 -> "m", 900 -> "cm", 500 -> "d", 400 -> "cd",
    100 -> "c", 90 -> "xc", 50 -> "l", 40 -> "xl",
    10 -> "x", 9 -> "ix", 5 -> "v", 4 -> "iv", 1 -> "i",
  )

  /** Lowercase roman numerals (1 → `i`, 4 → `iv`, 1990 → `mcmxc`). Zero and negatives have no roman form, so
    * they fall back to the decimal string — matching how TeX's `\romannumeral` simply emits nothing useful below 1
    * but staying informative rather than silent.
    */
  def roman(n: Int): String =
    if n <= 0 then n.toString
    else
      val b = new StringBuilder
      var rem = n
      for (value, sym) <- romanUnits do
        while rem >= value do
          b ++= sym
          rem -= value
      b.toString

  /** Uppercase roman numerals (1 → `I`, 1990 → `MCMXC`). */
  def Roman(n: Int): String = roman(n).toUpperCase

  /** Bijective base-26 lowercase letters, 1-based (1 → `a`, 26 → `z`, 27 → `aa`, 52 → `az`, 53 → `ba`). Outside the
    * positive range there is no letter form, so the decimal string is returned. LaTeX's `\alph` only covers 1–26;
    * this extends naturally past `z` instead of erroring.
    */
  def alph(n: Int): String =
    if n <= 0 then n.toString
    else
      val b = new StringBuilder
      var rem = n
      while rem > 0 do
        val d = (rem - 1) % 26
        b += ('a' + d).toChar
        rem = (rem - 1) / 26
      b.reverse.toString

  /** Bijective base-26 uppercase letters, 1-based (1 → `A`, 27 → `AA`). */
  def Alph(n: Int): String = alph(n).toUpperCase

  private val footnoteSymbols = Vector("*", "†", "‡", "§", "¶", "‖")

  /** Footnote symbols in LaTeX's classic order: `* † ‡ § ¶ ‖`, then doubled `** †† ‡‡ …` for 7–12, tripled for
    * 13–18, and so on. Anything outside `1..` falls back to the decimal string.
    */
  def fnsymbol(n: Int): String =
    if n <= 0 then n.toString
    else
      val idx    = (n - 1) % footnoteSymbols.length
      val repeat = (n - 1) / footnoteSymbols.length + 1
      footnoteSymbols(idx) * repeat

/** Shared body for the formatting primitives: read one number-valued argument, coerce it to an `Int`, format it,
  * then both output the text and set it as a result so the call composes in expression position.
  */
abstract class NumberFormatPrimitive(format: Int => String) extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalArgumentExpr(pos)
    val n = arg match
      case Value.Num(x)  => x.toInt
      case Value.Text(s) => s.trim.toIntOption.getOrElse(proc.handler.error(s"Expected a number, got '$s'", pos))
      case other         => proc.handler.error(s"Expected a number, got ${Value.display(other)}", pos)
    val text = format(n)
    proc.setResult(Value.Text(text))
    proc.handler.text(text)

object ArabicPrimitive   extends NumberFormatPrimitive(NumberFormat.arabic)
object RomanPrimitive    extends NumberFormatPrimitive(NumberFormat.roman)
object RomanUpPrimitive  extends NumberFormatPrimitive(NumberFormat.Roman)
object AlphPrimitive     extends NumberFormatPrimitive(NumberFormat.alph)
object AlphUpPrimitive    extends NumberFormatPrimitive(NumberFormat.Alph)
object FnSymbolPrimitive extends NumberFormatPrimitive(NumberFormat.fnsymbol)
