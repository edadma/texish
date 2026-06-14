package io.github.edadma.texish

/** The size level of a math style — TeX's four nested sizes. Display and text are the full math size;
  * script is what a first-level super/subscript is set in, and scriptscript the level below that. */
enum MathSize:
  case Display, Text, Script, ScriptScript

/** A TeX math style: a size level together with whether it is *cramped*. Style governs two things — the type
  * size symbols are set at, and the vertical placement of scripts. A cramped style sets superscripts a little
  * lower so they do not collide with material above; it is entered for the subscript of any atom, and (in
  * later stages) under a radical and in a fraction denominator.
  *
  * Inline math opens in [[MathStyle.Text]]. The transitions here size and place super/subscripts; the
  * numerator/denominator transitions fractions need arrive with fractions.
  */
case class MathStyle(size: MathSize, cramped: Boolean):
  import MathSize.*

  /** The style a superscript is set in: one size level smaller, keeping the current cramping. */
  def sup: MathStyle = MathStyle(scriptSize, cramped)

  /** The style a subscript is set in: one size level smaller and always cramped. */
  def sub: MathStyle = MathStyle(scriptSize, cramped = true)

  /** The cramped form of this style, at the same size. */
  def cramp: MathStyle = MathStyle(size, cramped = true)

  /** The style a fraction numerator is set in: one size level smaller, keeping the current cramping. */
  def num: MathStyle = MathStyle(fracSize, cramped)

  /** The style a fraction denominator is set in: one size level smaller and always cramped. */
  def denom: MathStyle = MathStyle(fracSize, cramped = true)

  /** The style a radical's degree (the small index of `\sqrt[3]{…}`) is set in: always scriptscript and
    * cramped, regardless of the current style — a cube root's 3 is tiny even in display. */
  def rootDegree: MathStyle = MathStyle(ScriptScript, cramped = true)

  /** Whether this is display style — display fractions/radicals use the wider gaps and shifts. */
  def isDisplay: Boolean = size == Display

  private def scriptSize: MathSize = size match
    case Display | Text        => Script
    case Script | ScriptScript => ScriptScript

  private def fracSize: MathSize = size match
    case Display               => Text
    case Text                  => Script
    case Script | ScriptScript => ScriptScript

  /** The size factor relative to the base (display/text) font, taken from the font's MATH percentages — or
    * TeX's defaults of 70% for script and 50% for scriptscript when the font carries no MATH table. The
    * factor is absolute from the base, never compounded: scriptscript is half the base size, not half of the
    * script size. */
  def scale(scriptPct: Double, scriptScriptPct: Double): Double = size match
    case Display | Text => 1.0
    case Script         => scriptPct
    case ScriptScript   => scriptScriptPct

object MathStyle:
  /** Inline math: text style, uncramped — the style a `$…$` opens in. */
  val Text: MathStyle = MathStyle(MathSize.Text, cramped = false)

  /** Display math: the style `$$…$$` opens in. */
  val Display: MathStyle = MathStyle(MathSize.Display, cramped = false)
