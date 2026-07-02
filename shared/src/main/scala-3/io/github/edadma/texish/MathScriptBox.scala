package io.github.edadma.texish

/** The font metrics that govern super/subscript placement, in points. They come from the OpenType MATH
  * constants when the math font carries them; an ordinary text font standing in as a math font has none, so
  * TeX's Computer Modern values (scaled to the font size) fill in. The shift constants are read from the
  * font of the *nucleus* the scripts attach to, so they are already at the nucleus's size.
  */
case class MathScriptParams(
    superscriptShiftUp:                Double,
    superscriptShiftUpCramped:         Double,
    superscriptBottomMin:              Double,
    superscriptBaselineDropMax:        Double,
    subscriptShiftDown:                Double,
    subscriptTopMax:                   Double,
    subscriptBaselineDropMin:          Double,
    subSuperscriptGapMin:              Double,
    superscriptBottomMaxWithSubscript: Double,
    spaceAfterScript:                  Double,
)

object MathScriptParams:
  /** TeX's Computer Modern script parameters as fractions of an em, scaled to a font size. Used only when
    * the math font has no MATH table; a real math font supplies these from its own constants. */
  def texDefaults(size: Double): MathScriptParams = MathScriptParams(
    superscriptShiftUp                = 0.362 * size,
    superscriptShiftUpCramped         = 0.289 * size,
    superscriptBottomMin              = 0.125 * size,
    superscriptBaselineDropMax        = 0.386 * size,
    subscriptShiftDown                = 0.150 * size,
    subscriptTopMax                   = 0.345 * size,
    subscriptBaselineDropMin          = 0.050 * size,
    subSuperscriptGapMin              = 0.160 * size,
    superscriptBottomMaxWithSubscript = 0.345 * size,
    spaceAfterScript                  = 0.070 * size,
  )

/** A nucleus with a superscript and/or subscript attached. The superscript is raised by `supShift` and the
  * subscript lowered by `subShift` (both positive distances from the baseline), and each is offset
  * horizontally from the nucleus's right edge by `supDx` / `subDx` — how the nucleus's italic correction is
  * applied (a superscript set out past a slanted letter, or a large operator's limits skewed to follow its
  * slant). A little space follows the scripts. The box reports the true vertical extent of the assembly, so
  * the line it sits on leaves room for the raised and lowered scripts.
  */
class MathScriptBox(
    nucleus: Box,
    sup: Option[Box],
    sub: Option[Box],
    supShift: Double,
    subShift: Double,
    supDx: Double,
    subDx: Double,
    scriptSpace: Double,
) extends ContentBox:

  // The widest of the two scripts (each measured from its own horizontal offset) plus the nucleus and a
  // trailing script space sets the whole box's width; a script tucked left of the nucleus edge never pulls
  // the width in below the nucleus.
  private val scriptsWidth: Double =
    math.max(0.0, math.max(sub.map(_.width + subDx).getOrElse(0.0), sup.map(_.width + supDx).getOrElse(0.0)))

  val width: Double    = nucleus.width + scriptsWidth + (if sup.isEmpty && sub.isEmpty then 0.0 else scriptSpace)
  val xAdvance: Double = width

  val ascent: Double =
    Seq(Some(nucleus.ascent), sup.map(supShift + _.ascent), sub.map(_.ascent - subShift)).flatten.max

  val descent: Double =
    Seq(Some(nucleus.descent), sub.map(subShift + _.descent), sup.map(_.descent - supShift)).flatten.max

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    nucleus.draw(t, x, y)
    sup.foreach(_.draw(t, x + nucleus.width + supDx, y - supShift))
    sub.foreach(_.draw(t, x + nucleus.width + subDx, y + subShift))

  override def toString: String =
    s"MathScriptBox(nucleus=$nucleus, sup=$sup, sub=$sub, supShift=$supShift, subShift=$subShift)"

object MathScriptBox:
  /** The superscript raise and subscript drop for a nucleus, following TeXbook Appendix G rule 18 adapted to
    * the OpenType MATH constants. Each result is a positive distance from the baseline. A superscript must
    * clear the baseline (`superscriptBottomMin`) and start high enough (`superscriptShiftUp`, or its cramped
    * variant); a subscript must drop far enough (`subscriptShiftDown`) and keep its top within
    * `subscriptTopMax` of the baseline. The baseline-drop constants tie both to a tall *boxed* nucleus — a
    * built-up sub-formula whose scripts must track its height. A single-glyph nucleus takes no baseline drop
    * (rule 18a's char case): every letter's scripts sit at the same standard raise, so `x²` and `d²` line up
    * rather than each superscript riding its own letter's ascender. When both scripts are present the
    * subscript is pushed down until the gap between them reaches `subSuperscriptGapMin`, and then, if the
    * superscript's bottom sits below `superscriptBottomMaxWithSubscript`, the whole pair is lifted so the
    * superscript clears that height — exactly rule 18f.
    */
  def shifts(
      nucleus: Box,
      sup: Option[Box],
      sub: Option[Box],
      p: MathScriptParams,
      cramped: Boolean,
  ): (Double, Double) =
    val boxedNucleus = !nucleus.isInstanceOf[GlyphBox]

    val supRaw =
      sup.map { s =>
        val base = if cramped then p.superscriptShiftUpCramped else p.superscriptShiftUp
        val drop = if boxedNucleus then nucleus.ascent - p.superscriptBaselineDropMax else 0.0
        math.max(math.max(base, drop), p.superscriptBottomMin + s.descent)
      }.getOrElse(0.0)

    val subRaw =
      sub.map { s =>
        val drop = if boxedNucleus then nucleus.descent + p.subscriptBaselineDropMin else 0.0
        math.max(math.max(p.subscriptShiftDown, drop), s.ascent - p.subscriptTopMax)
      }.getOrElse(0.0)

    (sup, sub) match
      case (Some(su), Some(sb)) =>
        var u = supRaw
        var v = subRaw

        val gap = (u - su.descent) - (sb.ascent - v)
        if gap < p.subSuperscriptGapMin then v += p.subSuperscriptGapMin - gap

        val delta = p.superscriptBottomMaxWithSubscript - (u - su.descent)
        if delta > 0 then { u += delta; v -= delta }

        (u, v)
      case _ => (supRaw, subRaw)
