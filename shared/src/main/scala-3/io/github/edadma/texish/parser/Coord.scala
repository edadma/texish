package io.github.edadma.texish.parser

/** Parses a single parenthesised picture coordinate to a point in the engine's point space — the TikZ-style
  * coordinate sublanguage that layers over the bare space-separated scalars a picture already accepts. Three
  * forms, distinguished by the top-level separator inside the parentheses:
  *
  *   - `(x, y)` — Cartesian. Each component is a full [[MathExpr]] expression, so units and functions work:
  *     `(2in, 3in)`, `(R*cosd(60), R*sind(60))`.
  *   - `(angle : radius)` — polar, the headline form. The angle is in degrees, the radius an expression; the
  *     point is `(radius·cos angle, radius·sin angle)`. `(60:1in)` is one inch out at sixty degrees.
  *   - `(name)` — a named coordinate, looked up from a point stored earlier by `\coordinate`.
  *
  * A bare scalar (no parentheses) is not a coordinate and is left to the existing scalar reader; [[looksLikeCoord]]
  * makes that call. The relative markers `++(dx,dy)` / `+(dx,dy)` are stripped by the coordinate reader before it
  * calls here, which then parses the parenthesised remainder as one of the forms above and offsets it against the
  * current point.
  */
object Coord:

  /** Thrown on a malformed coordinate or an unknown named point; surfaced as a positioned error by the reader. */
  class CoordException(message: String) extends RuntimeException(message)

  /** Whether `s` is a parenthesised coordinate (versus a bare scalar the old reader should handle). */
  def looksLikeCoord(s: String): Boolean = s.trim.startsWith("(")

  /** Resolve a coordinate string to `(x, y)` in points. `resolveVar` supplies document variables used in the
    * component expressions, `fontUnit` the `em`/`ex` units, and `namedPoint` looks up a `(name)` reference. */
  def parse(
      raw: String,
      resolveVar: String => Option[Double],
      fontUnit: String => Option[Double],
      namedPoint: String => Option[(Double, Double)],
  ): (Double, Double) =
    val t = raw.trim
    if !t.startsWith("(") || !t.endsWith(")") then throw new CoordException(s"malformed coordinate '$raw'")
    val inner = t.substring(1, t.length - 1)

    def expr(s: String): Double = MathExpr.eval(s, resolveVar, fontUnit)

    topLevelSeparator(inner) match
      case Some((':', i)) =>
        val angle = expr(inner.substring(0, i))
        val r     = expr(inner.substring(i + 1))
        (r * math.cos(math.toRadians(angle)), r * math.sin(math.toRadians(angle)))
      case Some((_, i)) =>
        (expr(inner.substring(0, i)), expr(inner.substring(i + 1)))
      case None =>
        namedPoint(inner.trim).getOrElse(throw new CoordException(s"unknown coordinate '${inner.trim}' in '$raw'"))

  /** The first `,` or `:` at parenthesis depth zero inside a coordinate body, with its index — the separator
    * between the two components. Nested parentheses (a function call like `atan2(y,x)`) are skipped, so their
    * commas do not split the coordinate. `None` means a single token: a named coordinate. */
  private def topLevelSeparator(inner: String): Option[(Char, Int)] =
    var depth = 0
    var i     = 0
    while i < inner.length do
      inner(i) match
        case '('             => depth += 1
        case ')'             => depth -= 1
        case c @ (',' | ':') if depth == 0 => return Some((c, i))
        case _               =>
      i += 1
    None
