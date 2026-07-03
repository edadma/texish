package io.github.edadma.texish.parser

/** Match a unit-suffixed dimension like 12pt, 0.5in, 3mm, 2pc, 1.5cm, 1.5em, 2ex */
private val DimensionPattern = """([+-]?(?:\d+\.?\d*|\.\d+))(pt|pc|in|cm|mm|em|ex)""".r

/** Match a flex (stretch or shrink) amount: a dimension or an infinite amount like 1fil / 2fill */
private val FlexPattern = """([+-]?(?:\d+\.?\d*|\.\d+))(pt|pc|in|cm|mm|em|ex|fil|fill)""".r

/** Match a full glue spec: a dimension with optional `plus <flex>` and `minus <flex>` parts */
// A unit is optional on each numeric component: an omitted unit means points, matching texish's point-space
// model where a bare number is already a length (so `\set leftskip {0 plus 1fil}` works, not only `0pt plus 1fil`).
private val GluePattern =
  """([+-]?(?:\d+\.?\d*|\.\d+))(pt|pc|in|cm|mm|em|ex)?(?:\s+plus\s+([+-]?(?:\d+\.?\d*|\.\d+))(pt|pc|in|cm|mm|em|ex|fil|fill)?)?(?:\s+minus\s+([+-]?(?:\d+\.?\d*|\.\d+))(pt|pc|in|cm|mm|em|ex|fil|fill)?)?""".r

/** Points per unit. Context-free units have fixed factors; em and ex come from the host's current font via the
  * resolver, and fail to resolve when the host has none (or has no font yet).
  */
private def unitPoints(unit: String, fontUnit: String => Option[Double]): Option[Double] = unit match
  case "pt"        => Some(1.0)
  case "pc"        => Some(12.0)
  case "in"        => Some(72.0)
  case "cm"        => Some(72 / 2.54)
  case "mm"        => Some(72 / 25.4)
  case "em" | "ex" => fontUnit(unit)

/** Parse a unit-suffixed dimension into Dimen (big points). Font-relative units (em, ex) resolve against the current
  * font through the resolver; without one they don't parse, leaving the input as text.
  */
def parseDimension(s: String, fontUnit: String => Option[Double] = _ => None): Option[Value] =
  s match
    case DimensionPattern(num, unit) => unitPoints(unit, fontUnit).map(f => Value.Dimen(num.toDouble * f))
    case _                           => None

/** Parse a flex amount into (size, infinity order): `2pt` is finite (order 0), `1fil` order 1, `1fill` order 2. The
  * size of an infinite amount is in fil units, not points.
  */
def parseFlex(s: String, fontUnit: String => Option[Double] = _ => None): Option[(Double, Int)] =
  s match
    case FlexPattern(num, "fil")  => Some((num.toDouble, 1))
    case FlexPattern(num, "fill") => Some((num.toDouble, 2))
    case FlexPattern(num, unit)   => unitPoints(unit, fontUnit).map(f => (num.toDouble * f, 0))
    case _                        => None

/** Parse a glue spec like `12pt plus 2pt minus 1fil` (the plus/minus parts optional, in that order). A bare
  * dimension yields Dimen; anything with a flex part yields Glue. Stretch and shrink carry independent infinity
  * orders, so finite stretch can coexist with infinite shrink.
  */
def parseGlue(s: String, fontUnit: String => Option[Double] = _ => None): Option[Value] =
  s.trim match
    case GluePattern(num, unit, stNum, stUnit, shNum, shUnit) =>
      def flex(n: String, u: String): Option[(Double, Int)] =
        if n == null then Some((0.0, 0))
        else
          u match
            case "fil"   => Some((n.toDouble, 1))
            case "fill"  => Some((n.toDouble, 2))
            case null    => Some((n.toDouble, 0)) // unitless stretch/shrink is in points
            case _       => unitPoints(u, fontUnit).map(f => (n.toDouble * f, 0))

      for
        factor                 <- if unit == null then Some(1.0) else unitPoints(unit, fontUnit)
        (stretch, stretchOrder) <- flex(stNum, stUnit)
        (shrink, shrinkOrder)   <- flex(shNum, shUnit)
      yield
        val natural = num.toDouble * factor
        if stNum == null && shNum == null then Value.Dimen(natural)
        else Value.Glue(natural, stretch, shrink, stretchOrder, shrinkOrder)
    case _ => None

/** Split a string into its code points, each returned as a string — one element per typed character, keeping a
  * surrogate pair (an astral symbol, an emoji) together where a char-by-char split would break it into two
  * lone surrogates. Manual scan because `String.codePoints()` is a java.util.stream API that Scala.js lacks. */
def codePointStrings(s: String): Vector[String] =
  val out = Vector.newBuilder[String]
  var i   = 0
  while i < s.length do
    val n =
      if Character.isHighSurrogate(s.charAt(i)) && i + 1 < s.length && Character.isLowSurrogate(s.charAt(i + 1))
      then 2
      else 1
    out += s.substring(i, i + n)
    i += n
  out.result()

// Helper to evaluate tokens to a value
def evalTokens(tokens: Vector[Token], handler: Handler): Value =
  stripOuterBraces(tokens) match
    case Vector(Token.Text(s, _)) =>
      // Try to parse as a number, then as a unit-suffixed dimension
      try Value.Num(s.toDouble)
      catch case _: Exception => parseDimension(s, handler.fontUnit).getOrElse(Value.Text(s))
    case Vector(Token.ControlSeq(name, _)) =>
      handler.get(name)
    case _ =>
      // Multiple tokens - concatenate as text including spaces. An active character that stands for
      // itself outside its special mode (notably #, which is only a placeholder inside \halign)
      // contributes its character, so a value like the colour {#808080} keeps its leading # instead of
      // collapsing to a bare number that then reads as a dimension. A control sequence contributes the
      // display of the variable it names, so `\set greeting {Hello \name}` interpolates rather than
      // silently dropping the reference; an unset name (or a macro, which cannot expand here) adds nothing.
      val text = tokens.map {
        case Token.Text(s, _)   => s
        case Token.Space(s, _)  => s
        case Token.Newline(_)   => "\n"
        case Token.Active(c, _) => c.toString
        case Token.ControlSeq(name, _) =>
          handler.get(name) match
            case Value.Undefined | Value.Nil | Value.Macro(_, _, _) => ""
            case v                                                  => Value.display(v)
        case _ => ""
      }.mkString
      if text.isEmpty then Value.Nil
      else parseGlue(text, handler.fontUnit).getOrElse(Value.Text(text)) // a braced glue spec like {12pt plus 2pt} arrives here
