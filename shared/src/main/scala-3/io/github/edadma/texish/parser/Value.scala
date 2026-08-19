package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader

/** The core value type for the parser language.
  *
  * All values in parser are represented by this sealed enum, providing exhaustive pattern matching and type safety.
  */
enum Value:
  /** Text content */
  case Text(s: String)

  /** Numeric value */
  case Num(n: Double)

  /** Boolean value */
  case Bool(b: Boolean)

  /** Ordered sequence of values */
  case Seq(items: Vector[Value])

  /** Key-value mapping */
  case Map(entries: scala.collection.immutable.Map[String, Value])

  /** A macro definition - parameters and unexpanded body tokens */
  case Macro(params: Vector[MacroParam], body: Vector[Token], pos: CharReader)

  /** A dimension with unit (internally stored as points) */
  case Dimen(points: Double)

  /** A 2-D point `(x, y)` in the engine's point space — the value form of a picture coordinate. Produced by
    * `\point` and stored by `\coordinate`, read back by `(name)` references and by `\xof` / `\yof`. */
  case Point(x: Double, y: Double)

  /** Glue (flexible space) for typesetting. Stretch and shrink each have their own infinity order (0 = finite, 1 =
    * fil, 2 = fill), so `12pt plus 2pt minus 1fil` — finite stretch, infinite shrink — is representable.
    */
  case Glue(natural: Double, stretch: Double, shrink: Double, stretchOrder: Int = 0, shrinkOrder: Int = 0)

  /** The nil/empty value */
  case Nil

  /** Undefined - variable not found */
  case Undefined

  /** An opaque host object (font, color, engine glue, ...) stored in the variable scope */
  case Native(value: Any)

object Value:
  /** Wrap a raw host value as a Value. Already-wrapped values pass through. */
  def from(value: Any): Value = value match
    case v: Value      => v
    case n: BigDecimal => Num(n.toDouble)
    case n: Number     => Num(n.doubleValue)
    case s: String     => Text(s)
    case b: Boolean    => Bool(b)
    case null          => Nil
    case other         => Native(other)
  /** Whether a value counts as true where a condition is wanted — `\if`, `\while` and `\filter` alike.
    *
    * **There is one rule and this is it.** `\if` used to carry its own copy and `\while` and `\filter` used this
    * one, and the two disagreed about a number: `\if {0}` was false while `\while {0}` looped for ever, so a
    * condition written as arithmetic — which is how a package says "in range" or "not yet placed" without four
    * nested comparisons — meant opposite things in the two loops.
    *
    * Nothing, zero, and the text `0`, `false` or blank are false; a number that is not zero, a non-empty sequence,
    * and any other value are true. The text cases are what make a condition read the same whether it arrived as a
    * value or as the characters of one.
    */
  def truthy(v: Value): Boolean = v match
    case Bool(b)    => b
    case Num(n)     => n != 0
    case Text(s)    => val t = s.trim; t.nonEmpty && t != "0" && t.toLowerCase != "false"
    case Nil        => false
    case Undefined  => false
    case Seq(items) => items.nonEmpty
    case _          => true

  /** Check if a value is "falsy" */
  def falsy(v: Value): Boolean = !truthy(v)

  /** Interpret a value as a number where arithmetic expects one. A `Num` is itself, a `Dimen` is its size in
    * points, and a `Text` coerces when it is wholly a numeric literal (e.g. `"5"`, `"-2.5"`) or a dimension with an
    * absolute unit (e.g. `"1in"`, `"3mm"`, which give their size in points) — so a variable holding a numeric or
    * dimension string is usable in `\calc`, `\round`, and picture coordinates rather than being rejected as an
    * unknown name, the same way the literal would be. (A font-relative `em`/`ex` string is not resolved here, where
    * no font is in scope; those evaluate where the font unit is available.) Anything else is not a number. */
  def number(v: Value): Option[Double] = v match
    case Num(n)   => Some(n)
    case Dimen(p) => Some(p)
    case Text(s)  => s.trim.toDoubleOption.orElse(parseDimension(s.trim).collect { case Dimen(p) => p })
    // A glue reads as its natural size, the way TeX coerces \baselineskip in a dimen context — engine
    // parameters like \baselineskip are glue-valued, and \calc{2 * baselineskip} must work against them.
    case Glue(n, _, _, _, _)              => Some(n)
    case Native(g: io.github.edadma.texish.Glue) => Some(g.naturalSize)
    case _        => None

  /** Convert a value to its display string */
  def display(v: Value): String = v match
    case Text(s)       => s
    case Num(n)        => if n.isWhole then n.toLong.toString else n.toString
    case Bool(b)       => b.toString
    case Seq(items)    => items.map(display).mkString("[", ", ", "]")
    case Map(entries)  => entries.map((k, v) => s"$k: ${display(v)}").mkString("{", ", ", "}")
    case Macro(_, _, _) => "<macro>"
    case Dimen(pts)    => (if pts.isWhole then pts.toLong.toString else pts.toString) + "pt"
    case Point(x, y)   =>
      def n(d: Double) = if d.isWhole then d.toLong.toString else d.toString
      s"(${n(x)}, ${n(y)})"
    case Glue(n, st, sh, sto, sho) =>
      // renders back into the glue syntax the parser accepts, so \the output is re-readable
      def num(d: Double)  = if d.isWhole then d.toLong.toString else d.toString
      def unit(ord: Int)  = if ord == 0 then "pt" else "fi" + "l" * ord
      val plus            = if st != 0 then s" plus ${num(st)}${unit(sto)}" else ""
      val minus           = if sh != 0 then s" minus ${num(sh)}${unit(sho)}" else ""
      s"${num(n)}pt$plus$minus"
    case Nil           => ""
    case Undefined     => "<undefined>"
    case Native(v)     => v.toString
