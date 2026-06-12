package io.github.edadma.typesetter.texish

import io.github.edadma.char_reader.CharReader

/** The core value type for the texish language.
  *
  * All values in texish are represented by this sealed enum, providing exhaustive pattern matching and type safety.
  */
enum Value:
  /** Text content */
  case Text(s: String)

  /** Numeric value (arbitrary precision) */
  case Num(n: BigDecimal)

  /** Boolean value */
  case Bool(b: Boolean)

  /** Ordered sequence of values */
  case Seq(items: Vector[Value])

  /** Key-value mapping */
  case Map(entries: scala.collection.immutable.Map[String, Value])

  /** A macro definition - parameters and unexpanded body tokens */
  case Macro(params: Vector[String], body: Vector[Token], pos: CharReader)

  /** A dimension with unit (internally stored as points) */
  case Dimen(points: BigDecimal)

  /** Glue (flexible space) for typesetting */
  case Glue(natural: BigDecimal, stretch: BigDecimal, shrink: BigDecimal)

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
    case n: BigDecimal => Num(n)
    case n: Int        => Num(BigDecimal(n))
    case n: Long       => Num(BigDecimal(n))
    case n: Number     => Num(BigDecimal(n.doubleValue))
    case s: String     => Text(s)
    case b: Boolean    => Bool(b)
    case null          => Nil
    case other         => Native(other)
  /** Check if a value is "truthy" (for conditionals) */
  def truthy(v: Value): Boolean = v match
    case Bool(false) => false
    case Nil         => false
    case Undefined   => false
    case Text("")    => false
    case Seq(items) if items.isEmpty => false
    case _           => true

  /** Check if a value is "falsy" */
  def falsy(v: Value): Boolean = !truthy(v)

  /** Convert a value to its display string */
  def display(v: Value): String = v match
    case Text(s)       => s
    case Num(n)        => if n.isWhole then n.toBigInt.toString else n.toString
    case Bool(b)       => b.toString
    case Seq(items)    => items.map(display).mkString("[", ", ", "]")
    case Map(entries)  => entries.map((k, v) => s"$k: ${display(v)}").mkString("{", ", ", "}")
    case Macro(_, _, _) => "<macro>"
    case Dimen(pts)    => (if pts.isWhole then pts.toBigInt.toString else pts.toString) + "pt"
    case Glue(n, st, sh) => s"${n}pt plus ${st}pt minus ${sh}pt"
    case Nil           => ""
    case Undefined     => "<undefined>"
    case Native(v)     => v.toString
