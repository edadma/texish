package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader

// ============ SEQUENCE FUNCTIONS ============

/** Split a `\seq`/`\map` literal's tokens into its whitespace-separated items and evaluate each. Only a space
  * (or newline) at brace depth zero separates: a braced item keeps its interior spaces, so `\seq{{a b} c}` is
  * two items and `\map{name {Ada Lovelace}}` can hold a space-containing value. */
private def splitItems(groupTokens: Vector[Token], handler: Handler): Vector[Value] =
  val items   = Vector.newBuilder[Value]
  var current = Vector.newBuilder[Token]
  var depth   = 0

  def flush(): Unit =
    val tokens = current.result()
    if tokens.nonEmpty then items += evalTokens(tokens, handler)
    current = Vector.newBuilder[Token]

  groupTokens.foreach {
    case (Token.Space(_, _) | Token.Newline(_)) if depth == 0 => flush()
    case t =>
      t match
        case Token.BeginGroup(_) => depth += 1
        case Token.EndGroup(_)   => depth -= 1
        case _                   =>
      current += t
  }
  flush()
  items.result()

object SeqPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val groupTokens = stripWrappingGroups(proc.readArgument(pos))
    proc.setResult(Value.Seq(splitItems(groupTokens, proc.handler)))

/** \message{text} — expand the argument and write the resulting text to standard error at once, as TeX's \message
  * does, for tracing what a document or a macro-heavy package is doing while it runs. The argument is processed like
  * ordinary body text — literal characters pass through and control sequences expand — so \message{i=\i} or
  * \message{got \head{\xs}} interleave labels with computed state. It adds nothing to the page; output is captured
  * and diverted to the diagnostic channel only. */
object MessagePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val tokens = proc.readArgument(pos)
    System.err.println(proc.handler.capture(proc.processTokenList(tokens)))

object RangePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // Bounds are full expressions, so a computed endpoint (`\range{1}{\calc{n+1}}`, `\range{\i}{9}`) works
    // the same as a literal one.
    def bound(which: String): Int =
      val v = proc.evalArgumentExpr(pos)
      Value.number(v).map(_.toInt).getOrElse(proc.handler.error(s"Range $which must be a number, got ${Value.display(v)}", pos))
    val start = bound("start")
    val end   = bound("end")
    val items = (start to end).map(n => Value.Num(n)).toVector
    proc.setResult(Value.Seq(items))

// The Text cases below work in code points, not chars, so a string starting or ending in an astral symbol
// (an emoji, a math alphanumeric) yields the whole symbol rather than half a surrogate pair.

object HeadPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalArgumentExpr(pos)
    val result = arg match
      case Value.Seq(items) if items.nonEmpty => items.head
      case Value.Text(s) if s.nonEmpty => Value.Text(codePointStrings(s).head)
      case _ => Value.Nil
    proc.setResult(result)
    proc.handler.text(Value.display(result))

object TailPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalArgumentExpr(pos)
    val result = arg match
      case Value.Seq(items) if items.nonEmpty => Value.Seq(items.tail)
      case Value.Text(s) if s.nonEmpty => Value.Text(s.substring(codePointStrings(s).head.length))
      case _ => Value.Nil
    // value (usable as \tail in an expression) and the legacy `seq` variable it has always set
    proc.setResult(result)
    proc.handler.set("seq", result)

object LastPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalArgumentExpr(pos)
    val result = arg match
      case Value.Seq(items) if items.nonEmpty => items.last
      case Value.Text(s) if s.nonEmpty => Value.Text(codePointStrings(s).last)
      case _ => Value.Nil
    proc.setResult(result)
    proc.handler.text(Value.display(result))

// ============ MAP/OBJECT CREATION ============

object MapPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // Read key-value pairs from brace group - space separated
    // Syntax: \map{key1 value1 key2 value2}
    val groupTokens = stripWrappingGroups(proc.readArgument(pos))
    val pairs       = splitItems(groupTokens, proc.handler)
    if pairs.length % 2 != 0 then
      proc.handler.error("Map requires an even number of elements (key-value pairs)", pos)

    // VectorMap keeps declaration order, so \for over the map visits entries as written (a plain hash map
    // would iterate 5+ entries in per-platform hash order)
    val map = pairs.grouped(2).collect {
      case Vector(k, v) =>
        val key = k match
          case Value.Text(s) => s
          case Value.Num(n) => Value.display(Value.Num(n))
          case other => Value.display(other)
        key -> v
    }.to(scala.collection.immutable.VectorMap)

    proc.setResult(Value.Map(map))

/** `\mapset name {key} {value}` — store `value` under a computed `key` in the map variable `name` (creating the
  * map if absent). The key is any expression, so it can be built with `\calc`/string ops — this is texish's answer
  * to TeX's `\csname`, the keyed store the counters package and option machinery sit on. Honours a `\global`
  * prefix, so a global map (e.g. counters) updates across groups.
  */
object MapSetPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name   = proc.readIdentifier(pos)
    val global = proc.handler.globalAssign
    proc.handler.globalAssign = false
    val key   = Value.display(proc.evalArgumentExpr(pos))
    val value = proc.evalArgumentExpr(pos)
    val current = proc.handler.get(name) match
      case Value.Map(m) => m
      // a fresh map starts as a VectorMap so \for visits its keys in insertion order
      case _            => scala.collection.immutable.VectorMap.empty[String, Value]
    val updated = Value.Map(current + (key -> value))
    if global then proc.handler.setGlobal(name, updated) else proc.handler.set(name, updated)

/** `\mapget name {key}` — the value stored under a computed `key` in the map variable `name`, or Undefined if the
  * variable is not a map or has no such key. Sets a result, so it composes in expressions (`\set v {\mapget …}`).
  */
object MapGetPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name = proc.readIdentifier(pos)
    val key  = Value.display(proc.evalArgumentExpr(pos))
    proc.setResult(proc.handler.get(name) match
      case Value.Map(m) => m.getOrElse(key, Value.Undefined)
      case _            => Value.Undefined)

/** `\maphas name {key}` — whether the map variable `name` has a computed `key`. Sets a Bool result for `\if`. */
object MapHasPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name = proc.readIdentifier(pos)
    val key  = Value.display(proc.evalArgumentExpr(pos))
    proc.setResult(Value.Bool(proc.handler.get(name) match
      case Value.Map(m) => m.contains(key)
      case _            => false))
