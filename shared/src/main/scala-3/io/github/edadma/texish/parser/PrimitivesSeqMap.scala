package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader

// ============ SEQUENCE FUNCTIONS ============

object SeqPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // Read items from brace group - space separated
    val items = Vector.newBuilder[Value]
    val groupTokens = stripWrappingGroups(proc.readArgument(pos))
    // Parse items from the group - space separated
    var current = Vector.newBuilder[Token]
    groupTokens.foreach {
      case Token.Space(_, _) =>
        val tokens = current.result()
        if tokens.nonEmpty then items += evalTokens(tokens, proc.handler)
        current = Vector.newBuilder[Token]
      case t =>
        current += t
    }
    val tokens = current.result()
    if tokens.nonEmpty then items += evalTokens(tokens, proc.handler)

    proc.setResult(Value.Seq(items.result()))

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
    val startTokens = proc.readArgument(pos)
    val endTokens = proc.readArgument(pos)
    val start = evalTokens(startTokens, proc.handler) match
      case Value.Num(n) => n.toInt
      case _ => proc.handler.error("Range start must be a number", pos)
    val end = evalTokens(endTokens, proc.handler) match
      case Value.Num(n) => n.toInt
      case _ => proc.handler.error("Range end must be a number", pos)
    val items = (start to end).map(n => Value.Num(n)).toVector
    proc.setResult(Value.Seq(items))

object HeadPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalArgumentExpr(pos)
    val result = arg match
      case Value.Seq(items) if items.nonEmpty => items.head
      case Value.Text(s) if s.nonEmpty => Value.Text(s.head.toString)
      case _ => Value.Nil
    proc.setResult(result)
    proc.handler.text(Value.display(result))

object TailPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalArgumentExpr(pos)
    val result = arg match
      case Value.Seq(items) if items.nonEmpty => Value.Seq(items.tail)
      case Value.Text(s) if s.nonEmpty => Value.Text(s.tail)
      case _ => Value.Nil
    // value (usable as \tail in an expression) and the legacy `seq` variable it has always set
    proc.setResult(result)
    proc.handler.set("seq", result)

object LastPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalArgumentExpr(pos)
    val result = arg match
      case Value.Seq(items) if items.nonEmpty => items.last
      case Value.Text(s) if s.nonEmpty => Value.Text(s.last.toString)
      case _ => Value.Nil
    proc.setResult(result)
    proc.handler.text(Value.display(result))

// ============ MAP/OBJECT CREATION ============

object MapPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // Read key-value pairs from brace group - space separated
    // Syntax: \map{key1 value1 key2 value2}
    val items = Vector.newBuilder[Value]
    val groupTokens = stripWrappingGroups(proc.readArgument(pos))
    // Parse items from the group - space separated
    var current = Vector.newBuilder[Token]
    groupTokens.foreach {
      case Token.Space(_, _) =>
        val tokens = current.result()
        if tokens.nonEmpty then items += evalTokens(tokens, proc.handler)
        current = Vector.newBuilder[Token]
      case t =>
        current += t
    }
    val tokens = current.result()
    if tokens.nonEmpty then items += evalTokens(tokens, proc.handler)

    val pairs = items.result()
    if pairs.length % 2 != 0 then
      proc.handler.error("Map requires an even number of elements (key-value pairs)", pos)

    val map = pairs.grouped(2).collect {
      case Vector(k, v) =>
        val key = k match
          case Value.Text(s) => s
          case Value.Num(n) => Value.display(Value.Num(n))
          case other => Value.display(other)
        key -> v
    }.toMap

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
      case _            => scala.collection.immutable.Map.empty[String, Value]
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
