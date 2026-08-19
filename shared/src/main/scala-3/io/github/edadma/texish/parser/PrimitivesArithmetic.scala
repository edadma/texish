package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader

// ============ ARITHMETIC ============

object AddPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = proc.evalArgumentExpr(pos)
    val b = proc.evalArgumentExpr(pos)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => Value.Num(x + y)
      case (Value.Text(x), Value.Text(y)) => Value.Text(x + y)
      case (Value.Seq(x), Value.Seq(y)) => Value.Seq(x ++ y)
      case _ => proc.handler.error(s"Cannot add ${Value.display(a)} and ${Value.display(b)}", pos)
    proc.setResult(result)
    proc.handler.text(Value.display(result))

object SubPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = proc.evalArgumentExpr(pos)
    val b = proc.evalArgumentExpr(pos)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => Value.Num(x - y)
      case _ => proc.handler.error(s"Cannot subtract ${Value.display(a)} and ${Value.display(b)}", pos)
    proc.setResult(result)
    proc.handler.text(Value.display(result))

object MulPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = proc.evalArgumentExpr(pos)
    val b = proc.evalArgumentExpr(pos)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => Value.Num(x * y)
      case _ => proc.handler.error(s"Cannot multiply ${Value.display(a)} and ${Value.display(b)}", pos)
    proc.setResult(result)
    proc.handler.text(Value.display(result))

object DivPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = proc.evalArgumentExpr(pos)
    val b = proc.evalArgumentExpr(pos)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) =>
        if y == 0 then proc.handler.error("Division by zero", pos)
        Value.Num(x / y)
      case _ => proc.handler.error(s"Cannot divide ${Value.display(a)} and ${Value.display(b)}", pos)
    proc.setResult(result)
    proc.handler.text(Value.display(result))

/** `\calc{expr}` — evaluate an infix arithmetic expression (with functions, constants and length units; see
  * [[MathExpr]]) to a number. Bare identifiers that are not built-in constants resolve to document variables, so
  * `\calc{2*x + cosd(angle)}` reads `x` and `angle` from scope. This is how a document does trigonometry and the
  * like — the function library lives inside the expression, not as a control sequence per operation. */
object CalcPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val (text, calls) = exprText(proc, stripOuterBraces(proc.readArgument(pos)), pos)
    val result =
      try
        MathExpr.eval(
          text,
          name => calls.get(name).orElse(Value.number(proc.handler.get(name))),
          proc.handler.fontUnit,
        )
      catch case e: MathExpr.MathExprException => proc.handler.error(e.getMessage, pos)
    proc.setResult(Value.Num(result))
    proc.handler.text(Value.display(Value.Num(result)))

/** `\round{value}{places}` — round a number to a fixed number of decimal places and emit it minimally. The
  * value and the place count are each evaluated as expressions, so `\round{\calc{x/3}}{2}` works. Unlike a
  * printf "%.2f", the result is displayed the same way every other number is (whole values lose their `.0`,
  * trailing zeros drop): rounding `0.1 + 0.2` to 2 places gives `0.3`, not `0.30`. This is what turns a tick
  * value computed by accumulation — which carries floating-point noise like `0.30000000000000004` — into a
  * clean axis label, but it is a general numeric helper, not specific to plots. The rounded number is also set
  * as the capturable result, so `\set v {\round{\x}{3}}` stores a number. */
object RoundPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val value  = numberArg(proc, pos)
    val places = numberArg(proc, pos).toInt
    val factor = math.pow(10, places)
    val result = math.round(value * factor) / factor
    proc.setResult(Value.Num(result))
    proc.handler.text(Value.display(Value.Num(result)))

  private def numberArg(proc: Processor, pos: CharReader): Double =
    val v = proc.evalArgumentExpr(pos)
    Value.number(v).getOrElse(proc.handler.error(s"\\round expects a number, got ${Value.display(v)}", pos))

/** Flatten an argument's tokens back into the raw expression string [[MathExpr]] parses, evaluating any call it
  * contains on the way.
  *
  * A bare control sequence contributes its name, so `\x` and `\pi` read as the identifiers `x` and `pi` and a
  * document variable is usable in arithmetic under either spelling. An active character (notably `^`) contributes
  * its character, so `\calc{2^\x}` works.
  *
  * **A control sequence followed by a braced group is a call, not a name.** `\calc` reads its argument as an
  * expression *string*, so flattening alone turned `\nth{\p}{1} * 2` into the identifier `nthp1` and the whole
  * thing failed with "unknown name" — arithmetic could not reach anything the data primitives computed, and the
  * idiom was to bind a variable first purely to work around it. Such a run is evaluated here as an expression, and
  * what it produced is bound to a private name the expression grammar then reads. Those names carry a digit, which
  * `\set` and `\def` refuse, so no document variable can collide with one.
  *
  * The call takes the control sequence and every braced group immediately after it, which is how a primitive call
  * is written; a `\macro` used this way must take its arguments in braces too.
  *
  * **A dotted field is read the same way.** `\forloop.index` is a field of a map, not an identifier, so flattening
  * gave `forloop.index` and the expression failed on the name `forloop`. The field is evaluated and bound like a
  * call, and whatever follows it in the same text token — `\forloop.indexz * histW` arrives as one token — carries
  * on as expression text.
  */
private def exprText(proc: Processor, tokens: Vector[Token], pos: CharReader): (String, Map[String, Double]) =
  val out   = new StringBuilder
  val calls = collection.mutable.Map.empty[String, Double]
  var i     = 0

  /** The index just past a balanced group starting at `from`, or -1 if there is no group there. */
  def groupEnd(from: Int): Int =
    if from >= tokens.length || !tokens(from).isInstanceOf[Token.BeginGroup] then -1
    else
      var depth = 0
      var j     = from
      var end   = -1
      while j < tokens.length && end < 0 do
        tokens(j) match
          case Token.BeginGroup(_) => depth += 1
          case Token.EndGroup(_)   => depth -= 1; if depth == 0 then end = j + 1
          case _                   =>
        j += 1
      end

  /** Bind a value under a private name the expression grammar can read, and write that name into the text. The
    * names carry a digit, which `\set` and `\def` refuse, so no document variable can collide with one. */
  def bind(what: String, value: Value, pos: CharReader, proc: Processor): Unit =
    val name = s"call0arg${calls.size}"
    calls(name) = Value
      .number(value)
      .getOrElse(proc.handler.error(s"\\calc: $what gave ${Value.display(value)}, which is not a number", pos))
    out ++= name

  while i < tokens.length do
    tokens(i) match
      // \var.field, whose field name runs to the first character that cannot be part of one
      case Token.ControlSeq(n, csPos) if i + 1 < tokens.length && (tokens(i + 1) match
            case Token.Text(t, _) => t.startsWith(".")
            case _                => false
          ) =>
        val Token.Text(t, tPos) = tokens(i + 1): @unchecked
        val rest     = t.drop(1)
        val fieldEnd = rest.indexWhere(c => !c.isLetterOrDigit && c != '_')
        val field    = if fieldEnd < 0 then rest else rest.substring(0, fieldEnd)
        val tail     = if fieldEnd < 0 then "" else rest.substring(fieldEnd)
        bind(s"\\$n.$field", proc.evalExpr(Vector(tokens(i), Token.Text("." + field, tPos)), pos), pos, proc)
        out ++= tail
        i += 2
      case Token.ControlSeq(n, _) if groupEnd(i + 1) > 0 =>
        var end = i + 1
        while groupEnd(end) > 0 do end = groupEnd(end)
        bind(s"\\$n", proc.evalExpr(tokens.slice(i, end), pos), pos, proc)
        i = end
      case Token.Text(s, _)       => out ++= s; i += 1
      case Token.Space(s, _)      => out ++= s; i += 1
      case Token.Newline(_)       => out ++= " "; i += 1
      case Token.ControlSeq(n, _) => out ++= n; i += 1
      case Token.Active(c, _)     => out ++= c.toString; i += 1
      case _                      => i += 1

  (out.toString, calls.toMap)

// ============ COMPARISON ============

/** Order two values when they are not the same kind: if both interpret as numbers (a `Num`, a `Dimen`, or a
  * numeric-string `Text`) compare them numerically, otherwise it is a genuine type mismatch and an error. The
  * ordering comparisons fall back to this so a number compares cleanly against a numeric string — the common case
  * when one operand came from a sequence element (which can be text-typed) and the other is a computed number. */
private def orderMismatch(a: Value, b: Value, num: (Double, Double) => Boolean, proc: Processor, pos: CharReader): Boolean =
  (Value.number(a), Value.number(b)) match
    case (Some(x), Some(y)) => num(x, y)
    case _                  => proc.handler.error(s"Cannot compare ${Value.display(a)} and ${Value.display(b)}", pos)

/** Whether a value is an absence rather than a thing: an unset variable is Undefined, an empty argument `{}` is
  * Nil, and no document can tell the two apart — they display alike and test alike. */
private def absent(v: Value): Boolean = v match
  case Value.Nil | Value.Undefined => true
  case _                           => false

/** Equality across value kinds, shared by `\=` and `\!=` so they stay exact negations.
  *
  * Same-kind values compare directly, and a sequence or map compares by its contents so `\= {\seq{a b}}
  * {\seq{a b}}` is true rather than falling through to a numeric test it cannot pass. A kind mismatch falls back
  * to numeric comparison when both sides interpret as numbers — the same coercion the ordering comparisons apply,
  * so `\={\x}{5}` is true whether `\x` holds `Num(5)` or `Text("5")` (a sequence element is text-typed even when
  * it looks numeric). A non-numeric kind mismatch is simply unequal.
  *
  * **Absence equals absence.** `\= {\x} {}` used to be *false* for an unset `\x`, because Undefined and Nil are
  * different kinds and neither reads as a number, so the test fell through to the numeric case and failed — the
  * one comparison every package wants to write, answering wrongly and silently. Package state had to carry a
  * sentinel (`-`) for "no value", or ask `\> {\size{\x}} {0}` instead. */
private def valuesEqual(a: Value, b: Value): Boolean =
  (a, b) match
    case _ if absent(a) || absent(b) => absent(a) && absent(b)
    case (Value.Num(x), Value.Num(y))   => x == y
    case (Value.Text(x), Value.Text(y)) => x == y
    case (Value.Bool(x), Value.Bool(y)) => x == y
    case (Value.Seq(x), Value.Seq(y))   => x.length == y.length && x.lazyZip(y).forall(valuesEqual)
    case (Value.Map(x), Value.Map(y)) =>
      x.keySet == y.keySet && x.forall((k, v) => valuesEqual(v, y(k)))
    case _ =>
      (Value.number(a), Value.number(b)) match
        case (Some(x), Some(y)) => x == y
        case _                  => false

object EqPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = proc.evalArgumentExpr(pos)
    val b = proc.evalArgumentExpr(pos)
    val result = valuesEqual(a, b)
    proc.setResult(Value.Bool(result))
    if result then proc.handler.text("true") else proc.handler.text("false")

object LtPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = proc.evalArgumentExpr(pos)
    val b = proc.evalArgumentExpr(pos)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => x < y
      case (Value.Text(x), Value.Text(y)) => x < y
      case _ => orderMismatch(a, b, _ < _, proc, pos)
    proc.setResult(Value.Bool(result))
    if result then proc.handler.text("true") else proc.handler.text("false")

object GtPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = proc.evalArgumentExpr(pos)
    val b = proc.evalArgumentExpr(pos)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => x > y
      case (Value.Text(x), Value.Text(y)) => x > y
      case _ => orderMismatch(a, b, _ > _, proc, pos)
    proc.setResult(Value.Bool(result))
    if result then proc.handler.text("true") else proc.handler.text("false")

object LePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = proc.evalArgumentExpr(pos)
    val b = proc.evalArgumentExpr(pos)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => x <= y
      case (Value.Text(x), Value.Text(y)) => x <= y
      case _ => orderMismatch(a, b, _ <= _, proc, pos)
    proc.setResult(Value.Bool(result))
    if result then proc.handler.text("true") else proc.handler.text("false")

object GePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = proc.evalArgumentExpr(pos)
    val b = proc.evalArgumentExpr(pos)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => x >= y
      case (Value.Text(x), Value.Text(y)) => x >= y
      case _ => orderMismatch(a, b, _ >= _, proc, pos)
    proc.setResult(Value.Bool(result))
    if result then proc.handler.text("true") else proc.handler.text("false")

object NePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = proc.evalArgumentExpr(pos)
    val b = proc.evalArgumentExpr(pos)
    val result = !valuesEqual(a, b)
    proc.setResult(Value.Bool(result))
    if result then proc.handler.text("true") else proc.handler.text("false")
