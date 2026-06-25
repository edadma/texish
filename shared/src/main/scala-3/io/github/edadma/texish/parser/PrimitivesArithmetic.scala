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
    val text = exprText(stripOuterBraces(proc.readArgument(pos)))
    val result =
      try
        MathExpr.eval(
          text,
          name => Value.number(proc.handler.get(name)),
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

/** Flatten an argument's tokens back into the raw expression string [[MathExpr]] parses. A control sequence
  * contributes its bare name (so `\x` and `\pi` read as the identifiers `x` and `pi`), and an active character
  * (notably `^`) contributes its character, so `\calc{2^\x}` works. */
private def exprText(tokens: Vector[Token]): String =
  tokens.map {
    case Token.Text(s, _)       => s
    case Token.Space(s, _)      => s
    case Token.Newline(_)       => " "
    case Token.ControlSeq(n, _) => n
    case Token.Active(c, _)     => c.toString
    case _                      => ""
  }.mkString

// ============ COMPARISON ============

/** Order two values when they are not the same kind: if both interpret as numbers (a `Num`, a `Dimen`, or a
  * numeric-string `Text`) compare them numerically, otherwise it is a genuine type mismatch and an error. The
  * ordering comparisons fall back to this so a number compares cleanly against a numeric string — the common case
  * when one operand came from a sequence element (which can be text-typed) and the other is a computed number. */
private def orderMismatch(a: Value, b: Value, num: (Double, Double) => Boolean, proc: Processor, pos: CharReader): Boolean =
  (Value.number(a), Value.number(b)) match
    case (Some(x), Some(y)) => num(x, y)
    case _                  => proc.handler.error(s"Cannot compare ${Value.display(a)} and ${Value.display(b)}", pos)

object EqPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = proc.evalArgumentExpr(pos)
    val b = proc.evalArgumentExpr(pos)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => x == y
      case (Value.Text(x), Value.Text(y)) => x == y
      case (Value.Bool(x), Value.Bool(y)) => x == y
      case _ => false
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
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => x != y
      case (Value.Text(x), Value.Text(y)) => x != y
      case (Value.Bool(x), Value.Bool(y)) => x != y
      case _ => true
    proc.setResult(Value.Bool(result))
    if result then proc.handler.text("true") else proc.handler.text("false")
