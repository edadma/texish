package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader

/** Read a macro/environment parameter list up to the body's opening brace: space-separated entries where a bare
  * identifier is a mandatory parameter and a bracketed `[name:default]` (or `[name]` for an empty default) is an
  * optional one. The brackets are ordinary text, so an optional spec is scanned across tokens up to its `]`, exactly
  * as an optional argument is at the call site. Reading stops at the first token that is neither — in practice the
  * body's `{`. */
private[parser] def readMacroParams(proc: Processor, pos: CharReader): Vector[MacroParam] =
  val out = Vector.newBuilder[MacroParam]
  proc.skipSpaces()
  var done = false

  while !done && proc.hasMoreTokens do
    proc.peekToken() match
      case Token.Text(s, sp) if s.startsWith("[") =>
        proc.nextToken()
        val spec   = new StringBuilder
        var closed = false

        def takeText(str: String): Unit =
          val idx = str.indexOf(']')
          if idx < 0 then spec ++= str
          else
            spec ++= str.substring(0, idx)
            val after = str.substring(idx + 1)
            if after.nonEmpty then proc.pushBack(Vector(Token.Text(after, sp)))
            closed = true

        takeText(s.substring(1))
        while !closed && proc.hasMoreTokens do
          proc.nextToken() match
            case Token.Text(str, _) => takeText(str)
            case Token.EOF(_)       => closed = true
            case _                  => ()

        // [name:default] declares an optional parameter; [name] gives it an empty default
        val text       = spec.toString
        val colon      = text.indexOf(':')
        val name       = (if colon < 0 then text else text.substring(0, colon)).trim
        val default    = if colon < 0 then "" else text.substring(colon + 1)
        val defTokens  = if default.nonEmpty then Vector(Token.Text(default, sp)) else Vector.empty
        out += MacroParam(name, ParamKind.Optional(defTokens))
        proc.skipSpaces()

      case Token.Text(s, sp) if s.startsWith("*") =>
        // A bare `*` declares a star flag (named `star`); the tail after it continues the parameter list.
        proc.nextToken()
        val rest = s.substring(1)
        if rest.nonEmpty then proc.pushBack(Vector(Token.Text(rest, sp)))
        out += MacroParam("star", ParamKind.Star)
        proc.skipSpaces()

      case Token.Text(s, sp) if s.startsWith("<") =>
        // <name> declares a raw (verbatim) parameter: its argument is read literally at the call site. Read the
        // name up to the closing `>`, which may sit mid-token, pushing back whatever follows it.
        proc.nextToken()
        val spec   = new StringBuilder
        var closed = false

        def takeText(str: String): Unit =
          val idx = str.indexOf('>')
          if idx < 0 then spec ++= str
          else
            spec ++= str.substring(0, idx)
            val after = str.substring(idx + 1)
            if after.nonEmpty then proc.pushBack(Vector(Token.Text(after, sp)))
            closed = true

        takeText(s.substring(1))
        while !closed && proc.hasMoreTokens do
          proc.nextToken() match
            case Token.Text(str, _) => takeText(str)
            case Token.EOF(_)       => closed = true
            case _                  => ()
        out += MacroParam(spec.toString.trim, ParamKind.Raw)
        proc.skipSpaces()

      case Token.Text(s, _) if s.nonEmpty && s.head.isLetter =>
        proc.nextToken()
        out += MacroParam(s, ParamKind.Mandatory)
        proc.skipSpaces()

      case _ => done = true

  out.result()

object DefPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name = proc.readBindingName("def", pos)
    // capture and clear the \global flag up front, so evaluating params/body can't be confused by it
    val global = proc.handler.globalAssign
    proc.handler.globalAssign = false

    // Read the parameter list (mandatory names and optional [name:default] specs) up to the body brace.
    val params = readMacroParams(proc, pos)

    // Strip the braces that delimit the body in the \def syntax: a macro is pure token substitution and must not
    // open a scope of its own, so a \set or \coordinate in its body lands in the caller's scope, as in TeX.
    // Grouping inside a macro is whatever explicit { } the body itself contains.
    val body = proc.moduleBody(stripOuterBraces(proc.readArgument(pos)))
    val mac  = Value.Macro(params, body, pos)

    if global then proc.handler.setGlobal(name, mac) else proc.handler.set(name, mac)

object GdefPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    proc.handler.globalAssign = true
    DefPrimitive.execute(proc, pos)

/** `\global` — a prefix that makes the following assignment (\set, \def, …) global. */
object GlobalPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    proc.handler.globalAssign = true

/** `\let newname oldname` — give `newname` the current meaning of `oldname`, snapshotting it. A macro or variable
  * meaning is copied through the scope-aware store (so `\let` is local to its group, and honours a `\global`
  * prefix); a built-in primitive is aliased by binding the same primitive under the new name. The snapshot is of
  * the meaning *now*: a later redefinition of `oldname` does not change `newname`, as in TeX. */
object LetPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name   = proc.readBindingName("let", pos)
    val source = proc.readIdentifier(pos)
    val global = proc.handler.globalAssign
    proc.handler.globalAssign = false
    proc.handler.get(source) match
      case Value.Undefined =>
        // Not a macro or variable — alias a built-in primitive if there is one (primitive bindings are global).
        proc.lookupPrimitive(source) match
          case Some(prim) => proc.registerPrimitive(name, prim)
          case None       => proc.handler.error(s"\\let: '$source' has no meaning to copy", pos)
      case meaning =>
        if global then proc.handler.setGlobal(name, meaning) else proc.handler.set(name, meaning)

object SetPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name = proc.readBindingName("set", pos)
    // capture and clear the \global flag before evaluating the value, so a nested assignment in the value
    // expression can't steal it
    val global = proc.handler.globalAssign
    proc.handler.globalAssign = false
    val value = proc.evalArgumentExpr(pos)
    if global then proc.handler.setGlobal(name, value) else proc.handler.set(name, value)

object IfPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // Evaluate the condition expression
    val condValue = proc.evalArgumentExpr(pos)
    val cond = condValue match
      case Value.Bool(b)     => b
      case Value.Text(s)     => s.trim.nonEmpty && s.trim != "0" && s.trim.toLowerCase != "false"
      case Value.Num(n)      => n != 0
      case Value.Nil         => false
      case Value.Undefined   => false
      case Value.Seq(items)  => items.nonEmpty
      case _                 => true

    if !cond then
      // Skip to \else or \fi
      val hasElse = proc.skipToElseOrFi()
      // If there's no else, we're done

/** Two tokens are the same token where it matters for a comparison of meanings: their content, ignoring the source
  * position each carries for error reporting. Two macros written identically in two places must compare equal, and
  * their bodies never share positions. */
private def sameToken(a: Token, b: Token): Boolean = (a, b) match
  case (Token.Text(x, _), Token.Text(y, _))       => x == y
  case (Token.ControlSeq(x, _), Token.ControlSeq(y, _)) => x == y
  case (Token.Active(x, _), Token.Active(y, _))   => x == y
  case (Token.Space(x, _), Token.Space(y, _))     => x == y
  case (Token.BeginGroup(_), Token.BeginGroup(_)) => true
  case (Token.EndGroup(_), Token.EndGroup(_))     => true
  case (Token.Newline(_), Token.Newline(_))       => true
  case (Token.EOF(_), Token.EOF(_))               => true
  case _                                          => false

private def sameTokens(a: Vector[Token], b: Vector[Token]): Boolean =
  a.length == b.length && a.lazyZip(b).forall(sameToken)

private def sameParams(a: Vector[MacroParam], b: Vector[MacroParam]): Boolean =
  a.length == b.length && a.lazyZip(b).forall { (x, y) =>
    x.name == y.name && ((x.kind, y.kind) match
      case (ParamKind.Optional(d1), ParamKind.Optional(d2)) => sameTokens(d1, d2)
      case (k1, k2)                                         => k1 == k2)
  }

/** Whether two control sequences *mean* the same thing, which is what TeX's `\ifx` asks.
  *
  * Two macros are alike when they take the same parameters and have the same body — so a `\let` copy compares equal
  * to its original, and one macro compares equal to another written the same way. Two names with no meaning at all
  * are alike, which is the test a package uses to ask whether an option was ever defined. A primitive is itself.
  * Anything else is a value, and values compare as `\=` compares them.
  *
  * This used to compare the two *names* instead — `\ifx \a \b` was false however `\a` and `\b` were defined, and
  * `\ifx \a \a` was true even for a name that meant nothing, so the one question `\ifx` exists to answer could not
  * be asked. */
private def sameMeaning(proc: Processor, n1: String, n2: String): Boolean =
  (proc.handler.get(n1), proc.handler.get(n2)) match
    case (Value.Macro(p1, b1, _), Value.Macro(p2, b2, _)) => sameParams(p1, p2) && sameTokens(b1, b2)
    case (Value.Macro(_, _, _), _) | (_, Value.Macro(_, _, _)) => false
    case (Value.Undefined, Value.Undefined) =>
      (proc.lookupPrimitive(n1), proc.lookupPrimitive(n2)) match
        case (Some(a), Some(b)) => a eq b
        case (None, None)       => true
        case _                  => false
    case (a, b) => valuesEqual(a, b)

object IfxPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // \ifx compares the next two tokens; interword spaces are separators, not operands, so `\ifx \a \a`
    // compares the two control sequences rather than a space against the first of them
    proc.skipSpaces()
    val tok1 = proc.nextToken()
    proc.skipSpaces()
    val tok2 = proc.nextToken()
    val equal = (tok1, tok2) match
      case (Token.ControlSeq(n1, _), Token.ControlSeq(n2, _)) => sameMeaning(proc, n1, n2)
      case (Token.Text(s1, _), Token.Text(s2, _))             => s1 == s2
      case _                                                   => false

    if !equal then proc.skipToElseOrFi()

object ElsePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // If we hit \else during normal processing, skip to \fi
    proc.skipToFi()

object FiPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit = ()
    // End of conditional - nothing to do

object ThePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // \the outputs the value of a variable, and sets it as the result so it composes in an expression
    // (`\set x {\the\pageno}` stores the value, not Nil)
    val name = proc.readControlSeqName(pos)
    val value = proc.handler.get(name)
    proc.setResult(value)
    proc.handler.text(Value.display(value))

/** Strip the outer brace pair that readArgument preserves for scoping. Value
  * evaluation wants the inner content only.
  */
def stripOuterBraces(tokens: Vector[Token]): Vector[Token] =
  tokens match
    case Vector(Token.BeginGroup(_), rest*) if rest.nonEmpty && rest.last.isInstanceOf[Token.EndGroup] =>
      rest.init.toVector
    case other => other

/** True when the tokens are a single balanced group wrapping everything — `{ … }` whose opening brace is closed
  * only by the final token (so `{a}{b}`, whose first brace closes early, is not a wrapper). */
def isSingleWrappingGroup(tokens: Vector[Token]): Boolean =
  tokens.length >= 2 && tokens.head.isInstanceOf[Token.BeginGroup] && tokens.last.isInstanceOf[Token.EndGroup] && {
    var depth       = 0
    var closedEarly = false
    for (t, i) <- tokens.zipWithIndex do
      t match
        case _: Token.BeginGroup => depth += 1
        case _: Token.EndGroup   => depth -= 1; if depth == 0 && i != tokens.length - 1 then closedEarly = true
        case _                   =>
    depth == 0 && !closedEarly
  }

/** Strip every brace layer that wraps the whole token vector — `{{x}}` → `x` — while leaving inner groups that
  * are only part of the content (`{a {b} c}` → `a {b} c`, no further). Primitives that split their argument into
  * pieces (\seq, \map) need this: macro-parameter substitution wraps an argument in an extra `{…}` layer, and a
  * lone strip would leave that wrapper's braces to split off as spurious empty pieces. */
def stripWrappingGroups(tokens: Vector[Token]): Vector[Token] =
  var cur = tokens
  while isSingleWrappingGroup(cur) do cur = cur.tail.init
  cur
