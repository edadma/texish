package io.github.edadma.typesetter.texish

import io.github.edadma.char_reader.CharReader
import scala.collection.mutable

/** The streaming processor/expander for texish.
  *
  * This is the core engine that reads tokens, expands macros, and calls the handler. Unlike an AST-based interpreter,
  * this processes tokens as they come, expanding macros immediately.
  *
  * @param handler The handler to receive events
  */
class Processor(val handler: Handler):
  // Token sources form a stack - macro expansions push new sources
  private val tokenSources = mutable.Stack[TokenSource]()

  // Result from last expression evaluation (used by \for, etc.)
  private var lastResult: Value = Value.Nil

  // Built-in primitives
  private val primitives = mutable.Map[String, Primitive]()

  // Registered active character handlers
  private val actives = mutable.Map[Char, Active]()

  // Register default primitives
  registerPrimitive("def", DefPrimitive)
  registerPrimitive("gdef", GdefPrimitive)
  registerPrimitive("set", SetPrimitive)
  registerPrimitive("if", IfPrimitive)
  registerPrimitive("ifx", IfxPrimitive)
  registerPrimitive("else", ElsePrimitive)
  registerPrimitive("fi", FiPrimitive)
  registerPrimitive("the", ThePrimitive)

  // Loop primitives
  registerPrimitive("for", ForPrimitive)
  registerPrimitive("done", DonePrimitive)

  // File inclusion
  registerPrimitive("include", IncludePrimitive)

  // Arithmetic primitives
  registerPrimitive("+", AddPrimitive)
  registerPrimitive("-", SubPrimitive)
  registerPrimitive("*", MulPrimitive)
  registerPrimitive("/", DivPrimitive)

  // Comparison primitives
  registerPrimitive("=", EqPrimitive)
  registerPrimitive("<", LtPrimitive)
  registerPrimitive(">", GtPrimitive)
  registerPrimitive("<=", LePrimitive)
  registerPrimitive(">=", GePrimitive)
  registerPrimitive("!=", NePrimitive)

  // String primitives
  registerPrimitive("upcase", UpcasePrimitive)
  registerPrimitive("downcase", DowncasePrimitive)
  registerPrimitive("trim", TrimPrimitive)
  registerPrimitive("size", SizePrimitive)

  // Sequence primitives
  registerPrimitive("seq", SeqPrimitive)
  registerPrimitive("range", RangePrimitive)
  registerPrimitive("head", HeadPrimitive)
  registerPrimitive("tail", TailPrimitive)
  registerPrimitive("last", LastPrimitive)

  // Map/object primitive
  registerPrimitive("map", MapPrimitive)

  // Escape sequences for special characters
  registerPrimitive("{", LiteralPrimitive("{"))
  registerPrimitive("}", LiteralPrimitive("}"))
  registerPrimitive("%", LiteralPrimitive("%"))
  registerPrimitive("\\", LiteralPrimitive("\\"))
  registerPrimitive("~", LiteralPrimitive("~"))

  def registerPrimitive(name: String, prim: Primitive): Unit =
    primitives(name) = prim

  /** Register an active character handler.
    *
    * When the given character is encountered in input, it will be treated as an active character
    * and the handler's execute method will be called.
    *
    * @param c The character to treat as active
    * @param active The handler to call when this character is encountered
    */
  def registerActive(c: Char, active: Active): Unit =
    actives(c) = active

  /** Get the set of active characters (registered actives + default ~) */
  def activeChars: Set[Char] = actives.keySet.toSet + '~'

  /** Process input from a string */
  def process(input: String): Unit =
    process(Tokenizer(input, activeChars))

  /** Process input from a tokenizer */
  def process(tokenizer: Tokenizer): Unit =
    tokenSources.push(TokenizerSource(tokenizer))
    processTokens()
    tokenSources.pop()

  /** Main processing loop */
  private def processTokens(): Unit =
    while hasMoreTokens do
      val token = nextToken()
      token match
        case Token.Text(s, _)       => handler.text(s)
        case Token.Space(_, _)      => handler.space()
        case Token.Newline(_)       => handler.newline()
        case Token.BeginGroup(pos)  => handleBeginGroup(pos)
        case Token.EndGroup(pos)    => handleEndGroup(pos)
        case Token.ControlSeq(name, pos) => handleControlSeq(name, pos)
        case Token.Active(c, pos)   => handleActive(c, pos)
        case Token.EOF(_)           => // done

  private def handleBeginGroup(pos: CharReader): Unit =
    handler.enterScope()

  private def handleEndGroup(pos: CharReader): Unit =
    handler.exitScope()

  private def handleControlSeq(name: String, pos: CharReader): Unit =
    // Check for primitive first
    primitives.get(name) match
      case Some(prim) =>
        prim.execute(this, pos)
      case None =>
        // Check for macro
        handler.get(name) match
          case Value.Macro(params, body, _) =>
            expandMacro(name, params, body, pos)
          case Value.Undefined =>
            // Unknown command - pass to handler
            val result = handler.command(name, Seq.empty, pos)
            outputValue(result)
          case mapValue @ Value.Map(entries) =>
            // Check for dotted access like \forloop.index
            if hasMoreTokens then
              peekToken() match
                case Token.Text(s, _) if s.startsWith(".") =>
                  nextToken() // consume the text token
                  val rest = s.drop(1) // remove the dot
                  // Find the field name (up to any non-identifier char)
                  val fieldEnd = rest.indexWhere(c => !c.isLetterOrDigit && c != '_')
                  val (field, remaining) = if fieldEnd == -1 then (rest, "") else rest.splitAt(fieldEnd)
                  entries.get(field) match
                    case Some(v) =>
                      outputValue(v)
                      // Output any remaining text after the field
                      if remaining.nonEmpty then handler.text(remaining)
                    case None => outputValue(Value.Undefined)
                case _ =>
                  outputValue(mapValue)
            else
              outputValue(mapValue)
          case other =>
            // It's a variable - output its value
            outputValue(other)

  private def handleActive(c: Char, pos: CharReader): Unit =
    // Check for registered active handler first
    actives.get(c) match
      case Some(active) =>
        active.execute(this, c, pos)
      case None =>
        // Check if there's a macro defined for this active character
        val name = s"active:$c"
        handler.get(name) match
          case Value.Macro(params, body, _) =>
            expandMacro(name, params, body, pos)
          case _ =>
            // Default behavior for ~: non-breaking space
            if c == '~' then handler.text("\u00A0")
            else handler.text(c.toString)

  private def expandMacro(name: String, params: Vector[String], body: Vector[Token], pos: CharReader): Unit =
    // Read arguments
    val args = readMacroArgs(params.size, pos)

    // Create parameter map (param name -> argument tokens)
    val paramMap = params.zip(args).toMap

    // Substitute parameters in body (control sequences matching param names)
    val expandedBody = substituteNamedParams(body, paramMap)

    // Push expanded tokens as new source
    tokenSources.push(TokenListSource(expandedBody))

  private def readMacroArgs(count: Int, pos: CharReader): Vector[Vector[Token]] =
    (0 until count).map(_ => readArgument(pos)).toVector

  /** Read a single macro argument (brace-delimited or single token) */
  def readArgument(pos: CharReader): Vector[Token] =
    skipSpaces()
    if !hasMoreTokens then
      handler.error("Unexpected end of input while reading argument", pos)

    peekToken() match
      case begin @ Token.BeginGroup(_) =>
        nextToken() // consume {
        // Include the braces so processTokenList will trigger enterScope/exitScope
        Vector(begin) ++ readBalancedGroup() :+ Token.EndGroup(begin.pos)
      case _ =>
        val tok = Vector(nextToken())
        skipSpaces() // consume trailing whitespace after non-braced argument (matches texish-old behavior)
        tok

  /** Read tokens until matching } */
  private def readBalancedGroup(): Vector[Token] =
    val tokens = Vector.newBuilder[Token]
    var depth = 1
    while depth > 0 && hasMoreTokens do
      val t = nextToken()
      t match
        case Token.BeginGroup(_) =>
          depth += 1
          tokens += t
        case Token.EndGroup(_) =>
          depth -= 1
          if depth > 0 then tokens += t
        case Token.EOF(pos) =>
          handler.error("Unexpected end of input in group", pos)
        case _ =>
          tokens += t
    tokens.result()

  private def substituteNamedParams(body: Vector[Token], params: Map[String, Vector[Token]]): Vector[Token] =
    body.flatMap {
      case Token.ControlSeq(name, _) if params.contains(name) =>
        // Replace \paramname with the argument tokens
        params(name)
      case t => Vector(t)
    }

  private def outputValue(v: Value): Unit =
    val s = Value.display(v)
    if s.nonEmpty then handler.text(s)

  // Token source management
  def hasMoreTokens: Boolean =
    while tokenSources.nonEmpty && tokenSources.top.atEnd do
      if tokenSources.size > 1 then tokenSources.pop()
      else return false
    tokenSources.nonEmpty && !tokenSources.top.atEnd

  def peekToken(): Token =
    if hasMoreTokens then tokenSources.top.peek
    else Token.EOF(null)

  def nextToken(): Token =
    if hasMoreTokens then tokenSources.top.next()
    else Token.EOF(null)

  def skipSpaces(): Unit =
    while hasMoreTokens && (peekToken() match
        case Token.Space(_, _) => true
        case _                 => false
      )
    do nextToken()

  /** Read optional parameters in the form `name:value`.
    *
    * Reads zero or more `name:value` pairs that appear before regular arguments.
    * Stops when it encounters something that doesn't match the `name:` pattern.
    *
    * Examples:
    *   - `to:100` -> Map("to" -> Value.Num(100))
    *   - `width:50 height:30` -> Map("width" -> Value.Num(50), "height" -> Value.Num(30))
    *   - `to:\hsize` -> Map("to" -> <value of \hsize>)
    *
    * @param pos Position for error reporting
    * @return Map of parameter names to their values
    */
  def readOptionalParams(pos: CharReader): Map[String, Value] =
    val params = scala.collection.mutable.Map[String, Value]()

    @scala.annotation.tailrec
    def loop(): Unit =
      skipSpaces()
      if !hasMoreTokens then return

      peekToken() match
        case Token.Text(s, _) if s.contains(':') =>
          val colonIdx = s.indexOf(':')
          val name = s.substring(0, colonIdx)
          val rest = s.substring(colonIdx + 1)

          // Validate name is an identifier
          if name.nonEmpty && name.head.isLetter && name.forall(c => c.isLetterOrDigit || c == '_') then
            nextToken() // consume the token

            // Get the value
            val value =
              if rest.nonEmpty then
                // Value is in the same token after the colon (e.g., "to:100")
                parseSimpleValue(rest)
              else
                // Value is in the next argument (e.g., "to:" followed by "{...}" or "\var")
                evalArgumentExpr(pos)

            params(name) = value
            loop()
          // else: not a valid name:value pattern, stop

        case Token.Text(s, textPos) if s.nonEmpty && s.head.isLetter && s.forall(c => c.isLetterOrDigit || c == '_') =>
          // Might be `name` followed by `:value` in next token - peek ahead
          // For now, only handle the `name:value` case above
          // Stop here as this looks like a regular argument
          ()

        case _ =>
          // Not a text token, stop looking for optional params
          ()

    loop()
    params.toMap

  /** Parse a simple value from a string (number or text) */
  private def parseSimpleValue(s: String): Value =
    try Value.Num(BigDecimal(s))
    catch case _: Exception => Value.Text(s)

  /** Process a list of tokens (used by primitives like \for) */
  def processTokenList(tokens: Vector[Token]): Unit =
    val minDepth = tokenSources.size
    tokenSources.push(TokenListSource(tokens))
    processTokensUntilDepth(minDepth)

  /** Push a tokenizer onto the source stack (used by \include) */
  def pushTokenizer(tokenizer: Tokenizer): Unit =
    tokenSources.push(TokenizerSource(tokenizer))

  /** Process tokens until stack depth reaches minDepth */
  private def processTokensUntilDepth(minDepth: Int): Unit =
    while tokenSources.size > minDepth && hasMoreTokensAtDepth(minDepth) do
      val token = nextToken()
      token match
        case Token.Text(s, _)       => handler.text(s)
        case Token.Space(_, _)      => handler.space()
        case Token.Newline(_)       => handler.newline()
        case Token.BeginGroup(pos)  => handleBeginGroup(pos)
        case Token.EndGroup(pos)    => handleEndGroup(pos)
        case Token.ControlSeq(name, pos) => handleControlSeq(name, pos)
        case Token.Active(c, pos)   => handleActive(c, pos)
        case Token.EOF(_)           => // done

  /** Check if we have more tokens without popping below minDepth */
  private def hasMoreTokensAtDepth(minDepth: Int): Boolean =
    while tokenSources.size > minDepth && tokenSources.top.atEnd do
      tokenSources.pop()
    tokenSources.size > minDepth && !tokenSources.top.atEnd

  /** Set the result value (used by expression-producing primitives) */
  def setResult(v: Value): Unit = lastResult = v

  /** Get and clear the last result */
  def getResult: Value =
    val r = lastResult
    lastResult = Value.Nil
    r

  /** Evaluate an argument as an expression and return the result value.
    * This processes the tokens, capturing any result set by primitives.
    */
  def evalArgumentExpr(pos: CharReader): Value =
    evalTokensExpr(stripOuterBraces(readArgument(pos)), pos)

  /** Evaluate a list of tokens as an expression (without outputting to handler) */
  private def evalTokensExpr(tokens: Vector[Token], pos: CharReader): Value =
    tokens match
      // Empty
      case Vector() => Value.Nil

      // Simple variable reference
      case Vector(Token.ControlSeq(name, csPos)) =>
        handler.get(name) match
          case Value.Undefined =>
            // Try executing as primitive that sets a result
            primitives.get(name) match
              case Some(prim) =>
                lastResult = Value.Nil
                handler.suppressOutput(true)
                prim.execute(this, csPos)
                handler.suppressOutput(false)
                val r = getResult
                if r == Value.Nil then Value.Undefined else r
              case None => Value.Undefined
          case v => v

      // Dotted access: \var.field (field may have trailing chars)
      case Vector(Token.ControlSeq(name, _), Token.Text(dotField, _)) if dotField.startsWith(".") =>
        val rest = dotField.drop(1)
        val fieldEnd = rest.indexWhere(c => !c.isLetterOrDigit && c != '_')
        val field = if fieldEnd == -1 then rest else rest.substring(0, fieldEnd)
        handler.get(name) match
          case Value.Map(entries) => entries.getOrElse(field, Value.Undefined)
          case _ => Value.Undefined

      // Primitive followed by arguments (like \range{1}{5})
      case tokens if tokens.nonEmpty && tokens.head.isInstanceOf[Token.ControlSeq] =>
        val Token.ControlSeq(name, csPos) = tokens.head: @unchecked
        primitives.get(name) match
          case Some(prim) =>
            // Push remaining tokens as source, then execute primitive
            val rest = tokens.tail
            if rest.nonEmpty then tokenSources.push(TokenListSource(rest))
            lastResult = Value.Nil
            handler.suppressOutput(true)
            prim.execute(this, csPos)
            handler.suppressOutput(false)
            if rest.nonEmpty && tokenSources.nonEmpty && tokenSources.top.atEnd then tokenSources.pop()
            val r = getResult
            if r == Value.Nil then evalTokens(tokens, handler) else r
          case None =>
            // Check if it's a variable
            handler.get(name) match
              case Value.Undefined => evalTokens(tokens, handler)
              case v => v

      case _ =>
        // Simple tokens - just interpret as text/number
        evalTokens(tokens, handler)

  /** Read a control sequence name (for \let, etc.) */
  def readControlSeqName(pos: CharReader): String =
    skipSpaces()
    nextToken() match
      case Token.ControlSeq(name, _) => name
      case other => handler.error(s"Expected control sequence, got ${Token.show(other)}", pos)

  /** Read a simple identifier name (for \def, \set, etc.) */
  def readIdentifier(pos: CharReader): String =
    skipSpaces()
    nextToken() match
      case Token.Text(s, _) if s.nonEmpty && s.head.isLetter && s.forall(c => c.isLetterOrDigit || c == '_') => s
      case other => handler.error(s"Expected identifier, got ${Token.show(other)}", pos)

  /** Read tokens until \else or \fi at the current conditional level */
  def skipToElseOrFi(): Boolean =
    var depth = 1
    while depth > 0 && hasMoreTokens do
      nextToken() match
        case Token.ControlSeq("if" | "ifx", _) => depth += 1
        case Token.ControlSeq("else", _) if depth == 1 =>
          skipSpaces() // skip space after \else
          return true
        case Token.ControlSeq("fi", _) =>
          depth -= 1
          if depth == 0 then return false
        case _ => // skip
    false

  def skipToFi(): Unit =
    var depth = 1
    while depth > 0 && hasMoreTokens do
      nextToken() match
        case Token.ControlSeq("if" | "ifx", _) => depth += 1
        case Token.ControlSeq("fi", _)         => depth -= 1
        case _                                  => // skip

/** Token source abstraction - allows macro expansion to push tokens */
trait TokenSource:
  def peek: Token
  def next(): Token
  def atEnd: Boolean

class TokenizerSource(tokenizer: Tokenizer) extends TokenSource:
  def peek: Token = tokenizer.peek
  def next(): Token = tokenizer.next()
  def atEnd: Boolean = tokenizer.atEnd

class TokenListSource(tokens: Vector[Token]) extends TokenSource:
  private var index = 0
  def peek: Token = if index < tokens.size then tokens(index) else Token.EOF(null)
  def next(): Token =
    if index < tokens.size then
      val t = tokens(index)
      index += 1
      t
    else Token.EOF(null)
  def atEnd: Boolean = index >= tokens.size

/** Base trait for primitive commands */
trait Primitive:
  def execute(proc: Processor, pos: CharReader): Unit

/** Base trait for active character handlers.
  *
  * Active characters are special characters that trigger custom behavior when encountered in input.
  * Register active handlers with Processor.registerActive().
  */
trait Active:
  /** Called when the active character is encountered.
    *
    * @param proc The processor (provides access to handler, token reading, etc.)
    * @param c The active character that was encountered
    * @param pos The source position of the character
    */
  def execute(proc: Processor, c: Char, pos: CharReader): Unit

object DefPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name = proc.readIdentifier(pos)

    // Read named parameters (identifiers) until we hit the body brace
    val params = Vector.newBuilder[String]
    proc.skipSpaces()
    while proc.hasMoreTokens && (proc.peekToken() match
        case Token.BeginGroup(_) => false
        case Token.Text(s, _) if s.nonEmpty && s.head.isLetter => true
        case _ => false
      )
    do
      proc.nextToken() match
        case Token.Text(s, _) => params += s
        case _ => // shouldn't happen
      proc.skipSpaces()

    val body = proc.readArgument(pos)
    proc.handler.set(name, Value.Macro(params.result(), body, pos))

object GdefPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // Global def - for now same as def (TODO: implement global scope)
    DefPrimitive.execute(proc, pos)

object SetPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name = proc.readIdentifier(pos)
    // Evaluate the expression to get a value
    val value = proc.evalArgumentExpr(pos)
    proc.handler.set(name, value)

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

object IfxPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // \ifx compares two tokens for equality
    val tok1 = proc.nextToken()
    val tok2 = proc.nextToken()
    val equal = (tok1, tok2) match
      case (Token.ControlSeq(n1, _), Token.ControlSeq(n2, _)) => n1 == n2
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
    // \the outputs the value of a variable
    val name = proc.readControlSeqName(pos)
    val value = proc.handler.get(name)
    proc.handler.text(Value.display(value))

/** Strip the outer brace pair that readArgument preserves for scoping. Value
  * evaluation wants the inner content only.
  */
def stripOuterBraces(tokens: Vector[Token]): Vector[Token] =
  tokens match
    case Vector(Token.BeginGroup(_), rest*) if rest.nonEmpty && rest.last.isInstanceOf[Token.EndGroup] =>
      rest.init.toVector
    case other => other

/** Match a unit-suffixed dimension like 12pt, 0.5in, 3mm, 2pc, 1.5cm */
private val DimensionPattern = """([+-]?(?:\d+\.?\d*|\.\d+))(pt|pc|in|cm|mm)""".r

/** Parse a unit-suffixed dimension into Dimen (big points). Only context-free units — font-relative units (em, ex)
  * need the typesetter and are handled at the primitive level.
  */
def parseDimension(s: String): Option[Value] =
  s match
    case DimensionPattern(num, unit) =>
      val factor = unit match
        case "pt" => BigDecimal(1)
        case "pc" => BigDecimal(12)
        case "in" => BigDecimal(72)
        case "cm" => BigDecimal(72 / 2.54)
        case "mm" => BigDecimal(72 / 25.4)
      Some(Value.Dimen(BigDecimal(num) * factor))
    case _ => None

// Helper to evaluate tokens to a value
def evalTokens(tokens: Vector[Token], handler: Handler): Value =
  stripOuterBraces(tokens) match
    case Vector(Token.Text(s, _)) =>
      // Try to parse as a number, then as a unit-suffixed dimension
      try Value.Num(BigDecimal(s))
      catch case _: Exception => parseDimension(s).getOrElse(Value.Text(s))
    case Vector(Token.ControlSeq(name, _)) =>
      handler.get(name)
    case _ =>
      // Multiple tokens - concatenate as text including spaces
      val text = tokens.map {
        case Token.Text(s, _)  => s
        case Token.Space(s, _) => s
        case Token.Newline(_)  => "\n"
        case _                 => ""
      }.mkString
      if text.isEmpty then Value.Nil
      else Value.Text(text)

// ============ FOR LOOP ============

object ForPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // Syntax: \for\var{sequence-expr}{body}
    val varName = proc.readControlSeqName(pos)
    proc.skipSpaces()

    // Evaluate sequence expression
    val seqValue = proc.evalArgumentExpr(pos)

    // Read body
    val bodyTokens = proc.readArgument(pos)

    // Get items to iterate
    val items: Vector[Value] = seqValue match
      case Value.Seq(items) => items
      case Value.Text(s) => s.map(c => Value.Text(c.toString)).toVector
      case Value.Map(entries) => entries.map((k, v) => Value.Map(Map("key" -> Value.Text(k), "value" -> v))).toVector
      case Value.Nil | Value.Undefined => Vector.empty
      case _ => Vector(seqValue)

    // Iterate
    val length = items.size
    items.zipWithIndex.foreach { (item, idx) =>
      proc.handler.enterScope()

      // Set loop variable
      proc.handler.set(varName, item)

      // Set forloop metadata as a map (like texish-old)
      val forloop = Value.Map(Map(
        "index" -> Value.Num(idx + 1),
        "indexz" -> Value.Num(idx),
        "first" -> Value.Bool(idx == 0),
        "last" -> Value.Bool(idx == length - 1),
        "length" -> Value.Num(length),
        "rindex" -> Value.Num(length - idx),
        "rindexz" -> Value.Num(length - idx - 1),
        "element" -> item
      ))
      proc.handler.set("forloop", forloop)

      // Process body
      proc.processTokenList(bodyTokens)

      proc.handler.exitScope()
    }

object DonePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit = ()
    // End of for loop - marker only

// ============ FILE INCLUSION ============

object IncludePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // Read the file path argument
    val pathTokens = proc.readArgument(pos)
    val path = pathTokens.map {
      case Token.Text(s, _)  => s
      case Token.Space(s, _) => s
      case _                 => ""
    }.mkString.trim

    if path.isEmpty then
      proc.handler.error("\\include requires a file path", pos)

    // Read the file and push its tokens onto the source stack
    try
      val fileReader = CharReader.fromFile(path)
      val tokenizer = Tokenizer(fileReader)
      proc.pushTokenizer(tokenizer)
    catch
      case e: Exception =>
        proc.handler.error(s"Cannot include file '$path': ${e.getMessage}", pos)

// ============ ARITHMETIC ============

object AddPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = evalTokens(proc.readArgument(pos), proc.handler)
    val b = evalTokens(proc.readArgument(pos), proc.handler)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => Value.Num(x + y)
      case (Value.Text(x), Value.Text(y)) => Value.Text(x + y)
      case (Value.Seq(x), Value.Seq(y)) => Value.Seq(x ++ y)
      case _ => proc.handler.error(s"Cannot add ${Value.display(a)} and ${Value.display(b)}", pos)
    proc.setResult(result)
    proc.handler.text(Value.display(result))

object SubPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = evalTokens(proc.readArgument(pos), proc.handler)
    val b = evalTokens(proc.readArgument(pos), proc.handler)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => Value.Num(x - y)
      case _ => proc.handler.error(s"Cannot subtract ${Value.display(a)} and ${Value.display(b)}", pos)
    proc.setResult(result)
    proc.handler.text(Value.display(result))

object MulPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = evalTokens(proc.readArgument(pos), proc.handler)
    val b = evalTokens(proc.readArgument(pos), proc.handler)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => Value.Num(x * y)
      case _ => proc.handler.error(s"Cannot multiply ${Value.display(a)} and ${Value.display(b)}", pos)
    proc.setResult(result)
    proc.handler.text(Value.display(result))

object DivPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = evalTokens(proc.readArgument(pos), proc.handler)
    val b = evalTokens(proc.readArgument(pos), proc.handler)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) =>
        if y == 0 then proc.handler.error("Division by zero", pos)
        Value.Num(x / y)
      case _ => proc.handler.error(s"Cannot divide ${Value.display(a)} and ${Value.display(b)}", pos)
    proc.setResult(result)
    proc.handler.text(Value.display(result))

// ============ COMPARISON ============

object EqPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = evalTokens(proc.readArgument(pos), proc.handler)
    val b = evalTokens(proc.readArgument(pos), proc.handler)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => x == y
      case (Value.Text(x), Value.Text(y)) => x == y
      case (Value.Bool(x), Value.Bool(y)) => x == y
      case _ => false
    proc.setResult(Value.Bool(result))
    if result then proc.handler.text("true") else proc.handler.text("false")

object LtPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = evalTokens(proc.readArgument(pos), proc.handler)
    val b = evalTokens(proc.readArgument(pos), proc.handler)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => x < y
      case (Value.Text(x), Value.Text(y)) => x < y
      case _ => proc.handler.error(s"Cannot compare ${Value.display(a)} and ${Value.display(b)}", pos)
    if result then proc.handler.text("true") else proc.handler.text("false")

object GtPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = evalTokens(proc.readArgument(pos), proc.handler)
    val b = evalTokens(proc.readArgument(pos), proc.handler)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => x > y
      case (Value.Text(x), Value.Text(y)) => x > y
      case _ => proc.handler.error(s"Cannot compare ${Value.display(a)} and ${Value.display(b)}", pos)
    if result then proc.handler.text("true") else proc.handler.text("false")

object LePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = evalTokens(proc.readArgument(pos), proc.handler)
    val b = evalTokens(proc.readArgument(pos), proc.handler)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => x <= y
      case (Value.Text(x), Value.Text(y)) => x <= y
      case _ => proc.handler.error(s"Cannot compare ${Value.display(a)} and ${Value.display(b)}", pos)
    if result then proc.handler.text("true") else proc.handler.text("false")

object GePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = evalTokens(proc.readArgument(pos), proc.handler)
    val b = evalTokens(proc.readArgument(pos), proc.handler)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => x >= y
      case (Value.Text(x), Value.Text(y)) => x >= y
      case _ => proc.handler.error(s"Cannot compare ${Value.display(a)} and ${Value.display(b)}", pos)
    if result then proc.handler.text("true") else proc.handler.text("false")

object NePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = evalTokens(proc.readArgument(pos), proc.handler)
    val b = evalTokens(proc.readArgument(pos), proc.handler)
    val result = (a, b) match
      case (Value.Num(x), Value.Num(y)) => x != y
      case (Value.Text(x), Value.Text(y)) => x != y
      case (Value.Bool(x), Value.Bool(y)) => x != y
      case _ => true
    if result then proc.handler.text("true") else proc.handler.text("false")

// ============ STRING FUNCTIONS ============

object UpcasePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = evalTokens(proc.readArgument(pos), proc.handler)
    proc.handler.text(Value.display(arg).toUpperCase)

object DowncasePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = evalTokens(proc.readArgument(pos), proc.handler)
    proc.handler.text(Value.display(arg).toLowerCase)

object TrimPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = evalTokens(proc.readArgument(pos), proc.handler)
    proc.handler.text(Value.display(arg).trim)

object SizePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalArgumentExpr(pos)
    val size = arg match
      case Value.Text(s) => s.length
      case Value.Seq(items) => items.size
      case Value.Map(entries) => entries.size
      case _ => 0
    proc.handler.text(size.toString)

// ============ SEQUENCE FUNCTIONS ============

object SeqPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // Read items from brace group - space separated
    val items = Vector.newBuilder[Value]
    val groupTokens = stripOuterBraces(proc.readArgument(pos))
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
    val arg = evalTokens(proc.readArgument(pos), proc.handler)
    val result = arg match
      case Value.Seq(items) if items.nonEmpty => items.head
      case Value.Text(s) if s.nonEmpty => Value.Text(s.head.toString)
      case _ => Value.Nil
    proc.handler.text(Value.display(result))

object TailPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = evalTokens(proc.readArgument(pos), proc.handler)
    val result = arg match
      case Value.Seq(items) if items.nonEmpty => Value.Seq(items.tail)
      case Value.Text(s) if s.nonEmpty => Value.Text(s.tail)
      case _ => Value.Nil
    proc.handler.set("seq", result)

object LastPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = evalTokens(proc.readArgument(pos), proc.handler)
    val result = arg match
      case Value.Seq(items) if items.nonEmpty => items.last
      case Value.Text(s) if s.nonEmpty => Value.Text(s.last.toString)
      case _ => Value.Nil
    proc.handler.text(Value.display(result))

// ============ MAP/OBJECT CREATION ============

object MapPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // Read key-value pairs from brace group - space separated
    // Syntax: \map{key1 value1 key2 value2}
    val items = Vector.newBuilder[Value]
    val groupTokens = proc.readArgument(pos)
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

// ============ ESCAPE SEQUENCES ============

class LiteralPrimitive(literal: String) extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    proc.handler.text(literal)
