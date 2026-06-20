package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.path.Path
import io.github.edadma.texish.EmbeddedPackages
import scala.collection.mutable

/** The streaming processor/expander for parser.
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

  // Names of the environments currently open, innermost last, so \end can verify it matches \begin
  private[parser] val envStack = mutable.Stack[String]()

  // The directory of the file currently being processed, innermost last. The bottom is the top-level document's
  // directory (set by the host via setBaseDir); each \use pushes the loaded module's directory while it runs, so a
  // module that itself does a relative \use resolves it against the module's own location, not the document's.
  private val dirStack = mutable.Stack[String]()

  // Canonical (absolute, normalized) paths of every module already loaded by \use, so a second \use of the same
  // file is a no-op — dependency diamonds load once.
  private val loadedModules = mutable.Set[String]()

  // Register default primitives
  registerPrimitive("def", DefPrimitive)
  registerPrimitive("gdef", GdefPrimitive)
  registerPrimitive("global", GlobalPrimitive)
  registerPrimitive("let", LetPrimitive)
  registerPrimitive("set", SetPrimitive)
  registerPrimitive("if", IfPrimitive)
  registerPrimitive("ifx", IfxPrimitive)
  registerPrimitive("else", ElsePrimitive)
  registerPrimitive("fi", FiPrimitive)
  registerPrimitive("the", ThePrimitive)

  // Loop primitives
  registerPrimitive("for", ForPrimitive)
  registerPrimitive("done", DonePrimitive)

  // File inclusion (raw input) and module import (load-once, no typesetting)
  registerPrimitive("include", IncludePrimitive)
  registerPrimitive("use", UsePrimitive)

  // Arithmetic primitives
  registerPrimitive("+", AddPrimitive)
  registerPrimitive("-", SubPrimitive)
  registerPrimitive("*", MulPrimitive)
  registerPrimitive("/", DivPrimitive)
  registerPrimitive("calc", CalcPrimitive)
  registerPrimitive("round", RoundPrimitive)

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
  registerPrimitive("accent", AccentPrimitive)

  // Sequence primitives
  registerPrimitive("seq", SeqPrimitive)
  registerPrimitive("range", RangePrimitive)
  registerPrimitive("cat", CatPrimitive)
  registerPrimitive("head", HeadPrimitive)
  registerPrimitive("tail", TailPrimitive)
  registerPrimitive("last", LastPrimitive)

  // Map/object primitive
  registerPrimitive("map", MapPrimitive)
  registerPrimitive("mapset", MapSetPrimitive)
  registerPrimitive("mapget", MapGetPrimitive)
  registerPrimitive("maphas", MapHasPrimitive)

  // Token-stream control
  registerPrimitive("ignorespaces", IgnoreSpacesPrimitive)

  // Hooks: register deferred code under a name, run it later
  registerPrimitive("addtohook", AddToHookPrimitive)
  registerPrimitive("usehook", UseHookPrimitive)

  // Environments: \begin{name} … \end{name} runs the env's begin/end code around a scoped body
  registerPrimitive("newenvironment", NewEnvironmentPrimitive)
  registerPrimitive("begin", BeginPrimitive)
  registerPrimitive("end", EndPrimitive)

  // Number-formatting primitives (section/list/footnote labels)
  registerPrimitive("arabic", ArabicPrimitive)
  registerPrimitive("roman", RomanPrimitive)
  registerPrimitive("Roman", RomanUpPrimitive)
  registerPrimitive("alph", AlphPrimitive)
  registerPrimitive("Alph", AlphUpPrimitive)
  registerPrimitive("fnsymbol", FnSymbolPrimitive)

  // Escape sequences for special characters
  registerPrimitive("{", LiteralPrimitive("{"))
  registerPrimitive("}", LiteralPrimitive("}"))
  registerPrimitive("%", LiteralPrimitive("%"))
  registerPrimitive("\\", LiteralPrimitive("\\"))
  registerPrimitive("~", LiteralPrimitive("~"))
  // & and # are alignment-active only inside \halign; \& and \# are their literals everywhere
  registerPrimitive("&", LiteralPrimitive("&"))
  registerPrimitive("#", LiteralPrimitive("#"))
  // $ is the active math toggle; \$ is a literal dollar sign (for prose, prices, code samples)
  registerPrimitive("$", LiteralPrimitive("$"))

  def registerPrimitive(name: String, prim: Primitive): Unit =
    primitives(name) = prim

  /** The primitive bound to `name`, if any — used by `\let` to alias a built-in. */
  def lookupPrimitive(name: String): Option[Primitive] = primitives.get(name)

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
    while hasMoreTokens do dispatch(nextToken())

  /** Dispatch one token to the handler, attaching the token's source position to any error the host raises while
    * handling it. Errors from the language layer already carry a position and pass through unchanged.
    */
  private def dispatch(token: Token): Unit =
    try
      token match
        case Token.Text(s, _)            => handler.text(s)
        case Token.Space(_, _)           => handler.space()
        case Token.Newline(_)            => handler.newline()
        case Token.BeginGroup(pos)       => handleBeginGroup(pos)
        case Token.EndGroup(pos)         => handleEndGroup(pos)
        case Token.ControlSeq(name, pos) => handleControlSeq(name, pos)
        case Token.Active(c, pos)        => handleActive(c, pos)
        case Token.EOF(_)                => // done
    catch
      case e: ParserException => throw e
      case e: RuntimeException =>
        handler.error(Option(e.getMessage).getOrElse(e.toString), Token.pos(token))

  private def handleBeginGroup(pos: CharReader): Unit =
    handler.enterScope()

  private def handleEndGroup(pos: CharReader): Unit =
    handler.exitScope()

  private def handleControlSeq(name: String, pos: CharReader): Unit =
    // A user-defined macro (\def) overrides a built-in primitive of the same name, as in TeX — so e.g. a
    // document can redefine \hbox or any other built-in. Only an explicit macro overrides; ordinary
    // variables of the same name do not, so they can't accidentally shadow a primitive.
    val defined = handler.get(name)

    defined match
      case Value.Macro(params, body, _) =>
        expandMacro(name, params, body, pos)
      case _ if primitives.contains(name) =>
        primitives(name).execute(this, pos)
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

  private def expandMacro(name: String, params: Vector[MacroParam], body: Vector[Token], pos: CharReader): Unit =
    // Read arguments by name, then substitute control sequences matching param names in the body, and push the
    // expanded tokens as a new source.
    val expandedBody = substituteNamedParams(body, readMacroArgs(params, pos))

    tokenSources.push(TokenListSource(expandedBody))

  /** Read a macro or environment's arguments in declaration order, returning a name → tokens map ready for
    * [[substituteNamedParams]]. A mandatory parameter reads the next braced group or token; an optional parameter
    * reads a following `[…]` if one is present and otherwise expands to its declared default — so a missing optional
    * never consumes input.
    */
  def readMacroArgs(params: Vector[MacroParam], pos: CharReader): Map[String, Vector[Token]] =
    params.map { p =>
      val tokens = p.kind match
        case ParamKind.Mandatory       => readArgument(pos)
        case ParamKind.Optional(deflt) => readOptionalArg(pos).getOrElse(deflt)
        case ParamKind.Star            => readStarFlag(pos)
      p.name -> tokens
    }.toMap

  /** Read an optional `[…]` argument if one follows, returning its tokens (the content between the brackets) or
    * None when the next token is not an opening `[`. Brackets are ordinary text, so `[` may begin a text run and the
    * closing `]` may sit mid-token; this scans across tokens, splitting text at the brackets and pushing back any
    * tail after `]`. A `]` inside a braced group does not close the argument, so `[{a]b}]` reads `{a]b}`.
    */
  def readOptionalArg(pos: CharReader): Option[Vector[Token]] =
    skipSpaces()
    peekToken() match
      case Token.Text(s, sp) if s.startsWith("[") =>
        nextToken()
        val out    = Vector.newBuilder[Token]
        var depth  = 0
        var closed = false

        def takeText(str: String, p: CharReader): Unit =
          if depth > 0 then out += Token.Text(str, p)
          else
            val idx = str.indexOf(']')
            if idx < 0 then { if str.nonEmpty then out += Token.Text(str, p) }
            else
              val before = str.substring(0, idx)
              val after  = str.substring(idx + 1)
              if before.nonEmpty then out += Token.Text(before, p)
              if after.nonEmpty then pushBack(Vector(Token.Text(after, p)))
              closed = true

        takeText(s.substring(1), sp)
        while !closed && hasMoreTokens do
          nextToken() match
            case Token.Text(str, p)      => takeText(str, p)
            case t @ Token.BeginGroup(_) => depth += 1; out += t
            case t @ Token.EndGroup(_)   => depth -= 1; out += t
            case Token.EOF(_)            => closed = true
            case other                   => out += other
        Some(out.result())
      case _ => None

  /** Read a `\section*`-style star flag: consume a leading `*` if one follows and return `1`, otherwise consume
    * nothing and return `0`. The `*` is ordinary text, so it may sit at the head of a longer text token; the tail
    * after it is pushed back to be read as the next argument. */
  def readStarFlag(pos: CharReader): Vector[Token] =
    skipSpaces()
    peekToken() match
      case Token.Text(s, sp) if s.startsWith("*") =>
        nextToken()
        val rest = s.substring(1)
        if rest.nonEmpty then pushBack(Vector(Token.Text(rest, sp)))
        Vector(Token.Text("1", sp))
      case _ => Vector(Token.Text("0", pos))

  /** Read a brace-delimited argument verbatim — the literal characters, with no comment, escape, active-char or
    * macro processing. For URL-like arguments that must survive `//` (otherwise a comment) and other specials.
    * Works only over live top-level input; inside a macro expansion the text was already tokenized, so this
    * errors rather than returning a corrupted string. */
  def readRawArgument(pos: CharReader): String =
    skipSpaces()
    if !hasMoreTokens then handler.error("Unexpected end of input while reading a verbatim argument", pos)
    tokenSources.top.readRawGroup() match
      case Some(s) => s
      case None =>
        handler.error("expected a verbatim {…} argument (a URL must be given directly, not through a macro)", pos)

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
        skipSpaces() // consume trailing whitespace after non-braced argument
        tok

  /** Read a single math script field, the argument of a `^` or `_`: a braced group is the whole group; a
    * control sequence is that one token; a run of text contributes only its first character, the rest pushed
    * back to be read normally — so `x^2y` makes `2` the script and `y` a following atom, as in TeX. */
  def readScriptField(pos: CharReader): Vector[Token] =
    skipSpaces()
    if !hasMoreTokens then handler.error("Expected a superscript or subscript", pos)

    peekToken() match
      case Token.BeginGroup(_) => readArgument(pos)
      case Token.Text(s, p) =>
        nextToken()
        if s.length > 1 then tokenSources.push(TokenListSource(Vector(Token.Text(s.substring(1), p))))
        Vector(Token.Text(s.substring(0, 1), p))
      case _ => Vector(nextToken())

  /** Push tokens back onto the source stack so the next reads see them first — used after peeking past a
    * delimiter to return the unconsumed remainder of a text run to the stream. */
  def pushBack(tokens: Vector[Token]): Unit =
    if tokens.nonEmpty then tokenSources.push(TokenListSource(tokens))

  /** Collect the tokens of a `\left…\right` body: everything up to the matching `\right`, with nested
    * `\left`/`\right` pairs balanced and passed through verbatim (they are re-processed when the body is laid
    * out). The matching `\right` is consumed, so its delimiter is read next; an end of input before it is an
    * error. */
  def collectDelimitedBody(pos: CharReader): Vector[Token] =
    val tokens = Vector.newBuilder[Token]
    var depth  = 1

    while depth > 0 && hasMoreTokens do
      nextToken() match
        case t @ Token.ControlSeq("left", _)  => depth += 1; tokens += t
        case t @ Token.ControlSeq("right", _) => depth -= 1; if depth > 0 then tokens += t
        case Token.EOF(p)                     => handler.error("\\left without matching \\right", p)
        case t                                => tokens += t

    if depth > 0 then handler.error("\\left without matching \\right", pos)
    tokens.result()

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

  private[parser] def substituteNamedParams(body: Vector[Token], params: Map[String, Vector[Token]]): Vector[Token] =
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

  /** TeX-style glue continuation: after a dimension argument, consume optional `plus <flex>` and `minus <flex>`
    * keywords from the token stream (`\vskip 12pt plus 2pt minus 1fil`). Returns Glue if either keyword was present,
    * otherwise Dimen of the natural size alone. Spaces skipped while looking for a keyword that isn't there are
    * pushed back so surrounding text is unaffected.
    */
  def readGlueContinuation(natural: Double, pos: CharReader): Value =
    def keyword(kw: String): Boolean =
      val skipped = Vector.newBuilder[Token]
      while hasMoreTokens && peekToken().isInstanceOf[Token.Space] do skipped += nextToken()
      peekToken() match
        case Token.Text(s, _) if s == kw =>
          nextToken()
          true
        case _ =>
          val toRestore = skipped.result()
          if toRestore.nonEmpty then tokenSources.push(TokenListSource(toRestore))
          false

    def flexAmount(kw: String): (Double, Int) =
      skipSpaces()
      nextToken() match
        case Token.Text(s, p) =>
          parseFlex(s, handler.fontUnit)
            .getOrElse(handler.error(s"expected a dimension or fil/fill amount after '$kw', got '$s'", p))
        case _ =>
          handler.error(s"expected a dimension or fil/fill amount after '$kw'", pos)

    val hasPlus                 = keyword("plus")
    val (stretch, stretchOrder) = if hasPlus then flexAmount("plus") else (0.0, 0)
    val hasMinus                = keyword("minus")
    val (shrink, shrinkOrder)   = if hasMinus then flexAmount("minus") else (0.0, 0)

    if hasPlus || hasMinus then Value.Glue(natural, stretch, shrink, stretchOrder, shrinkOrder)
    else Value.Dimen(natural)

  /** Parse a simple value from a string (number, unit-suffixed dimension, or text) */
  private def parseSimpleValue(s: String): Value =
    try Value.Num(s.toDouble)
    catch case _: Exception => parseDimension(s, handler.fontUnit).getOrElse(Value.Text(s))

  /** Process a list of tokens (used by primitives like \for) */
  def processTokenList(tokens: Vector[Token]): Unit =
    val minDepth = tokenSources.size
    tokenSources.push(TokenListSource(tokens))
    processTokensUntilDepth(minDepth)

  /** Push a tokenizer onto the source stack (used by \include) */
  def pushTokenizer(tokenizer: Tokenizer): Unit =
    tokenSources.push(TokenizerSource(tokenizer))

  /** The directory of the file currently being processed — the innermost active `\use` module, or the top-level
    * document's directory, or "." if the host set no base. Relative `\use` resolution starts here. */
  def currentDir: String = if dirStack.nonEmpty then dirStack.top else "."

  /** Set the top-level document's directory, so `\use` can resolve packages relative to the document being run.
    * The host (a CLI or GUI) calls this once before processing; default with no call is the current directory. */
  def setBaseDir(dir: String): Unit =
    dirStack.clear()
    dirStack.push(dir)

  /** Record that the module at this canonical (absolute, normalized) path is being loaded. Returns true the first
    * time the path is seen and false afterwards, so `\use` loads each module exactly once. */
  def claimModule(canonical: String): Boolean = loadedModules.add(canonical)

  /** Load a module's source as part of the current document without typesetting it: output is suppressed for the
    * duration, so the file's prose, blank lines and comments emit nothing and only its definitions take effect.
    * The module's own directory is pushed while it runs (for nested relative `\use`), and only the module's tokens
    * are drained — control returns to the enclosing document at the same stack depth. Suppression and the directory
    * stack are restored even if loading raises. */
  def loadModule(content: String, dir: String): Unit =
    val saved    = handler.outputSuppressed
    val minDepth = tokenSources.size
    dirStack.push(dir)
    tokenSources.push(TokenizerSource(Tokenizer(content, activeChars)))
    handler.suppressOutput(true)
    try processTokensUntilDepth(minDepth)
    finally
      handler.suppressOutput(saved)
      dirStack.pop()

  /** Process tokens until stack depth reaches minDepth */
  private def processTokensUntilDepth(minDepth: Int): Unit =
    while tokenSources.size > minDepth && hasMoreTokensAtDepth(minDepth) do dispatch(nextToken())

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

  /** Evaluate an already-extracted token run as an expression — including a primitive applied to its braced
    * arguments (`\*{a}{b}`) and dotted access (`\forloop.index`), which the plain [[evalTokens]] does not. For
    * callers that have split a compound argument into pieces themselves, such as the space-separated coordinates
    * of a picture shape. */
  def evalExpr(tokens: Vector[Token], pos: CharReader): Value =
    evalTokensExpr(tokens, pos)

  /** True when the tokens are a single balanced group wrapping everything — `{ … }` with the opening brace closed
    * only by the final token. (Distinguishes a genuine wrapper from `{a}{b}`, whose first brace closes early.) */
  private def isWrappingGroup(tokens: Vector[Token]): Boolean =
    tokens.length >= 2 && tokens.head.isInstanceOf[Token.BeginGroup] && {
      var depth = 0
      var closedEarly = false
      for (t, i) <- tokens.zipWithIndex do
        t match
          case _: Token.BeginGroup => depth += 1
          case _: Token.EndGroup   => depth -= 1; if depth == 0 && i != tokens.length - 1 then closedEarly = true
          case _                   =>
      depth == 0 && !closedEarly && tokens.last.isInstanceOf[Token.EndGroup]
    }

  /** Evaluate a list of tokens as an expression (without outputting to handler) */
  private def evalTokensExpr(tokens: Vector[Token], pos: CharReader): Value =
    tokens match
      // Empty
      case Vector() => Value.Nil

      // A single wrapping group is the parenthesised expression — evaluate its contents. This makes `{\e.key}`
      // and `{\mapget m {k}}` evaluate as values even when an extra brace layer is introduced by macro parameter
      // substitution (a braced argument carries its braces, so `{\param}` in a body becomes `{{arg}}`).
      case ts if isWrappingGroup(ts) => evalTokensExpr(stripOuterBraces(ts), pos)

      // Simple variable reference
      case Vector(Token.ControlSeq(name, csPos)) =>
        handler.get(name) match
          // A macro overrides a primitive of the same name (as in document position); expand it and evaluate
          // its body as an expression, so a macro that yields a value (e.g. \value over \mapget) composes here.
          case Value.Macro(params, body, _) => evalMacroExpr(params, body, Vector.empty, csPos)
          case Value.Undefined =>
            // Try executing as primitive that sets a result
            primitives.get(name) match
              case Some(prim) =>
                lastResult = Value.Nil
                val savedSuppress = handler.outputSuppressed
                handler.suppressOutput(true)
                prim.execute(this, csPos)
                handler.suppressOutput(savedSuppress)
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

      // A control sequence applied to following arguments — a macro (\value{c}) or a primitive (\range{1}{5}).
      case tokens if tokens.nonEmpty && tokens.head.isInstanceOf[Token.ControlSeq] =>
        val Token.ControlSeq(name, csPos) = tokens.head: @unchecked
        handler.get(name) match
          // A macro overrides a same-named primitive: read its arguments from the remaining tokens, expand, and
          // evaluate the body as an expression so a value-yielding macro composes inside other expressions.
          case Value.Macro(params, body, _) => evalMacroExpr(params, body, tokens.tail, csPos)
          case _ =>
            primitives.get(name) match
              case Some(prim) =>
                // Push remaining tokens as a source for the primitive to read its arguments from, then clean it up.
                // Track the pushed source by identity: reading an unbraced argument ends with a skipSpaces that can
                // exhaust and pop this source already, so popping whatever is now on top would over-pop into the
                // enclosing source. Only pop our own source, and only if the primitive left it on top unexhausted.
                val rest       = tokens.tail
                val restSource = if rest.nonEmpty then Some(TokenListSource(rest)) else None
                restSource.foreach(tokenSources.push)
                lastResult = Value.Nil
                val savedSuppress = handler.outputSuppressed
                handler.suppressOutput(true)
                prim.execute(this, csPos)
                handler.suppressOutput(savedSuppress)
                restSource.foreach(s => if tokenSources.nonEmpty && (tokenSources.top eq s) then tokenSources.pop())
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

  /** Expand a macro in expression position: push the remaining tokens so the macro can read its arguments, read
    * them, substitute into the body, then evaluate the expanded body as an expression. The pushed source is
    * tracked by identity and only popped if argument reading left it on top (see the primitive case). */
  private def evalMacroExpr(params: Vector[MacroParam], body: Vector[Token], rest: Vector[Token], pos: CharReader): Value =
    val restSource = if rest.nonEmpty then Some(TokenListSource(rest)) else None
    restSource.foreach(tokenSources.push)
    val args = readMacroArgs(params, pos)
    restSource.foreach(s => if tokenSources.nonEmpty && (tokenSources.top eq s) then tokenSources.pop())
    evalTokensExpr(substituteNamedParams(body, args), pos)

  /** Read a control sequence name (for \let, etc.) */
  def readControlSeqName(pos: CharReader): String =
    skipSpaces()
    nextToken() match
      case Token.ControlSeq(name, _) => name
      case other => handler.error(s"Expected control sequence, got ${Token.show(other)}", pos)

  /** Read a simple identifier name (for \def, \set, etc.). A bare word names it; a control
    * sequence names it by its control-sequence name, so the original TeX form `\def\TeX{…}` works
    * as well as `\def TeX{…}`. */
  def readIdentifier(pos: CharReader): String =
    skipSpaces()
    nextToken() match
      case Token.Text(s, _) if s.nonEmpty && s.head.isLetter && s.forall(c => c.isLetterOrDigit || c == '_') => s
      case Token.ControlSeq(name, _) => name
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
  // Verbatim read of a brace group, available only over live (untokenized) input — see Tokenizer.readRawGroup.
  // A pre-tokenized list cannot offer it: its text was already tokenized, so any // is long gone.
  def readRawGroup(): Option[String] = None

class TokenizerSource(tokenizer: Tokenizer) extends TokenSource:
  def peek: Token = tokenizer.peek
  def next(): Token = tokenizer.next()
  def atEnd: Boolean = tokenizer.atEnd
  override def readRawGroup(): Option[String] = tokenizer.readRawGroup()

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

      case Token.Text(s, _) if s.nonEmpty && s.head.isLetter =>
        proc.nextToken()
        out += MacroParam(s, ParamKind.Mandatory)
        proc.skipSpaces()

      case _ => done = true

  out.result()

object DefPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name = proc.readIdentifier(pos)
    // capture and clear the \global flag up front, so evaluating params/body can't be confused by it
    val global = proc.handler.globalAssign
    proc.handler.globalAssign = false

    // Read the parameter list (mandatory names and optional [name:default] specs) up to the body brace.
    val params = readMacroParams(proc, pos)

    // Strip the braces that delimit the body in the \def syntax: a macro is pure token substitution and must not
    // open a scope of its own, so a \set or \coordinate in its body lands in the caller's scope, as in TeX.
    // Grouping inside a macro is whatever explicit { } the body itself contains.
    val body = stripOuterBraces(proc.readArgument(pos))
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
    val name   = proc.readIdentifier(pos)
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
    val name = proc.readIdentifier(pos)
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
      // Multiple tokens - concatenate as text including spaces
      val text = tokens.map {
        case Token.Text(s, _)  => s
        case Token.Space(s, _) => s
        case Token.Newline(_)  => "\n"
        case _                 => ""
      }.mkString
      if text.isEmpty then Value.Nil
      else parseGlue(text, handler.fontUnit).getOrElse(Value.Text(text)) // a braced glue spec like {12pt plus 2pt} arrives here

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

      // Set forloop metadata as a map
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

/** `\use{name}` — import a texish module: locate `name.texish`, load it once with output suppressed (so the file's
  * prose and blank lines emit nothing — only its definitions take effect), and skip a repeat `\use` of the same
  * file. This is the module loader; `\include` remains the raw input that re-reads and typesets a literal path.
  *
  * Resolution searches, in order: the directory of the file doing the `\use`, the current directory, the `packages`
  * folder under `$TEXISHHOME` if that environment variable is set, and a `./packages/` folder under the current
  * directory. The first existing file wins, so a local module shadows a standard one. The search roots are an
  * ordered list, so more roots can be added later without changing the ones already in effect.
  */
object UsePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val nameTokens = proc.readArgument(pos)
    val name = nameTokens.map {
      case Token.Text(s, _)  => s
      case Token.Space(s, _) => s
      case _                 => ""
    }.mkString.trim

    if name.isEmpty then proc.handler.error("\\use requires a module name", pos)

    val fileName = if name.endsWith(".texish") then name else s"$name.texish"

    // Read TEXISHHOME from the live environment (see PlatformEnv — `System.getenv`/`sys.env` snapshot
    // the environment on Native, so a host that sets the variable after start would not be seen).
    val texishHome = PlatformEnv.get("TEXISHHOME").filter(_.nonEmpty)

    // Search the filesystem first, but tolerate a host that has no working filesystem at all — a browser, where
    // the path layer reaches for Node APIs that are absent. If probing the filesystem throws, treat it as "no
    // file found" and fall through to the embedded copy below.
    val onDisk: Option[Path] =
      try
        val roots: List[Path] =
          List(
            Some(Path(proc.currentDir)),
            Some(Path(".")),
            texishHome.map(h => Path(h) / "packages"),
            Some(Path(".") / "packages"),
          ).flatten
        roots.map(_ / fileName).find(p => p.exists && p.isFile)
      catch case _: Throwable => None

    onDisk match
      case Some(file) =>
        val resolved  = file.toAbsolutePath.normalize
        val canonical = resolved.toPlatformString
        if proc.claimModule(canonical) then
          val dir = resolved.parent.map(_.toPlatformString).getOrElse(".")
          try proc.loadModule(file.readText(), dir)
          catch
            case e: ParserException => throw e
            case e: Exception       => proc.handler.error(s"\\use: cannot load module '$name': ${e.getMessage}", pos)
      case None =>
        // No file on disk: fall back to a module embedded in the build. This is how a host with no package
        // directory — chiefly the browser — resolves the standard modules; where the files are present the
        // search above wins first, so a local package still shadows the embedded copy.
        EmbeddedPackages.sources.get(name.stripSuffix(".texish")) match
          case Some(chunks) =>
            if proc.claimModule(s"embedded:${name.stripSuffix(".texish")}") then proc.loadModule(chunks.mkString, ".")
          case None =>
            proc.handler.error(s"\\use: module '$name' not found on the filesystem or among the embedded modules", pos)

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

// ============ STRING FUNCTIONS ============

object AccentPrimitive extends Primitive:
  private val accents =
    Map(
      ("'", "a") -> "á", ("'", "A") -> "Á",
      ("'", "e") -> "é", ("'", "E") -> "É",
      ("'", "i") -> "í", ("'", "I") -> "Í",
      ("'", "o") -> "ó", ("'", "O") -> "Ó",
      ("'", "u") -> "ú", ("'", "U") -> "Ú",
      ("'", "y") -> "ý", ("'", "Y") -> "Ý",
      ("`", "a") -> "à", ("`", "A") -> "À",
      ("`", "e") -> "è", ("`", "E") -> "È",
      ("`", "i") -> "ì", ("`", "I") -> "Ì",
      ("`", "o") -> "ò", ("`", "O") -> "Ò",
      ("`", "u") -> "ù", ("`", "U") -> "Ù",
      ("^", "a") -> "â", ("^", "A") -> "Â",
      ("^", "e") -> "ê", ("^", "E") -> "Ê",
      ("^", "i") -> "î", ("^", "I") -> "Î",
      ("^", "o") -> "ô", ("^", "O") -> "Ô",
      ("^", "u") -> "û", ("^", "U") -> "Û",
      ("\"", "a") -> "ä", ("\"", "A") -> "Ä",
      ("\"", "e") -> "ë", ("\"", "E") -> "Ë",
      ("\"", "i") -> "ï", ("\"", "I") -> "Ï",
      ("\"", "o") -> "ö", ("\"", "O") -> "Ö",
      ("\"", "u") -> "ü", ("\"", "U") -> "Ü",
      ("\"", "y") -> "ÿ", ("\"", "Y") -> "Ÿ",
      ("~", "a") -> "ã", ("~", "A") -> "Ã",
      ("~", "n") -> "ñ", ("~", "N") -> "Ñ",
      ("~", "o") -> "õ", ("~", "O") -> "Õ",
      ("c", "c") -> "ç", ("c", "C") -> "Ç",
    )

  // the accent mark may arrive wrapped in literal quotes (\accent "'" e or \accent '"' e)
  private def unquote(s: String): String =
    if s.length >= 3 && (s.head == '"' && s.last == '"' || s.head == '\'' && s.last == '\'') then s.tail.init
    else s

  // read an argument as literal text: active chars like ~ keep their literal character (\accent {~} n)
  private def literalArg(proc: Processor, pos: CharReader): String =
    stripOuterBraces(proc.readArgument(pos)).map {
      case Token.Text(s, _)   => s
      case Token.Active(c, _) => c.toString
      case Token.Space(s, _)  => s
      case _                  => ""
    }.mkString

  def execute(proc: Processor, pos: CharReader): Unit =
    val accent = unquote(literalArg(proc, pos))
    val base   = literalArg(proc, pos)

    accents get (accent, base) match
      case Some(c) =>
        proc.setResult(Value.Text(c))
        proc.handler.text(c)
      case None => proc.handler.error(s"accented character not found: $accent $base", pos)

object UpcasePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalArgumentExpr(pos)
    proc.handler.text(Value.display(arg).toUpperCase)

object DowncasePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalArgumentExpr(pos)
    proc.handler.text(Value.display(arg).toLowerCase)

object TrimPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalArgumentExpr(pos)
    proc.handler.text(Value.display(arg).trim)

object SizePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalArgumentExpr(pos)
    val size = arg match
      case Value.Text(s) => s.length
      case Value.Seq(items) => items.size
      case Value.Map(entries) => entries.size
      case _ => 0
    // value (for \if/\set, where output is suppressed) and typeset output (in direct position)
    proc.setResult(Value.Num(size))
    proc.handler.text(size.toString)

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

/** \cat{a}{b} — concatenate two values as text and return the result, for building up strings in the document
  * language (e.g. accumulating an element run while parsing a formula). A general string operation. */
object CatPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = Value.display(proc.evalArgumentExpr(pos))
    val b = Value.display(proc.evalArgumentExpr(pos))
    proc.setResult(Value.Text(a + b))

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

// ============ HOOKS ============

/** All hooks live in one global map variable `hookStore`: hook name → a `Seq` of code fragments, each a
  * parameterless `Macro` holding the *unevaluated* tokens registered for it. The list preserves registration
  * order so `\usehook` runs the fragments in the order they were added. */
private val HookStoreName = "hookStore"

private def hookFragments(proc: Processor, name: String): Vector[Value] =
  proc.handler.get(HookStoreName) match
    case Value.Map(m) =>
      m.get(name) match
        case Some(Value.Seq(items)) => items
        case _                      => Vector.empty
    case _ => Vector.empty

/** `\addtohook name {code}` — append a fragment of (unevaluated) code to the named hook, to be run later by
  * `\usehook`. Hooks are stored globally so a package can register set-up or clean-up code that the document — or
  * the host driver, for lifecycle hooks like `begindocument`/`enddocument` — fires at the right moment. The code
  * is kept verbatim and only executed when the hook runs, which is the whole point of deferring it. */
object AddToHookPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    proc.handler.globalAssign = false // hooks are always global; ignore any stray prefix
    val name = proc.readIdentifier(pos)
    val code = stripOuterBraces(proc.readArgument(pos))
    val fragment = Value.Macro(Vector.empty, code, pos)
    val updated = proc.handler.get(HookStoreName) match
      case Value.Map(m) => Value.Map(m + (name -> Value.Seq(hookFragments(proc, name) :+ fragment)))
      case _            => Value.Map(Map(name -> Value.Seq(Vector(fragment))))
    proc.handler.setGlobal(HookStoreName, updated)

/** `\usehook name` — run every fragment registered for the named hook, in registration order, at the current
  * point in the document. An empty or never-registered hook is a silent no-op (hooks are routinely empty). */
object UseHookPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name   = proc.readIdentifier(pos)
    val tokens = hookFragments(proc, name).flatMap {
      case Value.Macro(_, body, _) => body
      case _                       => Vector.empty
    }
    proc.pushBack(tokens)

// ============ ENVIRONMENTS ============

/** Every environment is stored in one global map variable `envStore`: environment name → a two-element `Seq`
  * of parameterless `Macro`s holding the *unevaluated* begin-code and end-code. `\begin{name}` opens a group and
  * runs the begin-code; the matching `\end{name}` runs the end-code and then closes the group, so anything the
  * begin-code sets is local to the body and visible to the end-code, exactly as a LaTeX environment behaves. */
private val EnvStoreName = "envStore"

/** Read an environment name written LaTeX-style in braces (`\begin{quote}`); a bare identifier is also accepted
  * so the name can come from a variable or be written without braces. */
private def readEnvName(proc: Processor, pos: CharReader): String =
  stripOuterBraces(proc.readArgument(pos)).map {
    case Token.Text(s, _)       => s
    case Token.ControlSeq(n, _) => n
    case _                      => ""
  }.mkString.trim

/** Look up an environment: its begin-code's parameters, the begin-code tokens, and the end-code tokens. */
private def envCode(proc: Processor, name: String): Option[(Vector[MacroParam], Vector[Token], Vector[Token])] =
  proc.handler.get(EnvStoreName) match
    case Value.Map(m) =>
      m.get(name) match
        case Some(Value.Seq(Vector(Value.Macro(params, begin, _), Value.Macro(_, end, _)))) =>
          Some((params, begin, end))
        case _ => None
    case _ => None

/** `\ignorespaces` — swallow the run of `Space` tokens that immediately follows, as in LaTeX. A macro meant to
  * be followed directly by running text ends with `\ignorespaces`, so the space the author leaves for
  * readability (e.g. `\item the text`) does not survive as an interword space in the output. Newlines are left
  * alone, so a blank line still ends a paragraph. */
object IgnoreSpacesPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit = proc.skipSpaces()

/** `\newenvironment name [param…] {begin-code} {end-code}` — define an environment. Parameters are named
  * identifiers (as in `\def`) and are bound only in the begin-code, where `\begin{name}` supplies them as
  * arguments. Both code blocks are stored verbatim and only run when the environment is used. The definition is
  * global so a package can supply environments to a document. */
object NewEnvironmentPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    proc.handler.globalAssign = false // environment definitions are always global; ignore any stray prefix
    val name = readEnvName(proc, pos)

    // Read the parameter list (mandatory names and optional [name:default] specs) up to the begin-code brace,
    // exactly as \def does.
    val params = readMacroParams(proc, pos)

    val beginCode = stripOuterBraces(proc.readArgument(pos))
    val endCode   = stripOuterBraces(proc.readArgument(pos))
    val pair = Value.Seq(Vector(Value.Macro(params, beginCode, pos), Value.Macro(Vector.empty, endCode, pos)))
    val updated = proc.handler.get(EnvStoreName) match
      case Value.Map(m) => Value.Map(m + (name -> pair))
      case _            => Value.Map(Map(name -> pair))
    proc.handler.setGlobal(EnvStoreName, updated)

/** `\begin{name}` — open a fresh scope and run the environment's begin-code inside it, after reading and
  * substituting any declared arguments. The synthetic `BeginGroup` drives the normal scope machinery, so locals
  * set in the body (or by the begin-code) stay contained and `\global` still escapes. The name is pushed on the
  * environment stack so the matching `\end` can verify it. */
object BeginPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name = readEnvName(proc, pos)
    envCode(proc, name) match
      case Some((params, begin, _)) =>
        val expanded = proc.substituteNamedParams(begin, proc.readMacroArgs(params, pos))
        proc.envStack.push(name)
        proc.pushBack(Token.BeginGroup(pos) +: expanded)
      case None => proc.handler.error(s"Unknown environment '$name'", pos)

/** `\end{name}` — finish any paragraph the body left open, run the environment's end-code, and then close the
  * scope. Breaking the paragraph here, before the scope closes, lets the line builder read the environment's scoped
  * paragraph shape (`\leftskip`, `\hangindent`, …) while it is still in effect — without it the paragraph would be
  * laid out only at the next break, after the values had been restored (LaTeX's `\end` issues `\par` for the same
  * reason). The trailing synthetic `EndGroup` runs after the end-code, so the end-code can see anything the
  * begin-code or the body set locally. The name must match the innermost open `\begin`. */
object EndPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name = readEnvName(proc, pos)
    envCode(proc, name) match
      case Some((_, _, end)) =>
        if proc.envStack.isEmpty then proc.handler.error(s"\\end{$name} with no open environment", pos)
        val open = proc.envStack.pop()
        if open != name then proc.handler.error(s"\\end{$name} does not match \\begin{$open}", pos)
        proc.handler.endParagraph()
        proc.pushBack(end :+ Token.EndGroup(pos))
      case None => proc.handler.error(s"Unknown environment '$name'", pos)

// ============ ESCAPE SEQUENCES ============

class LiteralPrimitive(literal: String) extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    proc.handler.text(literal)
