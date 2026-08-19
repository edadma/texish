package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.path.Path
import io.github.edadma.texish.{Color, EmbeddedPackages, TexishException}
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

  // True while a module (\use) is loading. A module is code, not prose: an isolated source newline between the
  // statements of a captured macro/environment body is insignificant, the way a trailing // makes it. While this
  // is set, a captured body has its lone newlines dropped (see moduleBody), so package code needs no line-end
  // comments to stop a line break becoming a stray interword space when the macro later runs in a document.
  var inModuleLoad: Boolean = false

  // Named counters (LaTeX-style \newcounter/\stepcounter/\value, …). Counters are global by definition — TeX never
  // restores them at group exit — so they live in plain maps here rather than the scoped variable store.
  // counterValues maps a counter name to its current integer; counterParent maps a child counter to the parent
  // whose \stepcounter resets it (LaTeX's reset lists), applied recursively by \stepcounter.
  private[parser] val counterValues = mutable.Map[String, Int]()
  private[parser] val counterParent = mutable.Map[String, String]()

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
  registerPrimitive("while", WhilePrimitive)
  registerPrimitive("done", DonePrimitive)

  // File inclusion (raw input) and module import (load-once, no typesetting)
  registerPrimitive("include", IncludePrimitive)
  registerPrimitive("use", UsePrimitive)

  // Scripture: translate a USFM file into calls on the `usfm` package's macros and typeset it
  registerPrimitive("usfm", UsfmPrimitive)

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
  registerPrimitive("words", WordsPrimitive)
  registerPrimitive("message", MessagePrimitive)
  registerPrimitive("oklch", OklchPrimitive)
  registerPrimitive("oklchof", OklchOfPrimitive)
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
  registerPrimitive("mapdel", MapDelPrimitive)
  registerPrimitive("keys", KeysPrimitive)
  registerPrimitive("values", ValuesPrimitive)

  // Data primitives: indexing, slicing, ordering, searching and folding over sequences and strings
  // (see PrimitivesData.scala). A string counts as the sequence of its characters throughout.
  registerPrimitive("nth", NthPrimitive)
  registerPrimitive("slice", SlicePrimitive)
  registerPrimitive("reverse", ReversePrimitive)
  registerPrimitive("append", AppendPrimitive)
  registerPrimitive("prepend", PrependPrimitive)
  registerPrimitive("concat", ConcatPrimitive)
  registerPrimitive("join", JoinPrimitive)
  registerPrimitive("chunk", ChunkPrimitive)
  registerPrimitive("contains", ContainsPrimitive)
  registerPrimitive("indexof", IndexOfPrimitive)
  registerPrimitive("total", TotalPrimitive)
  registerPrimitive("minimum", MinimumPrimitive)
  registerPrimitive("maximum", MaximumPrimitive)
  registerPrimitive("sort", SortPrimitive)
  registerPrimitive("sortby", SortByPrimitive)
  registerPrimitive("filter", FilterPrimitive)
  registerPrimitive("transform", TransformPrimitive)
  registerPrimitive("split", SplitPrimitive)
  registerPrimitive("replace", ReplacePrimitive)
  registerPrimitive("repeat", RepeatPrimitive)
  registerPrimitive("startswith", StartsWithPrimitive)
  registerPrimitive("endswith", EndsWithPrimitive)
  registerPrimitive("fixed", FixedPrimitive)

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

  // Named counters (LaTeX's \newcounter family). The formatters above turn a counter's value into a label; these
  // hold and advance the value. \value reads a counter as a number, so \arabic{\value{section}} composes the two.
  registerPrimitive("newcounter", NewCounterPrimitive)
  registerPrimitive("setcounter", SetCounterPrimitive)
  registerPrimitive("addtocounter", AddToCounterPrimitive)
  registerPrimitive("value", ValuePrimitive)
  registerPrimitive("counterwithin", CounterWithinPrimitive)
  registerPrimitive("stepcounter", StepCounterPrimitive)

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

  // Common text symbols, under their standard LaTeX names. Each is a single Unicode character emitted as literal
  // text — the same mechanism as the escapes above — so any document can type them, not only ones that
  // \use{document}: a poster, a chord chart or a music score wants an em-dash or a degree sign without pulling in
  // article-class title and sectioning machinery. A \def of the same name still overrides, as for any primitive.
  // (\dots / \ldots are mode-sensitive — a low ellipsis in text, dot glyphs in math — so they are registered with
  // the typesetting primitives, which know the mode, not here.)
  registerPrimitive("S", LiteralPrimitive("§"))
  registerPrimitive("P", LiteralPrimitive("¶"))
  registerPrimitive("dag", LiteralPrimitive("†"))
  registerPrimitive("ddag", LiteralPrimitive("‡"))
  registerPrimitive("copyright", LiteralPrimitive("©"))
  registerPrimitive("textregistered", LiteralPrimitive("®"))
  registerPrimitive("texttrademark", LiteralPrimitive("™"))
  registerPrimitive("pounds", LiteralPrimitive("£"))
  registerPrimitive("textbullet", LiteralPrimitive("•"))
  registerPrimitive("textemdash", LiteralPrimitive("—"))
  registerPrimitive("textendash", LiteralPrimitive("–"))
  registerPrimitive("textdegree", LiteralPrimitive("°"))

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

  /** Dispatch one token to the handler, attaching the token's source position to any error raised while
    * handling it. Errors from the language layer already carry a position and pass through unchanged.
    *
    * An error that already carries a position was raised by the language layer, which knew where it was, and
    * passes through untouched. One without a position came from the engine — which has no notion of where in
    * a document it is — and is re-raised against the token being handled, which is what puts a line and column
    * on a failure thrown from deep inside the typesetter.
    *
    * Anything that is not a [[io.github.edadma.texish.TexishException]] is a defect in texish rather than in
    * the document, and reporting it as though the author erred sends them hunting through their source for a
    * mistake that is not there. Such a failure is labelled an internal error and keeps the original exception
    * as its cause, so the stack trace that locates the bug survives the formatting.
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
      case e: TexishException if e.pos != null => throw e
      case e: TexishException =>
        handler.error(Option(e.getMessage).getOrElse(e.toString), Token.pos(token), e)
      case e: RuntimeException =>
        handler.error(s"internal error (${e.getClass.getName}): ${Option(e.getMessage).getOrElse("no message")}",
                      Token.pos(token),
                      e)

  // Open groups seen at dispatch, so a stray `}` is reported at its own position instead of unbalancing the
  // handler's scope stack (whose eventual underflow would surface much later as a cryptic empty-stack crash).
  private var groupDepth = 0

  private def handleBeginGroup(pos: CharReader): Unit =
    groupDepth += 1
    handler.enterScope()

  private def handleEndGroup(pos: CharReader): Unit =
    if groupDepth == 0 then handler.error("Unmatched '}'", pos)
    groupDepth -= 1
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
        case ParamKind.Raw             => Vector(Token.Text(readRawArgument(pos), pos))
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

  /** Read a `\verb`-style inline verbatim argument — the delimiter character, then the literal text up to its
    * next occurrence. Like a raw brace argument, this works only over live top-level input; through a macro the
    * text was already tokenized, so this errors rather than returning corrupted text. */
  def readVerb(pos: CharReader): String =
    tokenSources.top.readVerb() match
      case Some(s) => s
      case None =>
        handler.error("\\verb needs a delimiter and verbatim text on the input directly (not through a macro)", pos)

  /** Read source literally up to the matching `\end{name}` (consuming the sentinel) — the raw body of a verbatim
    * environment. Works only over live top-level input, as the raw reads above do. */
  def readRawUntilEnd(name: String, pos: CharReader): String =
    tokenSources.top.readRawUntilEnd(name) match
      case Some(s) => s
      case None =>
        handler.error(s"\\begin{$name} without a matching \\end{$name} (a verbatim environment cannot come from a macro)", pos)

  /** Read an optional verbatim `[...]` argument (e.g. the `[language]` of `\code`) raw from live input, before a
    * raw brace body. None if there is no `[` next. */
  def readOptionalRawBracket(): Option[String] = tokenSources.top.readOptionalRawBracket()

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
        // The whitespace after an unbraced argument is consumed — deliberate texish semantics (pinned by
        // ProcessorTests and relied on by the packages), though it diverges from TeX, where the space after
        // `\upcase a b` would be document content. Brace the argument to keep a following space.
        val tok = Vector(nextToken())
        skipSpaces()
        tok

  /** Read a single math script field, the argument of a `^` or `_`: a braced group is the whole group; a
    * control sequence is that one token; a run of text contributes only its first character (its first code
    * point, so an astral symbol's surrogate pair stays whole), the rest pushed back to be read normally — so
    * `x^2y` makes `2` the script and `y` a following atom, as in TeX. */
  def readScriptField(pos: CharReader): Vector[Token] =
    skipSpaces()
    if !hasMoreTokens then handler.error("Expected a superscript or subscript", pos)

    peekToken() match
      case Token.BeginGroup(_) => readArgument(pos)
      case Token.Text(s, p) =>
        nextToken()
        val n =
          if Character.isHighSurrogate(s.charAt(0)) && s.length > 1 && Character.isLowSurrogate(s.charAt(1))
          then 2
          else 1
        if s.length > n then tokenSources.push(TokenListSource(Vector(Token.Text(s.substring(n), p))))
        Vector(Token.Text(s.substring(0, n), p))
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

  /** Collect the body of an environment up to its matching `\end{name}`, working at the token level — so it
    * reads from whatever source is current, a live tokenizer or an already-tokenized macro body or argument,
    * unlike the raw character scan a verbatim environment needs. Nested `\begin{name}`/`\end{name}` of the same
    * name are balanced; the closing `\end{name}` is consumed and dropped. A `\begin`/`\end` of any other name
    * passes into the body verbatim, its name re-emitted as a braced group, to be handled when the body is
    * re-processed (so a matrix environment can sit inside an aligned cell). This is what lets the math-array
    * environments compose inside `\left…\right`, inside macros, and inside one another. */
  def collectEnvBody(name: String, pos: CharReader): Vector[Token] =
    val body  = Vector.newBuilder[Token]
    var depth = 1

    def nameGroup(n: String, p: CharReader): Unit =
      body += Token.BeginGroup(p)
      body += Token.Text(n, p)
      body += Token.EndGroup(p)

    while depth > 0 && hasMoreTokens do
      nextToken() match
        case t @ Token.ControlSeq("begin", p) =>
          val inner = readEnvName(this, p)
          if inner == name then depth += 1
          body += t
          nameGroup(inner, p)
        case t @ Token.ControlSeq("end", p) =>
          val inner = readEnvName(this, p)
          if inner == name then
            depth -= 1
            if depth > 0 then { body += t; nameGroup(inner, p) }
          else
            body += t
            nameGroup(inner, p)
        case Token.EOF(p) => handler.error(s"\\begin{$name} without a matching \\end{$name}", p)
        case t            => body += t

    if depth > 0 then handler.error(s"\\begin{$name} without a matching \\end{$name}", pos)
    body.result()

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

  /** Typeset a freshly built source string as part of the current document — re-entrant, so a primitive can
    * synthesize content (a table of contents replaying its entries) and have it processed in place, returning to
    * the enclosing document at the same stack depth. Unlike [[loadModule]] output is not suppressed. */
  def processContent(content: String): Unit =
    val minDepth = tokenSources.size
    tokenSources.push(TokenizerSource(Tokenizer(content, activeChars)))
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
    val saved       = handler.outputSuppressed
    val savedModule = inModuleLoad
    val minDepth    = tokenSources.size
    dirStack.push(dir)
    tokenSources.push(TokenizerSource(Tokenizer(content, activeChars)))
    handler.suppressOutput(true)
    inModuleLoad = true
    try processTokensUntilDepth(minDepth)
    finally
      handler.suppressOutput(saved)
      inModuleLoad = savedModule
      dirStack.pop()

  /** Filter a macro/environment body captured while a module is loading. An isolated source newline between two
    * statements of package code is dropped — outside a paragraph it never mattered, and inside one it would
    * become a stray interword space when the macro later runs, which is why hand-written packages terminate every
    * body line with `//`. A blank line (a run of two or more newlines) is kept, so an intentional paragraph break
    * inside a body still ends the paragraph. Outside module loading the body is returned unchanged. */
  def moduleBody(body: Vector[Token]): Vector[Token] =
    if !inModuleLoad then body
    else
      val out = Vector.newBuilder[Token]
      val n   = body.length
      var i   = 0
      while i < n do
        body(i) match
          case _: Token.Newline =>
            var j = i
            while j < n && body(j).isInstanceOf[Token.Newline] do j += 1
            if j - i >= 2 then out ++= body.slice(i, j) // blank line: a real paragraph break, keep it
            i = j
          case t =>
            out += t
            i += 1
      out.result()

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

  /** Evaluate an argument for the string functions, where a lone run of literal characters is those characters and
    * not the number they would parse as. `\words{1.e4}` is a move of chess notation, not 10000 — which is what
    * "1.e4" means read as a double — and `\upcase{007}` is "007". Everything else evaluates as usual, so a variable,
    * a nested call and a sequence all still arrive as values; only the one case where a string function was handed
    * the characters themselves is kept as written. This is what makes a verbatim `<name>` argument survive being
    * split, which is the use its own documentation names.
    */
  def evalStringArgument(pos: CharReader): Value =
    stripOuterBraces(readArgument(pos)) match
      case Vector(Token.Text(s, _)) => Value.Text(s)
      case ts                       => evalTokensExpr(ts, pos)

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
  private[parser] def evalTokensExpr(tokens: Vector[Token], pos: CharReader): Value =
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
                try prim.execute(this, csPos)
                finally handler.suppressOutput(savedSuppress)
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
                // Track the pushed source by identity: argument reading skips spaces, which can exhaust and pop
                // this source already, so popping whatever is now on top would over-pop into the enclosing
                // source. Only pop our own source, and only if the primitive left it on top unexhausted.
                val rest       = tokens.tail
                val restSource = if rest.nonEmpty then Some(TokenListSource(rest)) else None
                restSource.foreach(tokenSources.push)
                lastResult = Value.Nil
                val savedSuppress = handler.outputSuppressed
                handler.suppressOutput(true)
                try prim.execute(this, csPos)
                finally handler.suppressOutput(savedSuppress)
                restSource.foreach(s => if tokenSources.nonEmpty && (tokenSources.top eq s) then tokenSources.pop())
                val r = getResult
                if r == Value.Nil then evalTokens(tokens, handler) else r
              case None =>
                // A variable followed by more tokens is a text interpolation: concatenate the value's display
                // with the rest (`\set y {\x tail}` keeps " tail") instead of silently dropping the tail. A tail
                // of nothing but whitespace is not content — the value passes through with its type intact.
                handler.get(name) match
                  case Value.Undefined => evalTokens(tokens, handler)
                  case v =>
                    if tokens.tail.forall(t => t.isInstanceOf[Token.Space] || t.isInstanceOf[Token.Newline]) then v
                    else evalTokens(tokens, handler)

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
  // Verbatim reads, available only over live (untokenized) input — see Tokenizer.readRawGroup / readVerb /
  // readRawUntilEnd. A pre-tokenized list cannot offer them: its text was already tokenized, so any // is long gone.
  def readRawGroup(): Option[String] = None
  def readVerb(): Option[String] = None
  def readRawUntilEnd(name: String): Option[String] = None
  def readOptionalRawBracket(): Option[String] = None

class TokenizerSource(tokenizer: Tokenizer) extends TokenSource:
  def peek: Token = tokenizer.peek
  def next(): Token = tokenizer.next()
  def atEnd: Boolean = tokenizer.atEnd
  override def readRawGroup(): Option[String] = tokenizer.readRawGroup()
  override def readVerb(): Option[String] = tokenizer.readVerb()
  override def readRawUntilEnd(name: String): Option[String] = tokenizer.readRawUntilEnd(name)
  override def readOptionalRawBracket(): Option[String] = tokenizer.readOptionalRawBracket()

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

