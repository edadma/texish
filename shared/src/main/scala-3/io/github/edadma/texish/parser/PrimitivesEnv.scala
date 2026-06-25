package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader

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
private[parser] def readEnvName(proc: Processor, pos: CharReader): String =
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
    // A verbatim-style environment captures its body raw, straight from the input, and the matching \end is
    // consumed by that read — so it never reaches the generic env machinery below and runs no end-code.
    if proc.handler.rawEnvironment(name) then
      proc.handler.rawEnvironmentBody(name, proc.readRawUntilEnd(name, pos))
      return
    // A math-array environment (matrix family, \cases, aligned/split) captures its body raw and builds an array
    // box directly, the same as the brace matrix primitives — it runs no begin/end code.
    if tryMathArrayEnv(proc, name, pos) then return
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
