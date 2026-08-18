package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.TexishException

/** Handler interface for integrating parser with a typesetter or other backend.
  *
  * The processor calls these methods as it expands and executes the input. This is a streaming interface - events are
  * delivered as they occur, not as a tree to walk.
  */
trait Handler:
  /** When non-null, [[text]]/[[space]]/[[newline]] append here instead of producing document output, so a caller
    * can collect the expanded text of a token list (what `\message` writes, for instance) without disturbing the
    * page. Hosts route their output methods through it; [[capture]] saves, installs and restores it. */
  var captureSink: StringBuilder | Null = null

  /** Run `thunk` with output diverted into a buffer and return the text it produced, restoring any previous sink so
    * captures nest. The variable scope is untouched, so the captured text reflects the live document state. */
  /** Whether output written now goes into the capture buffer. Suppression wins over a capture: a capture collects
    * the text a body writes, and an expression evaluated inside that body — a condition, a `\set` argument —
    * writes none. It matters because the comparison and lookup primitives set a result *and* write it, so a
    * capture that ignored suppression would collect "true" and "false" for every test the body ran on its way to
    * the text it meant to produce. */
  protected def capturing: Boolean = captureSink != null && !outputSuppressed

  def capture(thunk: => Unit): String =
    val prev = captureSink
    val sb   = new StringBuilder
    captureSink = sb
    try thunk
    finally captureSink = prev
    sb.toString

  /** Called when text content should be output */
  def text(s: String): Unit

  /** Called when a space should be output */
  def space(): Unit

  /** Called when a newline is encountered */
  def newline(): Unit

  /** Get a variable's value */
  def get(name: String): Value

  /** Set a variable's value */
  def set(name: String, value: Value): Unit

  /** Set a variable globally: visible now and still set after every enclosing group closes (TeX's \global). */
  def setGlobal(name: String, value: Value): Unit

  /** Set by a `\global` prefix, consumed by the next assignment to make it global. */
  var globalAssign: Boolean = false

  /** Enter a new scope (for grouping) */
  def enterScope(): Unit

  /** Exit the current scope */
  def exitScope(): Unit

  /** Finish the current block of text, if the host has a notion of one. A typesetting host breaks the open
    * paragraph here (so its scoped paragraph shape is read before an enclosing scope closes); a plain text host has
    * no paragraphs and does nothing. */
  def endParagraph(): Unit = ()

  /** Whether `\begin{name}` names a verbatim-style environment whose body must be captured raw (no tokenizing)
    * rather than run as ordinary `\begin`/`\end` code. A typesetting host overrides this for `verbatim`; the
    * default host has none. */
  def rawEnvironment(name: String): Boolean = false

  /** Typeset the raw body of a verbatim-style environment (see [[rawEnvironment]]). The text is exactly the
    * source between `\begin{name}` and `\end{name}`, with spaces and newlines preserved; the host lays it out
    * line for line. Only called for a `name` it claims via [[rawEnvironment]]. */
  def rawEnvironmentBody(name: String, body: String): Unit = ()

  /** Called when a command is executed that the handler should process.
    *
    * Returns the result value, or Value.Nil if the command produces no value.
    */
  def command(name: String, args: Seq[Value], pos: CharReader): Value

  /** Enable/disable output suppression (for expression evaluation) */
  def suppressOutput(suppress: Boolean): Unit

  /** The current output-suppression state, so a caller can save it, suppress, and restore the prior value — as
    * loading a module does, since a `\use` may be nested inside an already-suppressed expression. */
  def outputSuppressed: Boolean = false

  /** Resolve a font-relative unit (em, ex) to points. Hosts with a notion of a current font override this; the
    * default knows no such units, so dimensions using them stay unparsed.
    */
  def fontUnit(unit: String): Option[Double] = None

  /** Report an error at the given position. Uses char-reader's error formatting. `cause` is the exception
    * being reported, when there is one, so that its stack trace is not lost behind the formatted message.
    */
  def error(msg: String, pos: CharReader, cause: Throwable = null): Nothing =
    if pos == null then throw TexishException(msg, null, cause)
    else throw TexishException(pos.longErrorText(msg), pos, cause)

  /** Report a warning at the given position */
  def warning(msg: String, pos: CharReader): Unit =
    if pos == null then System.err.println(s"Warning: $msg")
    else System.err.println(s"Warning: ${pos.longErrorText(msg)}")

/** A simple handler that collects output as a string. Useful for testing. */
class StringHandler extends Handler:
  private val output = new StringBuilder
  private var scopes = List(scala.collection.mutable.Map[String, Value]())
  private var suppressed = false

  def result: String = output.toString

  def text(s: String): Unit =
    if capturing then captureSink.nn.append(s)
    else if !suppressed then output.append(s)

  def space(): Unit =
    if capturing then captureSink.nn.append(" ")
    else if !suppressed then output.append(" ")

  def newline(): Unit =
    if capturing then captureSink.nn.append("\n")
    else if !suppressed then output.append("\n")

  def suppressOutput(suppress: Boolean): Unit = suppressed = suppress

  override def outputSuppressed: Boolean = suppressed

  def get(name: String): Value =
    scopes.find(_.contains(name)).map(_(name)).getOrElse(Value.Undefined)

  def set(name: String, value: Value): Unit =
    scopes.head(name) = value

  // global: set in every open scope, so it is visible now and survives every group exit down to the outermost
  def setGlobal(name: String, value: Value): Unit =
    scopes.foreach(_(name) = value)

  def enterScope(): Unit =
    scopes = scala.collection.mutable.Map[String, Value]() :: scopes

  def exitScope(): Unit =
    scopes = scopes.tail

  def command(name: String, args: Seq[Value], pos: CharReader): Value =
    // Default: unknown commands produce nothing
    Value.Nil

/** A handler for using parser as a template engine.
  *
  * Pre-populates variables from a data map, then collects output as a string.
  */
class TemplateHandler(data: Map[String, Value] = Map.empty) extends StringHandler:
  // Pre-populate from data map
  data.foreach((k, v) => set(k, v))

/** Convenience object for simple template rendering */
object Template:
  /** Render a template string with the given data */
  def render(template: String, data: Map[String, Value] = Map.empty): String =
    val handler = new TemplateHandler(data)
    val proc = new Processor(handler)
    proc.process(template)
    handler.result

