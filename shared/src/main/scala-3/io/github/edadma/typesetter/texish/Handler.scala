package io.github.edadma.typesetter.texish

import io.github.edadma.char_reader.CharReader

/** Handler interface for integrating texish with a typesetter or other backend.
  *
  * The processor calls these methods as it expands and executes the input. This is a streaming interface - events are
  * delivered as they occur, not as a tree to walk.
  */
trait Handler:
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

  /** Enter a new scope (for grouping) */
  def enterScope(): Unit

  /** Exit the current scope */
  def exitScope(): Unit

  /** Called when a command is executed that the handler should process.
    *
    * Returns the result value, or Value.Nil if the command produces no value.
    */
  def command(name: String, args: Seq[Value], pos: CharReader): Value

  /** Enable/disable output suppression (for expression evaluation) */
  def suppressOutput(suppress: Boolean): Unit

  /** Resolve a font-relative unit (em, ex) to points. Hosts with a notion of a current font override this; the
    * default knows no such units, so dimensions using them stay unparsed.
    */
  def fontUnit(unit: String): Option[Double] = None

  /** Report an error at the given position. Uses char-reader's error formatting. */
  def error(msg: String, pos: CharReader): Nothing =
    if pos == null then throw TexishException(msg, null)
    else throw TexishException(pos.longErrorText(msg), pos)

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

  def text(s: String): Unit = if !suppressed then output.append(s)

  def space(): Unit = if !suppressed then output.append(" ")

  def newline(): Unit = if !suppressed then output.append("\n")

  def suppressOutput(suppress: Boolean): Unit = suppressed = suppress

  def get(name: String): Value =
    scopes.find(_.contains(name)).map(_(name)).getOrElse(Value.Undefined)

  def set(name: String, value: Value): Unit =
    scopes.head(name) = value

  def enterScope(): Unit =
    scopes = scala.collection.mutable.Map[String, Value]() :: scopes

  def exitScope(): Unit =
    scopes = scopes.tail

  def command(name: String, args: Seq[Value], pos: CharReader): Value =
    // Default: unknown commands produce nothing
    Value.Nil

/** A handler for using texish as a template engine.
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

/** Exception thrown for texish errors */
case class TexishException(msg: String, pos: CharReader) extends RuntimeException(msg)
