package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader

/** LaTeX-style named counters. The number *formatters* (`\arabic`/`\roman`/…) turn an integer into a label; these
  * primitives hold and advance the integer a section, list or theorem label is built from. State lives in two maps
  * on the [[Processor]] — `counterValues` (name → count) and `counterParent` (child → the parent that resets it).
  * Counters are global by definition: TeX never restores them at group exit, so they are kept outside the scoped
  * variable store. Stepping a counter zeroes every counter declared `\counterwithin` it, recursively, reproducing
  * LaTeX's reset lists so that, e.g., advancing the section counter resets subsection back to zero.
  */
private[parser] object Counters:

  /** A counter name, evaluated like any argument so a computed name works, matching `\mapset`/`\mapget`. */
  def name(proc: Processor, pos: CharReader): String =
    Value.display(proc.evalArgumentExpr(pos))

  /** An integer-valued argument (the new value for `\setcounter`, the increment for `\addtocounter`). */
  def int(proc: Processor, pos: CharReader): Int =
    proc.evalArgumentExpr(pos) match
      case Value.Num(x)  => x.toInt
      case Value.Text(s) => s.trim.toIntOption.getOrElse(proc.handler.error(s"Expected a number, got '$s'", pos))
      case other         => proc.handler.error(s"Expected a number, got ${Value.display(other)}", pos)

  /** Zero every counter whose parent is `parent`, then recurse, so a single `\stepcounter` cascades down the whole
    * reset list (section → subsection → subsubsection, …). */
  def resetWithin(proc: Processor, parent: String): Unit =
    for (child, p) <- proc.counterParent if p == parent do
      proc.counterValues(child) = 0
      resetWithin(proc, child)

/** `\newcounter{name}` — declare a counter, initialized to zero. */
object NewCounterPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    proc.counterValues(Counters.name(proc, pos)) = 0

/** `\setcounter{name}{value}` — set a counter to an explicit value. */
object SetCounterPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val n = Counters.name(proc, pos)
    proc.counterValues(n) = Counters.int(proc, pos)

/** `\addtocounter{name}{amount}` — add an integer (possibly negative) to a counter. */
object AddToCounterPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val n = Counters.name(proc, pos)
    proc.counterValues(n) = proc.counterValues.getOrElse(n, 0) + Counters.int(proc, pos)

/** `\value{name}` — the counter's current integer, as a result so it composes in an expression (the usual use is
  * `\arabic{\value{section}}`, formatting the value the counter holds). An undeclared counter reads as zero. */
object ValuePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val n = Counters.name(proc, pos)
    proc.setResult(Value.Num(proc.counterValues.getOrElse(n, 0)))

/** `\counterwithin{child}{parent}` — make `child` reset to zero whenever `parent` steps (LaTeX's reset lists). */
object CounterWithinPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val child  = Counters.name(proc, pos)
    val parent = Counters.name(proc, pos)
    proc.counterParent(child) = parent

/** `\stepcounter{name}` — add one to a counter, then reset every counter declared `\counterwithin` it. */
object StepCounterPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val n = Counters.name(proc, pos)
    proc.counterValues(n) = proc.counterValues.getOrElse(n, 0) + 1
    Counters.resetWithin(proc, n)
