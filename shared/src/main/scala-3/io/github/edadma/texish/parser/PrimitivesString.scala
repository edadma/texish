package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader

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

// The three case-and-whitespace operations set a result as well as writing their text, like every other string
// function here: without the result they are text-only, and a `\set x {\upcase{ab}}` falls back to the argument's
// own text — which looks like the operation ran and did nothing, since the fallback keeps the case it started in.
private def stringOp(proc: Processor, pos: CharReader, f: String => String): Unit =
  val out = f(Value.display(proc.evalStringArgument(pos)))
  proc.setResult(Value.Text(out))
  proc.handler.text(out)

object UpcasePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit = stringOp(proc, pos, _.toUpperCase)

object DowncasePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit = stringOp(proc, pos, _.toLowerCase)

object TrimPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit = stringOp(proc, pos, _.trim)

object SizePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val arg = proc.evalStringArgument(pos)
    val size = arg match
      case Value.Text(s) => s.length
      case Value.Seq(items) => items.size
      case Value.Map(entries) => entries.size
      case _ => 0
    // value (for \if/\set, where output is suppressed) and typeset output (in direct position)
    proc.setResult(Value.Num(size))
    proc.handler.text(size.toString)

/** \words{s} — split a string into the sequence of its whitespace-separated words, with runs of spaces, tabs and
  * newlines all treated as one separator and leading/trailing whitespace dropped. Where \seq splits a token list on
  * its space tokens, \words splits the *characters* of one string — the natural first step for parsing a verbatim
  * argument (a grammar, a formula) captured with a `<name>` raw parameter, whose whitespace lives inside a single
  * text value rather than between tokens. An empty or all-whitespace string yields the empty sequence. */
object WordsPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val s     = Value.display(proc.evalStringArgument(pos))
    val words = s.split("\\s+").iterator.filter(_.nonEmpty).map(w => Value.Text(w)).toVector
    proc.setResult(Value.Seq(words))

/** \cat{a}{b} — concatenate two values as text and return the result, for building up strings in the document
  * language (e.g. accumulating an element run while parsing a formula). A general string operation. */
object CatPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = Value.display(proc.evalStringArgument(pos))
    val b = Value.display(proc.evalStringArgument(pos))
    proc.setResult(Value.Text(a + b))
