package io.github.edadma.typesetter.parser

import io.github.edadma.char_reader.CharReader

/** Tokens produced by the tokenizer and consumed by the processor.
  *
  * Each token carries its source position for error reporting.
  */
enum Token:
  /** Literal text content (not containing special characters) */
  case Text(s: String, pos: CharReader)

  /** A control sequence: \name */
  case ControlSeq(name: String, pos: CharReader)

  /** Begin group: { */
  case BeginGroup(pos: CharReader)

  /** End group: } */
  case EndGroup(pos: CharReader)

  /** An active character (like & for alignment) */
  case Active(char: Char, pos: CharReader)

  /** Whitespace that may be significant */
  case Space(s: String, pos: CharReader)

  /** Newline */
  case Newline(pos: CharReader)

  /** End of input */
  case EOF(pos: CharReader)

object Token:
  /** Get the source position of a token */
  def pos(t: Token): CharReader = t match
    case Text(_, p)       => p
    case ControlSeq(_, p) => p
    case BeginGroup(p)    => p
    case EndGroup(p)      => p
    case Active(_, p)     => p
    case Space(_, p)      => p
    case Newline(p)       => p
    case EOF(p)           => p

  /** Check if token is end of input */
  def isEOF(t: Token): Boolean = t match
    case EOF(_) => true
    case _      => false

  /** Format token for error messages */
  def show(t: Token): String = t match
    case Text(s, _)       => s"text \"$s\""
    case ControlSeq(n, _) => s"\\$n"
    case BeginGroup(_)    => "{"
    case EndGroup(_)      => "}"
    case Active(c, _)     => s"active char '$c'"
    case Space(_, _)      => "<space>"
    case Newline(_)       => "<newline>"
    case EOF(_)           => "<end of input>"
