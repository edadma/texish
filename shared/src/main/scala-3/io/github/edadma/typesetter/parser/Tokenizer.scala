package io.github.edadma.typesetter.parser

import io.github.edadma.char_reader.CharReader

/** Tokenizer for the parser macro language.
  *
  * Converts character input into a stream of tokens. This is a pull-based tokenizer - call next() to get each token.
  *
  * @param input The character reader to tokenize
  * @param activeChars Set of characters to treat as active (default: just ~)
  */
class Tokenizer(input: CharReader, activeChars: Set[Char] = Set('~')):
  private var reader: CharReader = input
  private var pendingToken: Option[Token] = None

  /** Peek at the next token without consuming it */
  def peek: Token =
    pendingToken match
      case Some(t) => t
      case None =>
        val t = readToken()
        pendingToken = Some(t)
        t

  /** Get the next token */
  def next(): Token =
    pendingToken match
      case Some(t) =>
        pendingToken = None
        t
      case None =>
        readToken()

  /** Check if we're at end of input */
  def atEnd: Boolean = peek match
    case Token.EOF(_) => true
    case _            => false

  private def readToken(): Token =
    skipComments()
    if reader.eoi then Token.EOF(reader)
    else
      val c = reader.ch
      // Active characters take precedence over default special handling (except \ { } %)
      if isActive(c) then readActive()
      else c match
        case '\\' => readControlSeq()
        case '{'  => readBeginGroup()
        case '}'  => readEndGroup()
        case '\n' => readNewline()
        case _ if c.isWhitespace => readSpace()
        case _                   => readText()

  private def skipComments(): Unit =
    while reader.ch == '%' do
      while !reader.eoi && reader.ch != '\n' do reader = reader.next
      if reader.ch == '\n' then reader = reader.next

  private def readControlSeq(): Token =
    val pos = reader
    reader = reader.next // skip backslash
    if reader.eoi then Token.Text("\\", pos)
    else if reader.ch.isLetter then
      val start = reader
      val name = new StringBuilder
      while !reader.eoi && reader.ch.isLetter do
        name.append(reader.ch)
        reader = reader.next
      Token.ControlSeq(name.toString, pos)
    else if isSymbolic(reader.ch) then
      // Symbolic control sequence - read consecutive symbolic chars
      val name = new StringBuilder
      while !reader.eoi && isSymbolic(reader.ch) do
        name.append(reader.ch)
        reader = reader.next
      Token.ControlSeq(name.toString, pos)
    else
      // Single special character escape like \{ or \}
      val c = reader.ch
      reader = reader.next
      Token.ControlSeq(c.toString, pos)

  private def readBeginGroup(): Token =
    val pos = reader
    reader = reader.next
    Token.BeginGroup(pos)

  private def readEndGroup(): Token =
    val pos = reader
    reader = reader.next
    Token.EndGroup(pos)

  private def readActive(): Token =
    val pos = reader
    val c = reader.ch
    reader = reader.next
    Token.Active(c, pos)

  private def readNewline(): Token =
    val pos = reader
    reader = reader.next
    Token.Newline(pos)

  private def readSpace(): Token =
    val pos = reader
    val sb = new StringBuilder
    while !reader.eoi && reader.ch.isWhitespace && reader.ch != '\n' do
      sb.append(reader.ch)
      reader = reader.next
    Token.Space(sb.toString, pos)

  private def readText(): Token =
    val pos = reader
    val sb = new StringBuilder
    while !reader.eoi && !isSpecial(reader.ch) do
      sb.append(reader.ch)
      reader = reader.next
    Token.Text(sb.toString, pos)

  private def isSpecial(c: Char): Boolean =
    c == '\\' || c == '{' || c == '}' || c == '%' || c == '\n' || c.isWhitespace || isActive(c)

  private def isActive(c: Char): Boolean = activeChars.contains(c)

  private def isSymbolic(c: Char): Boolean =
    !c.isLetterOrDigit && !c.isWhitespace && c != '\\' && c != '{' && c != '}' && c != '%' && !isActive(c)

object Tokenizer:
  def apply(input: String, activeChars: Set[Char] = Set('~')): Tokenizer =
    new Tokenizer(CharReader.fromString(input), activeChars)

  def apply(reader: CharReader, activeChars: Set[Char]): Tokenizer =
    new Tokenizer(reader, activeChars)

  def apply(reader: CharReader): Tokenizer =
    new Tokenizer(reader)
