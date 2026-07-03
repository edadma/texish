package io.github.edadma.texish.parser

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

  /** Read a brace-delimited group verbatim, with no tokenization: comments, escapes and active characters are
    * all taken literally, nested braces are balanced, and the raw characters between the outer braces are
    * returned. The next thing must be a `{` — a peeked `{` is honoured, otherwise the next input character must
    * be one. Returns None if there is no opening brace, or if the input ends before the group closes. This is
    * how a URL (which contains `//`, otherwise a comment) is read intact. */
  def readRawGroup(): Option[String] =
    pendingToken match
      case Some(Token.BeginGroup(_)) => pendingToken = None
      case Some(_)                   => return None
      case None =>
        if reader.eoi || reader.ch != '{' then return None
        reader = reader.next

    val sb    = new StringBuilder
    var depth = 1
    while depth > 0 && !reader.eoi do
      reader.ch match
        case '{' => depth += 1; sb.append('{')
        case '}' => depth -= 1; if depth > 0 then sb.append('}')
        case c   => sb.append(c)
      reader = reader.next

    if depth == 0 then Some(sb.toString) else None

  /** Read a `\verb`-style inline argument: the next character is taken as the delimiter, then every character up
    * to its next occurrence is captured literally (no comments, escapes or active characters) and the closing
    * delimiter is consumed. This is how `\verb|a_b\c|` sets its text exactly as written. Returns None if a token
    * has already been peeked (the delimiter would be lost), or if the input ends before the closing delimiter. */
  def readVerb(): Option[String] =
    if pendingToken.isDefined || reader.eoi then return None
    val delim = reader.ch
    reader = reader.next

    val sb = new StringBuilder
    while !reader.eoi && reader.ch != delim do
      sb.append(reader.ch)
      reader = reader.next

    if reader.eoi then None
    else
      reader = reader.next // consume the closing delimiter
      Some(sb.toString)

  /** If the next character is `[`, read raw up to the matching `]` and return the bracketed content (consuming
    * both brackets, and any blanks left before a following `{`); otherwise leave the reader untouched and return
    * None. This reads an optional verbatim argument — the `[language]` of `\code[scala]{…}` — before the raw
    * brace body, where tokenizing the bracket first would defeat the raw read. Requires no token to have been
    * peeked. */
  def readOptionalRawBracket(): Option[String] =
    if pendingToken.isDefined || reader.eoi || reader.ch != '[' then return None
    reader = reader.next

    val sb = new StringBuilder
    while !reader.eoi && reader.ch != ']' do
      sb.append(reader.ch)
      reader = reader.next

    if reader.eoi then None
    else
      reader = reader.next // consume ]
      while !reader.eoi && (reader.ch == ' ' || reader.ch == '\t') do reader = reader.next
      Some(sb.toString)

  /** Read source literally up to the next `\end{<name>}`, consuming that sentinel, and return the raw text before
    * it (spaces and newlines preserved). This is the raw-capture a verbatim environment needs: unlike a generic
    * `\begin`/`\end` body, nothing between the delimiters is tokenized. The sentinel may carry spaces around its
    * braces (`\end {name}`). Works only over live input — returns None if a token has already been peeked, or if
    * the input ends before the matching `\end`. */
  def readRawUntilEnd(name: String): Option[String] =
    if pendingToken.isDefined then return None

    val sb = new StringBuilder
    while !reader.eoi do
      if matchesEnd(name) then return Some(sb.toString)
      sb.append(reader.ch)
      reader = reader.next

    None

  /** Try to match `\end{name}` (with optional spaces around the braces) at the current reader position. On a
    * match the reader is advanced past the closing brace and true is returned; on no match the reader is left
    * exactly where it was. */
  private def matchesEnd(name: String): Boolean =
    var r = reader

    def literal(s: String): Boolean =
      var i = 0
      while i < s.length do
        if r.eoi || r.ch != s.charAt(i) then return false
        r = r.next
        i += 1
      true

    def skipBlanks(): Unit = while !r.eoi && (r.ch == ' ' || r.ch == '\t') do r = r.next

    if !literal("\\end") then return false
    skipBlanks()
    if r.eoi || r.ch != '{' then return false
    r = r.next
    skipBlanks()
    if !literal(name) then return false
    skipBlanks()
    if r.eoi || r.ch != '}' then return false
    r = r.next
    reader = r
    true

  private def readToken(): Token =
    skipComments()
    if reader.eoi then Token.EOF(reader)
    else
      val c = reader.ch
      // Active characters take precedence over default special handling (except \ { } and // comments)
      if isActive(c) then readActive()
      else c match
        case '\\'        => readControlSeq()
        case '{'         => readBeginGroup()
        case '}'         => readEndGroup()
        case '\n' | '\r' => readNewline()
        case _ if c.isWhitespace => readSpace()
        case _                   => readText()

  // Comments run from `//` to the end of the line. `%` is an ordinary character (so "50%" and the `%` modulo
  // operator inside \calc work); a single `/` is ordinary text — only a doubled `//` starts a comment.
  private def skipComments(): Unit =
    while reader.ch == '/' && peekNextIs('/') do
      while !reader.eoi && reader.ch != '\n' do reader = reader.next
      if reader.ch == '\n' then reader = reader.next

  private def peekNextIs(c: Char): Boolean =
    val nx = reader.next
    !nx.eoi && nx.ch == c

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
      // Symbolic control sequence — a single symbolic character, with exactly three two-character exceptions:
      // the comparison operators `\!=`, `\<=` and `\>=`. Fusing whole symbolic runs would swallow following
      // punctuation — `\;(x)` must read as `\;` then `(x)`, not a control sequence named `;(` — and no other
      // multi-character symbolic control sequence exists.
      val first = reader.ch
      reader = reader.next
      if (first == '!' || first == '<' || first == '>') && !reader.eoi && reader.ch == '=' then
        reader = reader.next
        Token.ControlSeq(s"$first=", pos)
      else Token.ControlSeq(first.toString, pos)
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

  // A line ending is one Newline token whether the source uses LF or CRLF (a lone CR — classic Mac — also
  // counts). Letting the CR fall through to readSpace would make every CRLF line end a Space token *and* a
  // Newline token: a doubled interword space at each line break, and a blank line that no longer reads as the
  // two consecutive Newlines a paragraph break looks for.
  private def readNewline(): Token =
    val pos = reader
    if reader.ch == '\r' then
      reader = reader.next
      if !reader.eoi && reader.ch == '\n' then reader = reader.next
    else reader = reader.next
    Token.Newline(pos)

  private def readSpace(): Token =
    val pos = reader
    val sb = new StringBuilder
    while !reader.eoi && reader.ch.isWhitespace && reader.ch != '\n' && reader.ch != '\r' do
      sb.append(reader.ch)
      reader = reader.next
    Token.Space(sb.toString, pos)

  private def readText(): Token =
    val pos = reader
    val sb = new StringBuilder
    while !reader.eoi && !isSpecial(reader.ch) && !(reader.ch == '/' && peekNextIs('/')) do
      sb.append(reader.ch)
      reader = reader.next
    Token.Text(sb.toString, pos)

  private def isSpecial(c: Char): Boolean =
    c == '\\' || c == '{' || c == '}' || c == '\n' || c.isWhitespace || isActive(c)

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
