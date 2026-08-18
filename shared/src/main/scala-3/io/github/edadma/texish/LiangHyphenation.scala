package io.github.edadma.texish

import scala.collection.mutable

/** Liang's hyphenation algorithm, as TeX does it: a table of patterns, an exception list of words spelled out by
  * hand, and the two minima that say how much of a word must be left on each side of a break.
  *
  * The patterns are the algorithm proper. Each carries digits between its letters, odd digits proposing a break
  * and even ones forbidding it; a word is scored by laying every matching pattern over it and taking the highest
  * digit at each position. The exception list is consulted first and settles the word outright, which is how a
  * language pins the handful of words its patterns get wrong. The minima come from the pattern file itself, so a
  * language whose typography wants three letters after a break gets three (English is such a language).
  */
class LiangHyphenation private (
    patterns:   Map[String, IndexedSeq[Int]],
    exceptions: Map[String, IndexedSeq[Int]],
    minLeft:    Int = 2,
    minRight:   Int = 2,
):

  /** Where `word` may be broken: the index of the last character before each break. */
  def hyphenate(word: String): IndexedSeq[Int] =
    // Hyphenate only the alphabetic core: leading and trailing non-letters (an opening parenthesis or quote, a
    // trailing period, the digits and colons of a reference) are not part of the word and must not count toward
    // break positions — otherwise "(Rom." breaks as "(R-om." and the abbreviation "Cor." as "Cor-.". Strip them,
    // hyphenate the core, and shift the points back so they index into the original word.
    val first = word.indexWhere(_.isLetter)
    if first < 0 then return IndexedSeq.empty
    val core = word.substring(first, word.lastIndexWhere(_.isLetter) + 1)
    if core.length < minLeft + minRight then return IndexedSeq.empty

    exceptions.get(core.toLowerCase) match
      case Some(points) => points.filter(permitted(_, core.length)).map(_ + first)
      case None         => fromPatterns(core).filter(permitted(_, core.length)).map(_ + first)

  /** The minima bind the exception list as well as the patterns: a word written `ta-ble` in a `\hyphenation`
    * list still may not break where the language forbids a break, which is what stops an exception from
    * stranding a letter or two on a line of its own. */
  private def permitted(point: Int, length: Int): Boolean =
    point >= minLeft - 1 && point < length - minRight

  private def fromPatterns(core: String): IndexedSeq[Int] =
    val marked = s".${core.toLowerCase}."
    val values = Array.fill(marked.length + 1)(0)

    // Find all matching patterns and overlay values (take max)
    for i <- marked.indices do
      for len <- 1 to (marked.length - i) do
        patterns.get(marked.substring(i, i + len)).foreach { patternValues =>
          for (v, j) <- patternValues.zipWithIndex do
            val pos = i + j
            if pos < values.length then values(pos) = math.max(values(pos), v)
        }

    // Odd values = hyphenation points, adjusted for the '.' marker
    (for
      i <- 2 until (values.length - 2)
      if values(i) % 2 == 1
    yield i - 2).toIndexedSeq

  /** Iterator of (before-with-hyphen, after) pairs, one per break point. */
  def apply(word: String): Option[Iterator[(String, String)]] =
    val points = hyphenate(word)
    Option.when(points.nonEmpty) {
      points.iterator.map { idx =>
        (word.substring(0, idx + 1) + "-", word.substring(idx + 1))
      }
    }

object LiangHyphenation:

  /** Everything a TeX pattern file says, which is more than its patterns: the exception list its `\hyphenation`
    * block spells out, and the hyphenation minima its header records. Reading all three is what makes a file
    * from `hyph-utf8` behave in texish as it does in TeX. */
  case class PatternFile(
      patterns:   Map[String, IndexedSeq[Int]],
      exceptions: Map[String, IndexedSeq[Int]],
      minLeft:    Int,
      minRight:   Int,
  )

  /** Parse TeX pattern like "hy3ph" into ("hyph", IndexedSeq(0,0,3,0,0)) */
  def parsePattern(pattern: String): (String, IndexedSeq[Int]) =
    val letters = new StringBuilder
    val values  = mutable.ArrayBuffer[Int]()

    for c <- pattern do
      if c.isDigit then values += (c - '0')
      else
        if values.length == letters.length then values += 0
        letters += c

    if values.length == letters.length then values += 0
    (letters.toString, values.toIndexedSeq)

  /** The patterns of a pattern file, for a caller that wants nothing else from it. */
  def parsePatterns(content: String): Map[String, IndexedSeq[Int]] = parse(content).patterns

  /** Read a TeX pattern file, in the dialect the files in `hyph-utf8` are actually written in.
    *
    * Comments go first, and a TeX comment runs from its `%` to the end of the line — not merely to the next
    * space. Dropping only the tokens that begin with one leaves the prose of a comment behind to be read as
    * patterns: the French file heads its table with `% phonetic patterns % etymological patterns %`, which
    * contributed `phonetic`, `patterns` and `etymological` and broke words wherever they matched.
    *
    * What is left is read as TeX reads it, which the files rely on in four ways:
    *
    *   - **`\patterns{…}` and `\hyphenation{…}` are balanced groups**, so the block ends at the brace that
    *     matches its opening one rather than at the first `}`. Stopping at the first one silently truncates any
    *     file that has a brace inside its table — Esperanto's is mostly written with macros, and stopping early
    *     there loses all but its opening lines.
    *   - **A one-argument macro is expanded where it is called.** Esperanto defines `\nom`, `\adj` and `\ver` to
    *     generate the patterns for a stem's inflected forms and then writes `\nom{1a2n}` a few hundred times;
    *     without expansion those patterns simply are not there.
    *   - **`\input` pulls in another file.** Norwegian is one pattern set with two exception lists over it, so
    *     `hyph-nb.tex` and `hyph-nn.tex` are an `\input hyph-no.tex` and a dozen words each.
    *   - **The header carries the language's hyphenation minima**, under `hyphenmins:` and in the file's leading
    *     comment block. They differ by language and matter: English asks for three letters after a break, so
    *     ignoring them sets `ta-ble` where TeX sets none.
    *
    * `include` resolves an `\input`, by the name as written (`hyph-no.tex`); a caller with nowhere to read from
    * passes nothing and gets a file's own content alone.
    */
  def parse(content: String, include: String => Option[String] = _ => None): PatternFile =
    val text  = stripComments(content)
    val bases = inputs(text).flatMap(include).map(parse(_, include))
    val defs  = macroDefinitions(text)

    val patternBlocks   = blocks(text, "patterns").map(expand(_, defs))
    val exceptionBlocks = blocks(text, "hyphenation").map(expand(_, defs))

    // A caller may hand over a bare table with no TeX around it at all — `\loadhyphenation` of a file somebody
    // wrote by hand, or a test's one-line pattern set. With no block and nothing included, the whole text is
    // the table; with either, it is not, or Norwegian's exception words would be read as patterns as well.
    val ownPatterns =
      if patternBlocks.isEmpty && exceptionBlocks.isEmpty && bases.isEmpty then tokens(text).map(parsePattern).toMap
      else patternBlocks.flatMap(b => tokens(b).map(parsePattern)).toMap

    val ownExceptions = exceptionBlocks.flatMap(b => tokens(b).map(parseException)).toMap
    val mins          = hyphenMins(content) orElse bases.headOption.map(b => (b.minLeft, b.minRight))

    PatternFile(
      patterns = bases.foldLeft(Map.empty[String, IndexedSeq[Int]])(_ ++ _.patterns) ++ ownPatterns,
      exceptions = bases.foldLeft(Map.empty[String, IndexedSeq[Int]])(_ ++ _.exceptions) ++ ownExceptions,
      minLeft = mins.map(_._1).getOrElse(2),
      minRight = mins.map(_._2).getOrElse(2),
    )

  /** An exception-list entry — `as-so-ciate`, or a bare `project` meaning "never break this word" — as the word
    * itself and the points its hyphens mark. */
  private def parseException(entry: String): (String, IndexedSeq[Int]) =
    val word   = new StringBuilder
    val points = mutable.ArrayBuffer[Int]()

    for c <- entry do
      if c == '-' then points += word.length - 1
      else word += c

    (word.toString.toLowerCase, points.toIndexedSeq)

  private def tokens(block: String): Iterator[String] =
    block.split("\\s+").iterator.filter(_.nonEmpty).filterNot(_.startsWith("\\"))

  private def stripComments(content: String): String =
    content.linesIterator
      .map { line =>
        val c = line.indexOf('%')
        if c >= 0 then line.substring(0, c) else line
      }
      .mkString("\n")

  private val InputCommand = raw"\\input\s+(\S+)".r
  private val MacroDef     = raw"\\e?def\\([a-zA-Z]+)#1\{".r

  private def inputs(text: String): List[String] = InputCommand.findAllMatchIn(text).map(_.group(1)).toList

  /** The contents of every `\command{…}` group in `text`, each read to the brace that closes it. */
  private def blocks(text: String, command: String): List[String] =
    val opener = "\\" + command + "{"
    val out    = mutable.ListBuffer[String]()
    var i      = text.indexOf(opener)

    while i >= 0 do
      val start = i + opener.length

      balanced(text, start) match
        case Some(body) =>
          out += body
          i = text.indexOf(opener, start + body.length)
        case None =>
          out += text.substring(start)
          i = -1

    out.toList

  /** The text from `start` up to the `}` that closes the group already open there, or None if it is never
    * closed. */
  private def balanced(text: String, start: Int): Option[String] =
    var depth = 1
    var i     = start

    while i < text.length && depth > 0 do
      text(i) match
        case '{' => depth += 1
        case '}' => depth -= 1
        case _   => ()
      i += 1

    Option.when(depth == 0)(text.substring(start, i - 1))

  /** The one-argument macros a file defines for itself, by name and body — `\def\ver#1{#1as. #1i. …}`. `\edef`
    * differs from `\def` only in when TeX expands the body, which for a table that is read once and never
    * redefined comes to the same thing. */
  private def macroDefinitions(text: String): Map[String, String] =
    MacroDef
      .findAllMatchIn(text)
      .flatMap(m => balanced(text, m.end).map(body => m.group(1) -> body))
      .toMap

  /** Expand every call of a defined macro, repeatedly, since one body may call another — Esperanto's `\nom` is
    * written in terms of its `\adj`. The depth is bounded so that a file defining a macro in terms of itself
    * cannot spin. */
  private def expand(text: String, defs: Map[String, String]): String =
    if defs.isEmpty then text
    else
      var out     = text
      var round   = 0
      var changed = true

      while changed && round < 8 do
        changed = false
        val sb = new StringBuilder
        var i  = 0

        while i < out.length do
          val call = if out(i) == '\\' then defs.keysIterator.find(n => out.startsWith("\\" + n + "{", i)) else None

          call.flatMap(n => balanced(out, i + n.length + 2).map(n -> _)) match
            case Some((name, arg)) =>
              sb ++= defs(name).replace("#1", arg)
              i += name.length + arg.length + 3
              changed = true
            case None =>
              sb += out(i)
              i += 1

        out = sb.toString
        round += 1

      out

  /** The language's hyphenation minima, from the YAML-ish block every `hyph-utf8` file carries in its leading
    * comments:
    * {{{
    * % hyphenmins:
    * %     typesetting:
    * %         left: 2
    * %         right: 3
    * }}}
    * A file may give minima for *generating* the patterns and for *typesetting* with them, and where it gives
    * both they differ — the generation values are what the pattern author fed the pattern generator, and the
    * typesetting values are what documents are meant to use. Taking the wrong pair sets breaks the language does
    * not want, so typesetting wins wherever it is stated. */
  private def hyphenMins(content: String): Option[(Int, Int)] =
    val comments = content.linesIterator.takeWhile(_.startsWith("%")).map(_.drop(1)).toVector
    val start    = comments.indexWhere(_.trim == "hyphenmins:")

    if start < 0 then None
    else
      val region = comments.drop(start + 1).takeWhile(indent(_) > 1)
      val block =
        subBlock(region, "typesetting:") orElse subBlock(region, "generation:") getOrElse region

      for
        l <- number(block, "left:")
        r <- number(block, "right:")
      yield (math.max(1, l), math.max(1, r))

  private def indent(line: String): Int = line.length - line.dropWhile(_ == ' ').length

  private def subBlock(region: Vector[String], key: String): Option[Vector[String]] =
    val at = region.indexWhere(_.trim == key)

    Option.when(at >= 0) {
      val depth = indent(region(at))

      region.drop(at + 1).takeWhile(indent(_) > depth)
    }

  /** The integer after `key`, ignoring anything a file writes after it — one of them annotates its value with a
    * remark about the other value it might have used. */
  private def number(block: Vector[String], key: String): Option[Int] =
    block
      .find(_.trim.startsWith(key))
      .map(_.trim.drop(key.length).trim.takeWhile(_.isDigit))
      .filter(_.nonEmpty)
      .map(_.toInt)

  /** Create from TeX pattern file content (e.g. fetched or read by the caller). */
  def fromString(content: String): LiangHyphenation = fromSource(parse(content))

  /** Create from an already-read pattern file, which is how a loader that had to resolve the file's `\input`
    * itself hands over what it read. */
  def fromSource(source: PatternFile): LiangHyphenation =
    new LiangHyphenation(source.patterns, source.exceptions, source.minLeft, source.minRight)

  /** Load patterns from a file path. An `\input` in the file resolves beside it, which is how Norwegian's two
    * exception lists find the pattern set they share. */
  def fromFile(path: String): LiangHyphenation =
    fromSource(parse(io.github.edadma.cross_platform.readFile(path), beside(path)))

  /** Reads a file named by an `\input` from the directory of the file that asked for it. */
  private[texish] def beside(path: String): String => Option[String] =
    name =>
      try
        val file = io.github.edadma.path.Path(path).parent.map(_ / name).getOrElse(io.github.edadma.path.Path(name))

        Option.when(file.exists)(file.readText())
      catch case _: Throwable => None

  /** Create from individual pattern strings. */
  def fromPatterns(patterns: String*): LiangHyphenation =
    new LiangHyphenation(patterns.map(parsePattern).toMap, Map.empty)

  /** Create from pre-parsed patterns. */
  def apply(patterns: Map[String, IndexedSeq[Int]]): LiangHyphenation =
    new LiangHyphenation(patterns, Map.empty)
