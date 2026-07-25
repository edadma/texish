package io.github.edadma.texish

import scala.collection.mutable

/**
 * Liang's hyphenation algorithm (as used in TeX).
 *
 * The algorithm:
 * 1. Wrap word with '.' markers: "hyphen" -> ".hyphen."
 * 2. Find all matching patterns and their numeric values
 * 3. Take the maximum value at each position
 * 4. Odd values indicate valid hyphenation points
 */
class LiangHyphenation private (patterns: Map[String, IndexedSeq[Int]], minLeft: Int = 2, minRight: Int = 2):

  /** Find all valid hyphenation points in a word. */
  def hyphenate(word: String): IndexedSeq[Int] =
    // Hyphenate only the alphabetic core: leading and trailing non-letters (an opening parenthesis or quote, a
    // trailing period, the digits and colons of a reference) are not part of the word and must not count toward
    // break positions — otherwise "(Rom." breaks as "(R-om." and the abbreviation "Cor." as "Cor-.". Strip them,
    // hyphenate the core, and shift the points back so they index into the original word.
    val first = word.indexWhere(_.isLetter)
    if first < 0 then return IndexedSeq.empty
    val core = word.substring(first, word.lastIndexWhere(_.isLetter) + 1)
    if core.length < minLeft + minRight then return IndexedSeq.empty

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

    // Odd values = hyphenation points, adjusted for the '.' marker and the stripped leading affix
    (for
      i <- 2 until (values.length - 2)
      if values(i) % 2 == 1
      charIdx = i - 2
      if charIdx >= minLeft - 1 && charIdx < core.length - minRight
    yield charIdx + first).toIndexedSeq

  /**
   * Iterator of (before-with-hyphen, after) pairs.
   * Compatible with existing Hyphenation.apply signature.
   */
  def apply(word: String): Option[Iterator[(String, String)]] =
    val points = hyphenate(word)
    Option.when(points.nonEmpty) {
      points.iterator.map { idx =>
        (word.substring(0, idx + 1) + "-", word.substring(idx + 1))
      }
    }

object LiangHyphenation:

  /** Parse TeX pattern like "hy3ph" into ("hyph", IndexedSeq(0,0,3,0,0)) */
  def parsePattern(pattern: String): (String, IndexedSeq[Int]) =
    val letters = new StringBuilder
    val values = mutable.ArrayBuffer[Int]()

    for c <- pattern do
      if c.isDigit then values += (c - '0')
      else
        if values.length == letters.length then values += 0
        letters += c

    if values.length == letters.length then values += 0
    (letters.toString, values.toIndexedSeq)

  /** Parse TeX pattern file content.
    *
    * Comments are stripped first, and a TeX comment runs from its `%` to the end of the line — not merely to
    * the next space. Dropping only the tokens that begin with one leaves the prose of a comment behind to be
    * read as patterns: the French file heads its table with `% phonetic patterns % etymological patterns %`,
    * which contributed `phonetic`, `patterns` and `etymological` and broke words wherever they matched. The
    * English file is almost uncommented, which is why it alone came out right.
    *
    * The `\patterns{…}` block is then taken up to its closing brace. That search must also come after the
    * comments are gone, since a brace inside one would end the block early and lose every pattern after it.
    */
  def parsePatterns(content: String): Map[String, IndexedSeq[Int]] =
    val text = content.linesIterator
      .map { line =>
        val c = line.indexOf('%')
        if c >= 0 then line.substring(0, c) else line
      }
      .mkString("\n")

    val block =
      val start = text.indexOf("\\patterns{")
      if start >= 0 then
        val s = start + "\\patterns{".length
        val e = text.indexOf("}", s)
        if e >= 0 then text.substring(s, e) else text
      else text

    block
      .split("\\s+")
      .iterator
      .filter(_.nonEmpty)
      .filterNot(_.startsWith("\\"))
      .map(parsePattern)
      .toMap

  /** Create from TeX pattern file content (e.g. fetched or read by the caller). */
  def fromString(content: String): LiangHyphenation =
    new LiangHyphenation(parsePatterns(content))

  /** Load patterns from a file path. */
  def fromFile(path: String): LiangHyphenation =
    fromString(io.github.edadma.cross_platform.readFile(path))

  /** Create from individual pattern strings. */
  def fromPatterns(patterns: String*): LiangHyphenation =
    new LiangHyphenation(patterns.map(parsePattern).toMap)

  /** Create from pre-parsed patterns. */
  def apply(patterns: Map[String, IndexedSeq[Int]]): LiangHyphenation =
    new LiangHyphenation(patterns)
