package io.github.edadma.texish

/** The hyphenation pattern cache: loaded Liang pattern sets, keyed by language tag, plus a pure lookup that
  * hyphenates a word in a named language. This object holds *only* the patterns, which are immutable once
  * parsed and expensive to build, so they are shared across every document and loaded at most once per tag.
  *
  * The *active* language is NOT held here — it is per-document state on the [[Typesetter]]
  * (`language`). Keeping selection out of this singleton is what lets two documents, or two
  * concurrent test suites, choose different languages (or none) without affecting each other's line breaking.
  *
  * The cache is append-only and thread-safe: writes are serialised, and the map is published through a
  * `@volatile` reference so a lookup always reads a consistent immutable snapshot without locking the hot path.
  */
object Hyphenation:
  @volatile private var hyphenators: Map[String, LiangHyphenation] = Map.empty
  private var embeddedLoaded                                       = Set.empty[String]

  private def store(language: String, h: LiangHyphenation): Unit =
    synchronized { hyphenators = hyphenators + (language -> h) }

  /** Load hyphenation patterns for a named language from a pattern file. */
  def loadPatterns(language: String, path: String): Unit =
    store(language, LiangHyphenation.fromFile(path))

  /** Load hyphenation patterns for a named language from a pattern string. */
  def loadPatternsFromString(language: String, content: String): Unit =
    store(language, LiangHyphenation(LiangHyphenation.parsePatterns(content)))

  /** Whether patterns for `language` are loaded. */
  def isLoaded(language: String): Boolean = hyphenators.contains(language)

  /** The languages whose patterns are currently loaded. */
  def languages: Set[String] = hyphenators.keySet

  // Embedded-tag bookkeeping: each pattern set compiled into the binary is parsed at most once.

  /** The language tags whose TeX patterns are compiled into the binary (en-us, es, fr, …), so they
    * work on a native target with no pattern file to read. */
  def embeddedLanguages: Set[String] = EmbeddedHyphenationPatterns.byTag.keySet

  /** Load the embedded patterns for a language tag, the way a TeX format loads `hyphen.tex` — without
    * any patterns loaded, long words never break and a paragraph stretches its spaces instead. Returns
    * false when no patterns for `tag` are bundled (the caller can fall back to `\loadhyphenation`).
    * Idempotent: a tag's patterns parse at most once. Does not select the language for any document. */
  def enableEmbedded(tag: String): Boolean =
    EmbeddedHyphenationPatterns.byTag.get(tag) match
      case Some(content) =>
        synchronized {
          if !embeddedLoaded(tag) then
            loadPatternsFromString(tag, content)
            embeddedLoaded += tag
        }
        true
      case None => false

  /** Hyphenation points for `word` in `language`, or `None` when no language is given, its patterns are not
    * loaded, or the word has no break points. The language is supplied by the caller (the document's active
    * language), so this lookup is pure with respect to the cache. */
  def apply(language: Option[String], word: String): Option[Iterator[(String, String)]] =
    for
      lang   <- language
      hyph   <- hyphenators.get(lang)
      result <- hyph(word)
    yield result

  // Load patterns under the conventional "default" name, for callers that do not name a language.
  def loadPatterns(path: String): Unit              = loadPatterns("default", path)
  def loadPatternsFromString(content: String): Unit = loadPatternsFromString("default", content)
  def setHyphenator(h: LiangHyphenation): Unit       = store("default", h)
