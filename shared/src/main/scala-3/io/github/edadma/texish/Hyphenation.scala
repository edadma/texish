package io.github.edadma.texish

object Hyphenation:
  private var hyphenators: Map[String, LiangHyphenation] = Map.empty
  private var currentLanguage: Option[String] = None

  /** Load hyphenation patterns for a named language. */
  def loadPatterns(language: String, path: String): Unit =
    hyphenators += (language -> LiangHyphenation.fromFile(path))
    if currentLanguage.isEmpty then currentLanguage = Some(language)

  /** Load hyphenation patterns from content string for a named language. */
  def loadPatternsFromString(language: String, content: String): Unit =
    hyphenators += (language -> LiangHyphenation(LiangHyphenation.parsePatterns(content)))
    if currentLanguage.isEmpty then currentLanguage = Some(language)

  /** Set the active language for hyphenation. */
  def setLanguage(language: String): Unit =
    if hyphenators.contains(language) then currentLanguage = Some(language)
    else sys.error(s"No hyphenation patterns loaded for language: $language")

  /** Get the current language. */
  def getLanguage: Option[String] = currentLanguage

  /** Get available languages. */
  def languages: Set[String] = hyphenators.keySet

  /** Clear all hyphenators. */
  def clear(): Unit =
    hyphenators = Map.empty
    currentLanguage = None
    embeddedLoaded = Set.empty

  /** Check if hyphenation is enabled. */
  def isEnabled: Boolean = currentLanguage.isDefined

  // Embedded-tag bookkeeping: each pattern set compiled into the binary is parsed at most once.
  private var embeddedLoaded = Set.empty[String]

  /** The language tags whose TeX patterns are compiled into the binary (en-us, es, fr, …), so they
    * work on a native target with no pattern file to read. */
  def embeddedLanguages: Set[String] = EmbeddedHyphenationPatterns.byTag.keySet

  /** Load the embedded patterns for a language tag, the way a TeX format loads `hyphen.tex` — without
    * any patterns loaded, long words never break and a paragraph stretches its spaces instead. Returns
    * false when no patterns for `tag` are bundled (the caller can fall back to `\loadhyphenation`).
    * Idempotent: a tag's patterns parse at most once. Does not change the active language. */
  def enableEmbedded(tag: String): Boolean =
    EmbeddedHyphenationPatterns.byTag.get(tag) match
      case Some(content) =>
        if !embeddedLoaded(tag) then
          loadPatternsFromString(tag, content)
          embeddedLoaded += tag
        true
      case None => false

  /** Enable the embedded TeX en-US patterns and make them the active language. */
  def enableEnglish(): Unit =
    if enableEmbedded("en-us") then setLanguage("en-us")

  /** Get hyphenation points for a word using the current language. */
  def apply(word: String): Option[Iterator[(String, String)]] =
    for
      lang <- currentLanguage
      hyph <- hyphenators.get(lang)
      result <- hyph(word)
    yield result

  // Backwards compatibility: load patterns without language name (uses "default")
  def loadPatterns(path: String): Unit = loadPatterns("default", path)
  def loadPatternsFromString(content: String): Unit = loadPatternsFromString("default", content)
  def setHyphenator(h: LiangHyphenation): Unit =
    hyphenators += ("default" -> h)
    if currentLanguage.isEmpty then currentLanguage = Some("default")
