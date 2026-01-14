package io.github.edadma.typesetter

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

  /** Check if hyphenation is enabled. */
  def isEnabled: Boolean = currentLanguage.isDefined

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
