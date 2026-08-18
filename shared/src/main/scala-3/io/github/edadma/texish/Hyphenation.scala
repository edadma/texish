package io.github.edadma.texish

import io.github.edadma.path.Path
import io.github.edadma.texish.parser.PlatformEnv

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
    store(language, LiangHyphenation.fromString(content))

  /** Whether patterns for `language` are loaded. */
  def isLoaded(language: String): Boolean = hyphenators.contains(language)

  /** The languages whose patterns are currently loaded. */
  def languages: Set[String] = hyphenators.keySet

  // Embedded-tag bookkeeping: each pattern set compiled into the binary is parsed at most once.

  /** The language tags whose TeX patterns are compiled into the binary (en-us, es, fr, …), so they
    * work on a native target with no pattern file to read. */
  def embeddedLanguages: Set[String] = EmbeddedHyphenationPatterns.byTag.keySet

  /** Every language tag texish ships patterns for, whether or not this installation can reach them. The five in
    * [[embeddedLanguages]] are compiled in; the rest are files in a `hyphenation/` folder, and a build that
    * shipped no such folder still knows their names — which is what lets naming one say that the folder is
    * missing rather than that the language is not a language. Generated from the folder itself, so the two
    * cannot drift. */
  def bundledLanguages: Set[String] = EmbeddedHyphenationPatterns.bundledTags

  /** Load the patterns for a language tag from wherever this installation keeps them, and say whether that
    * worked. This is what `\usehyphenation` calls.
    *
    * The search is the one `\use` does for a module, for the same reasons and in the same order: the directory
    * of the file asking, the current directory, then a `hyphenation/` folder under [[Typesetter.home]], under
    * `$TEXISHHOME`, and under the current directory. A file on disk therefore shadows the compiled-in copy of
    * the same language, so an installation can carry newer patterns than the build was made with, and a document
    * can carry its own beside itself.
    *
    * Idempotent, and cheap to call again: a tag already loaded is not looked for, let alone parsed, a second
    * time. Loading is global — the patterns of a language are the same patterns for every document — while
    * *choosing* a language stays per-document, on the typesetter.
    */
  def enable(tag: String, from: String = "."): Boolean =
    isLoaded(tag) || enableFromTree(tag, from) || enableEmbedded(tag)

  /** The pattern file for a tag in the folders this installation searches, if one of them has it. Guarded
    * because a host with no working filesystem reaches for platform APIs that may be absent; there nothing is
    * found and the compiled-in patterns are all there is, which is exactly right. */
  def patternFile(tag: String, from: String = "."): Option[Path] =
    try
      val fileName = s"hyph-$tag.tex"
      val home     = Option(Typesetter.home).filter(_.nonEmpty)
      val envHome  = PlatformEnv.get("TEXISHHOME").filter(_.nonEmpty)
      val roots =
        List(
          Some(Path(from)),
          Some(Path(".")),
          home.map(h => Path(h) / "hyphenation"),
          envHome.map(h => Path(h) / "hyphenation"),
          Some(Path(".") / "hyphenation"),
        ).flatten

      roots.map(_ / fileName).find(p => p.exists && p.isFile)
    catch case _: Throwable => None

  /** Why a language tag did not load, when the engine can say something better than "not bundled": that the
    * name is one texish ships patterns for and the folder holding them is what is missing. The two readings are
    * fixed in entirely different places — one is a broken installation, the other a misspelled tag — so they are
    * worth telling apart, and this says which. */
  def unavailableNote(tag: String): String =
    if bundledLanguages(tag) then
      s"texish bundles patterns for '$tag', but no hyphenation folder was found — this installation is missing " +
        s"its hyphenation/ tree; \\loadhyphenation{$tag}{path} reads a pattern file directly"
    else
      s"texish bundles no patterns for '$tag' (it bundles ${bundledLanguages.size} languages, of which " +
        s"${embeddedLanguages.toSeq.sorted.mkString(", ")} need no files on disk); use " +
        s"\\loadhyphenation{$tag}{path} for a pattern file of your own"

  private def enableFromTree(tag: String, from: String): Boolean =
    patternFile(tag, from) match
      case Some(file) =>
        // An \input in the file resolves beside it, so Norwegian's two exception lists find the pattern set they
        // share in the same folder they were found in.
        store(tag, LiangHyphenation.fromFile(file.toPlatformString))
        true
      case None => false

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
