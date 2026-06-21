package io.github.edadma.texish

import scala.collection.mutable

import io.github.edadma.highlighter.{Highlighter, Theme}

/** The bridge from the `highlighter` library to the engine's `\code` listing: resolve a language to a TextMate
  * grammar (from the bundled set), tokenize source through it, and turn each token into a coloured run via a
  * theme. The layout — stacking the coloured runs into line boxes — is the typesetter's job; this object only
  * produces the `(text, colour)` runs, one list per source line.
  */
object CodeHighlight:

  /** The named colour themes a document may select with `\set codetheme {…}`. The light themes read on a white
    * page (their default ink is dark); the dark themes suit a dark-background preview. */
  val themes: Map[String, Theme] = Map(
    "onedark"          -> Theme.OneDark,
    "onelight"         -> Theme.OneLight,
    "monokai"          -> Theme.Monokai,
    "dracula"          -> Theme.Dracula,
    "solarized-dark"   -> Theme.SolarizedDark,
    "solarized-light"  -> Theme.SolarizedLight,
    "github-dark"      -> Theme.GitHubDark,
    "github-light"     -> Theme.GitHubLight,
    "nord"             -> Theme.Nord,
    "gruvbox-dark"     -> Theme.GruvboxDark,
    "gruvbox-light"    -> Theme.GruvboxLight,
    "tokyo-night"      -> Theme.TokyoNight,
    "catppuccin-mocha" -> Theme.CatppuccinMocha,
  )

  /** The languages a grammar is bundled for, sorted — used in the error message for an unknown language. */
  def availableLanguages: Seq[String] = EmbeddedGrammars.sources.keys.toSeq.sorted

  // Compiling a grammar is not cheap, so each language's Highlighter (or its load error) is built once and reused.
  private val cache = mutable.Map[String, Either[String, Highlighter]]()

  /** A highlighter for a bundled language, or a Left explaining why none could be built. Only the embedded
    * grammars are consulted; loading a document-supplied grammar file from disk is a later refinement. */
  def forLanguage(lang: String): Either[String, Highlighter] =
    cache.getOrElseUpdate(
      lang,
      EmbeddedGrammars.sources.get(lang).map(_.mkString) match
        case Some(json) => Highlighter.fromJson(json)
        case None =>
          Left(s"no grammar bundled for code language '$lang' (have: ${availableLanguages.mkString(", ")})")
    )

  /** The theme colour for one of the highlighter's nine token categories (anything else is the theme default). */
  private def categoryColor(theme: Theme, category: String): String = category match
    case "keyword"     => theme.keyword
    case "string"      => theme.string
    case "comment"     => theme.comment
    case "number"      => theme.number
    case "type"        => theme.`type`
    case "function"    => theme.function
    case "variable"    => theme.variable
    case "operator"    => theme.operator
    case "punctuation" => theme.punctuation
    case _             => theme.default

  /** Tokenize `code` and colour it: one list of `(text, colour)` runs per source line, with each run coloured by
    * its token's category under `theme`. Concatenating a line's run texts reproduces that source line. */
  def styledLines(hl: Highlighter, theme: Theme, code: String): Seq[Seq[(String, Color)]] =
    hl.tokens(code).map(line => line.map(tok => (tok.text, Color(categoryColor(theme, hl.categoryOf(tok))))))
