package io.github.edadma.texish

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.collection.mutable

import io.github.edadma.highlighter.{Highlighter, Theme}
import io.github.edadma.path.Path
import io.github.edadma.texish.parser.PlatformEnv

/** How a code listing sets one token category: its ink, and whether it takes the code face's bold cut and its
  * italic cut. A colour theme only ever sets the ink; a theme meant for a black-and-white page leans on weight and
  * slope instead, because there every hue prints as a grey. */
final case class CodeStyle(color: Color, bold: Boolean = false, italic: Boolean = false)

/** One run of a highlighted source line: its text and the style it is set in. */
final case class CodeRun(text: String, style: CodeStyle):
  def color: Color = style.color

/** The bridge from the `highlighter` library to the engine's `\code` listing: resolve a language to a TextMate
  * grammar (a file on disk, else the bundled set), tokenize source through it, and turn each token into a styled run
  * via a theme. The layout — stacking the runs into line boxes — is the typesetter's job; this object only produces
  * the runs, one list per source line.
  */
object CodeHighlight:

  /** A code theme: the style for each of the highlighter's token categories (`keyword`, `string`, `comment`,
    * `number`, `type`, `function`, `variable`, `operator`, `punctuation`; any other category, the empty one
    * included, is plain text). */
  type CodeTheme = String => CodeStyle

  /** The highlighter's colour themes, by the name a document selects them with. The light themes read on a white
    * page (their default ink is dark); the dark themes suit a dark-background preview. Every category is set in the
    * regular cut; only the ink varies. */
  val colourThemes: Map[String, Theme] = Map(
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

  /** The lightest grey (as a fraction of white) the print theme may use: anything paler breaks up in black ink. */
  val printLightnessLimit: Double = 0.45

  /** The theme for a book printed in black ink only, where a colour theme's hues all collapse into greys that a
    * reader cannot tell apart. It leans on what black ink can still say: weight, slope, grey, and the delimiters
    * the source already carries.
    *
    *   - a keyword is bold black, so a listing's structure stands out the way it does in a printed language manual;
    *   - a comment is italic and mid-grey — two signals, so it is still unmistakable on an installation whose
    *     JetBrains Mono italic is absent and the cut falls back to upright;
    *   - a string is regular dark grey, set off further by its own quotes;
    *   - everything else — identifiers, numbers, types, operators, punctuation — is regular black. More greys
    *     would not survive the press, and black is what reads best at a listing's small size.
    *
    * The lighter grey is `#666666` (40% lightness, a 60% tint of black), dark enough to hold its strokes on an
    * uncoated black-and-white page; no grey of this theme is lighter than [[printLightnessLimit]]. */
  val printTheme: CodeTheme =
    val black      = Color("#000000")
    val stringGrey = Color("#4d4d4d")
    val remarkGrey = Color("#666666")

    {
      case "keyword" => CodeStyle(black, bold = true)
      case "comment" => CodeStyle(remarkGrey, italic = true)
      case "string"  => CodeStyle(stringGrey)
      case _         => CodeStyle(black)
    }

  /** The named themes a document may select with `\set codetheme {…}`: every colour theme, plus `print` for a
    * black-and-white interior. */
  val themes: Map[String, CodeTheme] =
    colourThemes.map((name, theme) => name -> ((category: String) => CodeStyle(Color(categoryColor(theme, category))))) +
      ("print" -> printTheme)

  /** The theme a listing is set in when `codetheme` is unset or names no theme. */
  val defaultTheme: String = "onelight"

  /** The languages a grammar is bundled for, sorted — used in the error message for an unknown language. */
  def availableLanguages: Seq[String] = EmbeddedGrammars.sources.keys.toSeq.sorted

  // Compiling a grammar is not cheap, so each grammar's Highlighter (or its load error) is built once and reused,
  // keyed by where the grammar came from: a file on disk by its path, a bundled one by its language name.
  private val cache = mutable.Map[String, Either[String, Highlighter]]()

  /** The grammar file for a language in the folders this installation searches, if one of them has it. The
    * search is the one `\usehyphenation` makes for a pattern file: the directory of the file asking, the current
    * directory, then a `grammars/` folder under [[Typesetter.home]], under `$TEXISHHOME`, and under the current
    * directory. Guarded because a host with no working filesystem reaches for platform APIs that may be absent;
    * there nothing is found and the bundled grammars are all there is. */
  def grammarFile(lang: String, from: String = "."): Option[Path] =
    try
      val fileName = s"$lang.tmLanguage.json"
      val home     = Option(Typesetter.home).filter(_.nonEmpty)
      val envHome  = PlatformEnv.get("TEXISHHOME").filter(_.nonEmpty)
      val roots =
        List(
          Some(Path(from)),
          Some(Path(".")),
          home.map(h => Path(h) / "grammars"),
          envHome.map(h => Path(h) / "grammars"),
          Some(Path(".") / "grammars"),
        ).flatten

      roots.map(_ / fileName).find(p => p.exists && p.isFile)
    catch case _: Throwable => None

  /** A highlighter for a language, or a Left explaining why none could be built. A grammar file found on disk by
    * [[grammarFile]] shadows the bundled grammar of the same name, so a document can carry the grammar for a
    * language texish does not bundle — or a newer one for a language it does — beside itself. */
  def forLanguage(lang: String, from: String = "."): Either[String, Highlighter] =
    grammarFile(lang, from) match
      case Some(file) =>
        val name = file.toPlatformString
        cache.getOrElseUpdate(
          name,
          try
            Highlighter
              .fromJson(new String(Files.readAllBytes(Paths.get(name)), StandardCharsets.UTF_8))
              .left
              .map(msg => s"grammar file '$name': $msg")
          catch case e: Exception => Left(s"cannot read grammar file '$name': ${e.getMessage}"),
        )
      case None =>
        cache.getOrElseUpdate(
          lang,
          EmbeddedGrammars.sources.get(lang).map(_.mkString) match
            case Some(json) => Highlighter.fromJson(json)
            case None =>
              Left(
                s"no grammar bundled for code language '$lang' (have: ${availableLanguages.mkString(", ")}); " +
                  s"a grammar file '$lang.tmLanguage.json' beside the document supplies one",
              ),
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

  /** Tokenize `code` and style it: one list of runs per source line, each run styled by its token's category under
    * `theme`. Concatenating a line's run texts reproduces that source line. */
  def styledLines(hl: Highlighter, theme: CodeTheme, code: String): Seq[Seq[CodeRun]] =
    hl.tokens(code).map(line => line.map(tok => CodeRun(tok.text, theme(hl.categoryOf(tok)))))
