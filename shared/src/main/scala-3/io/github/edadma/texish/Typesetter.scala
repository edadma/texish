package io.github.edadma.texish

//import pprint.pprintln

import io.github.edadma.path.Path
import io.github.edadma.texish.opentype.{ByteCursor, Gpos, Gsub, IndicShaper, MathTable}
import io.github.edadma.texish.parser.{PlatformEnv, Value}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized
import scala.language.postfixOps

/** The geometry of a single rendered fragment cropped to its ink, in points: its `width` and `height`, and
  * `baseline` — the distance from the top of the cropped box down to the math baseline. An inline math element
  * is aligned to the surrounding text by setting its CSS `vertical-align` to `baseline - height` (pt), which
  * drops the element so its math baseline, not its bottom edge, sits on the text baseline. */
final case class FragmentMetrics(width: Double, height: Double, baseline: Double)

object Typesetter:

  /** Where a host keeps the `fonts/` folder, when it ships one. The engine names each bundled face by a relative
    * path that begins at that folder (`fonts/LatinModernRoman/lmroman10-regular.otf`), and this directory is one
    * of the roots such a path is resolved against — so an application embedding the engine can keep the fonts
    * wherever it likes and point the loader at their parent, without touching the environment.
    *
    * It is empty by default, and *nothing requires it to be set*: the core is compiled into the artifact (see
    * [[EmbeddedFonts]]), so the engine draws with no font tree at all. Setting it is the one-liner form of
    * `registerFontSource(DirectoryFontSource(dir))` — convenient for a host with a single tree, and read once
    * when a typesetter is constructed, so set it before constructing one. A host with more to say (several
    * folders, fonts that are not files at all) registers sources instead.
    *
    * Pointing the engine at a tree does not by itself load the wider bundled families — that is
    * [[Typesetter.loadBundledCatalogue]], which a host calls when it wants them. */
  var fontsDir: String = ""

  /** The families [[Typesetter.loadBundledCatalogue]] registers, named here so that they can be recognised
    * without having been loaded. That is what lets a document naming one get told the catalogue is not loaded
    * rather than that the family does not exist — the difference between "your installation is missing its font
    * tree" and "you misspelled a name", which are fixed in entirely different places.
    *
    * This is a second copy of what the catalogue's code says, kept honest by a test that compares it against the
    * families the catalogue actually attempts, so a family added there and forgotten here is caught at once.
    * The core families ([[Typesetter.loadCoreFonts]]) are deliberately absent: they are always present, so no
    * document can ever ask for one and be told it is missing.
    */
  val BundledFamilies: Set[String] = Set(
    "noto", "gentium", "charm", "thai", "nosifer", "rubik-wet-paint", "cinzel", "gentiumbook", "pt",
    "cjksc", "cjktc", "cjkjp", "japanese", "cjkkr", "korean",
    "hebrew", "ezra", "arabic", "amiri",
    "devanagari", "hindi", "bengali", "assamese", "gurmukhi", "punjabi", "telugu",
    "jetbrains", "alegreya", "ebgaramond", "petaluma",
  )

abstract class Typesetter:

  type ImageHandle

  /** The backend's loaded-but-unsized font: a typeface file in memory (java.awt.Font on JVM, cairo FontFace on
    * Native).
    */
  type FontFace

  /** The backend's sized font, ready to render and measure with (a derived java.awt.Font on JVM, cairo ScaledFont on
    * Native). Stored in [[Font.renderFont]].
    */
  type RenderFont

  val output: String

  var debug: Boolean           = false
  var currentFont: Font        = uninitialized
  var currentColor: Color      = Color("black")

  /** The colour the page is painted before any content is drawn. White by default — the colour of
    * paper, and what print output (PDF) expects. A screen preview may set it dark to render the
    * document in a dark scheme; the ink is the pen ([[currentColor]]), so pairing a dark background
    * with a light pen inverts the page while leaving any author-specified colours untouched. */
  var backgroundColor: Color   = Color("white")
  val converter                = UnitConverter(this)
  val pages                    = new ArrayBuffer[Any]
  var ligatures: Boolean       = true
  var representations: Boolean = true

  // The typeface consulted for codepoints the active font has no glyph for — the glyph-fallback face. A text run
  // is split so those codepoints are set from this face instead of drawing a missing-glyph box (see coverageSegments
  // and CharBox). None disables fallback; it is set once the fallback face is loaded.
  var fallbackTypeface: Option[String] = None

  // The families \mono (mono) and \sans (sans) resolve to when the current typeface carries no such member —
  // the way LaTeX's \ttdefault and \sfdefault name a typewriter and a sans family independent of the text family,
  // so \mono in a Garamond paragraph sets in the document's typewriter face rather than failing. Latin Modern
  // provides both members, so it is the default for each; a document may point these elsewhere.
  var monoDefaultTypeface: String = "lmroman"
  var sansDefaultTypeface: String = "lmroman"

  // A typeface's style variants each carry their own ligature set, because different cuts of one super-family run
  // different ligature programs — the roman body forms f-ligatures and the dash/quote text representations, while
  // the typewriter (mono) role sets code literally with none of them. So the map keys a style to both its font
  // face and the ligatures that face enables, rather than sharing one set across the whole typeface.
  case class Typeface(
      fonts: mutable.HashMap[Set[String], (FontFace, Set[String])],
      baseline: Option[Double],
  )

  /** Shipout-time page decoration: produces the running header and footer boxes (either may be null) for the page
    * being shipped. Evaluated once per page, after `pageno` is set to the shipping page's number, so material like
    * the page number is always current. The language layer installs one that builds the boxes from the `headline`
    * and `footline` macros.
    */
  var pageDecorator: (() => (Box | Null, Box | Null)) | Null = null

  protected[texish] var document: DocumentMode         = new DocumentMode(this)

  /** Cross-reference store for `\label`/`\ref`/`\tableofcontents`. The driver shares one table across the passes of
    * a run (assigning it here before each pass) so forward references resolve; left to its own per-typesetter
    * default it simply collects within a single pass, which is all a backward reference needs.
    */
  var references: ReferenceTable                        = new ReferenceTable

  protected val typefaces                                  = new mutable.HashMap[String, Typeface]
  private val scopes                                       = mutable.Stack[Map[String, Value]](Map.empty)
  protected[texish] val modeStack: mutable.Stack[Mode] = new mutable.Stack
  var indentParagraph: Boolean                             = false // todo: this should go into page mode maybe
  private var contentInitialized                           = false

  /** Whether page content has begun — the output surface, sized from the page arrangement, has been created. The
    * arrangement and page size are fixed at that point, so `\arrange` and a sheet-resizing `\geometry` are only
    * meaningful in the preamble, before this turns true.
    */
  def contentStarted: Boolean = contentInitialized

  protected def ensureInitializedForContent(): Unit = {
    if (!contentInitialized) {
      // Get dimensions from variables (with sensible defaults)
      val width =
        try {
          getNumber("paperwidth")
        } catch {
          case _: Exception => 8.5 * in
        }
      val height =
        try {
          getNumber("paperheight")
        } catch {
          case _: Exception => 11 * in
        }

      // The physical surface is sized by the page arrangement, not the logical page: a booklet lays two A6 pages on
      // one A5-landscape sheet, so the sheet is wider than a page. With the default one-page-per-sheet arrangement
      // the sheet is the page, and this is the plain paper size.
      val (sheetWidth, sheetHeight) = document.arrangement.sheetSize(width, height)

      init(sheetWidth, sheetHeight)
      contentInitialized = true
    }
  }

  def init(width: Double, height: Double): Unit

  def createPageTarget: Any

  def draw(box: Box, xoffset: Double = 0, yoffset: Double = 0): Unit = box.draw(this, xoffset, yoffset + box.ascent)

  /** The device-space y, in points, of the first body line's baseline on the page being shipped — recorded by
    * the page builder as it draws the body. NaN until a body has been drawn. A fragment renderer reads it back,
    * with the cropped page box, to align an inline formula on the surrounding text's baseline. */
  private var bodyBaselineY: Double = Double.NaN

  /** Record the first body line's baseline (device y, in points); the first value seen per render wins, so a
    * multi-line column reports the top line — the one an inline fragment is aligned by. */
  def recordBodyBaseline(y: Double): Unit = if bodyBaselineY.isNaN then bodyBaselineY = y

  /** The recorded first-line baseline (device y, points), or NaN if no body has been drawn. */
  def bodyBaseline: Double = bodyBaselineY

  /** For a single-fragment render — an inline or display formula cropped to its ink — the cropped box's size
    * and the distance from its top to the math baseline, all in points. None on a backend that does not crop,
    * or before a page has shipped. The in-browser math API uses [[FragmentMetrics.baseline]] to set an inline
    * element's `vertical-align` so the formula sits on the surrounding text's baseline. */
  def fragmentMetrics: Option[FragmentMetrics] = None

  def ejectPageTarget(): Unit

  def setFont(font: RenderFont): Unit

  def setColor(color: Color): Unit

  def setLineWidth(width: Double): Unit

  def drawString(text: String, x: Double, y: Double): Unit

  def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit

  def drawRect(x: Double, y: Double, width: Double, height: Double): Unit

  def fillRect(x: Double, y: Double, width: Double, height: Double): Unit

  /** Open a face from a font file on disk. `path` has already been resolved to something that exists. */
  def loadFont(path: String): FontFace

  /** Open a face from font-file bytes held in memory, for a face that has no file to open — the Latin Modern
    * core the artifact carries as base64 (see [[EmbeddedFonts]]), which is the whole of what a consumer gets
    * when there is no font tree on disk. `name` is the relative path the bytes were embedded under, for error
    * messages and for a backend that keys a cache by it.
    *
    * A backend whose font API reads lazily from the buffer it is handed (FreeType does) must keep the bytes
    * alive and unchanged for the lifetime of the face it returns. */
  def loadFontBytes(bytes: Array[Byte], name: String): FontFace

  def getTextExtents(text: String, font: RenderFont): TextExtents

  def makeFont(font: FontFace, size: Double): RenderFont

  def charWidth(font: RenderFont, c: Char): Double

  // Glyph seam — a parallel path to the string seam (drawString/getTextExtents) above, used where the
  // engine must place individual glyphs itself rather than hand the backend a string to shape: math
  // mode positions every glyph by hand. Text stays on the string path; the two never interact beyond
  // sharing a font. A glyph is named by its index in the font (an opaque per-backend handle), obtained
  // from a Unicode codepoint via glyphIndex.

  /** Map a Unicode codepoint to a glyph index in `font`. The result is valid only within this backend —
    * it is exactly what [[glyphExtents]] and [[drawGlyph]] consume. Returns 0 (the .notdef glyph) when the
    * font has no glyph for the codepoint.
    */
  def glyphIndex(font: RenderFont, codepoint: Int): Int

  /** Geometric extents of a single glyph, by index, reported in points (same convention as
    * [[getTextExtents]]: `yBearing` is negative for the part above the baseline).
    */
  def glyphExtents(font: RenderFont, glyph: Int): TextExtents

  /** Draw a single glyph, by index, at a baseline origin, in the current color. */
  def drawGlyph(font: RenderFont, glyph: Int, x: Double, y: Double): Unit

  /** The raw bytes of one SFNT table of `font`, by four-character tag (e.g. "MATH"), or `None` if the
    * font has no such table. This is how the math layer reaches the OpenType `MATH` table, which the
    * rasterizers expose no structured API for; see [[io.github.edadma.texish.opentype.MathTable]].
    */
  def sfntTable(font: RenderFont, tag: String): Option[Array[Byte]]

  // Parsed GPOS mark-positioning shapers, one per render font that has them. Parsing reads the GPOS, GDEF
  // and head bytes once and is cached, since a paragraph of pointed Hebrew draws many runs in the same font.
  private val markShaperCache = mutable.HashMap.empty[Any, Option[Gpos]]

  /** The mark-positioning shaper for `font`, or None when the font has no GPOS mark lookups (every Latin
    * text font) — in which case text draws through the plain advance-only path. Combining marks (Hebrew
    * niqqud, Arabic marks) carry zero advance and are positioned by the font's anchors, which this reads. */
  def markShaper(font: RenderFont): Option[Gpos] =
    markShaperCache.getOrElseUpdate(
      font, {
        val upem = sfntTable(font, "head") match
          case Some(b) if b.length >= 20 => ((b(18) & 0xff) << 8) | (b(19) & 0xff)
          case _                         => 1000
        Gpos.from(sfntTable(font, "GPOS"), sfntTable(font, "GDEF"), upem)
      },
    )

  // Parsed GSUB Arabic-form shapers, one per render font that carries the cursive-joining form features.
  // Like the mark shaper this is read once and cached, since an Arabic paragraph draws many runs in one font.
  private val gsubShaperCache = mutable.HashMap.empty[Any, Option[Gsub]]

  /** The Arabic-form substitution shaper for `font`, or None when the font has no `init`/`medi`/`fina`/`isol`
    * features (every non-Arabic font) — in which case Arabic text, if any, draws in its nominal (isolated)
    * glyphs through the plain path. The joining forms themselves are decided font-free in
    * [[io.github.edadma.texish.opentype.ArabicShaping]]; this turns a decided form into the shaped glyph. */
  def gsubShaper(font: RenderFont): Option[Gsub] =
    gsubShaperCache.getOrElseUpdate(font, Gsub.from(sfntTable(font, "GSUB"), sfntTable(font, "GDEF")))

  // Parsed small-capitals shapers, one per render font whose GSUB carries an `smcp` feature. Read once and
  // cached like the other shapers, since a small-caps run draws many boxes in one font.
  private val smcpShaperCache = mutable.HashMap.empty[Any, Option[Gsub]]

  /** The small-capitals shaper for `font`, or None when the font has no `smcp` feature (a dedicated small-caps
    * cut, or a face without small caps at all) — in which case a small-caps run sets its letters through the
    * plain path, either as the drawn small-caps cut or, lacking one, as the ordinary letters. This turns the
    * lowercase letters of an ordinary roman into its small capitals; see the small-caps path in [[CharBox]]. */
  def smcpShaper(font: RenderFont): Option[Gsub] =
    smcpShaperCache.getOrElseUpdate(font, Gsub.fromSmallCaps(sfntTable(font, "GSUB"), sfntTable(font, "GDEF")))

  // Parsed Hebrew shapers, one per render font whose GSUB combines a letter with the point drawn inside it.
  // Read once and cached like the other shapers, since a pointed Hebrew page draws many runs in one font.
  private val hebrewShaperCache = mutable.HashMap.empty[Any, Option[Gsub]]

  /** The pointed-Hebrew shaper for `font`, or None when the font places every point by anchor instead of
    * combining it into the letter — in which case Hebrew takes the plain path and the mark shaper positions
    * the points, which is what the Noto Hebrew face wants. See
    * [[io.github.edadma.texish.opentype.HebrewShaping]] for why a face like Ezra SIL needs the substitution. */
  def hebrewShaper(font: RenderFont): Option[Gsub] =
    hebrewShaperCache.getOrElseUpdate(font, Gsub.fromHebrew(sfntTable(font, "GSUB"), sfntTable(font, "GDEF")))

  // Parsed Indic shapers, one per render font whose GSUB carries an Indic script (Devanagari, Bengali, …).
  // Read once and cached like the other shapers, since an Indic paragraph draws many runs in the same font.
  private val indicShaperCache = mutable.HashMap.empty[Any, Option[IndicShaper]]

  /** The Indic shaper for `font`, bound to whichever Indic script the font carries, or None when the font has
    * no Indic script table (every non-Indic font) — in which case Indic text, if any, falls to the plain path
    * and shows the font's nominal glyphs. The clustering and pre-base reordering are font-free (see
    * [[io.github.edadma.texish.opentype.IndicScript]]); this drives the font's GSUB features around them. */
  def indicShaper(font: RenderFont): Option[IndicShaper] =
    indicShaperCache.getOrElseUpdate(font, IndicShaper.from(sfntTable(font, "GSUB"), sfntTable(font, "GDEF")))

  def loadImage(path: String): (ImageHandle, Int, Int)

  // Paint the image with its top-left at (x, y), scaled from its source pixels to fill w × h points.
  def drawImage(image: ImageHandle, x: Double, y: Double, w: Double, h: Double): Unit

  /** Whether this backend can build and paint raster images. The raster backends (Cairo, Graphics2D) override this
    * to true; a non-rendering backend (headless, the JS layer) leaves it false, so a document can fall back to a
    * drawn alternative — `\defbitmap` no-ops and the `imagessupported` variable reads 0. */
  def imagesSupported: Boolean = false

  /** Build an image from straight (non-premultiplied) ARGB pixels in row-major order, top row first, for an
    * inline bitmap. Backends that support images override this; the default refuses, so callers must gate on
    * [[imagesSupported]] first. */
  def createImage(width: Int, height: Int, argb: Array[Int]): ImageHandle =
    throw new UnsupportedOperationException("this backend cannot create images")

  // Hyperlink seam: a LinkBox brackets the drawing of its content with these so a backend that supports
  // annotations (the PDF output) can mark the drawn region as a link to `uri`. The default is a no-op, so
  // raster and headless backends simply draw the content with no annotation.
  def beginLink(uri: String): Unit = ()
  def endLink(): Unit              = ()

  // Picture-graphics seam — the path/transform/clip vocabulary a PictureBox replays to draw vector graphics
  // (paths, shapes, fills and strokes, placed labels) in a y-up coordinate system. It is separate from the
  // immediate drawLine/fillRect calls above: those draw one primitive in device space, whereas these build a
  // path, paint it with the current stroke parameters, and compose an affine transform on the backend's CTM —
  // which is what lets graphics rotate and scale, and lets placed text stay upright over a flipped picture.
  //
  // The default bodies are no-ops so backends without a raster surface (and test stubs) need not implement
  // them; the Cairo and Graphics2D backends override the whole set. Coordinates are in points, like the rest
  // of the seam. Angles are radians.

  /** Save the full graphics state (transform, clip, colour, and stroke parameters) for a later [[grestore]]. */
  def gsave(): Unit = ()

  /** Restore the graphics state saved by the matching [[gsave]]. */
  def grestore(): Unit = ()

  def translate(dx: Double, dy: Double): Unit = ()
  def scale(sx: Double, sy: Double): Unit     = ()
  def rotate(radians: Double): Unit           = ()

  /** Begin a fresh path, discarding any path under construction. */
  def newPath(): Unit = ()

  def moveTo(x: Double, y: Double): Unit = ()
  def lineTo(x: Double, y: Double): Unit = ()

  /** Add a cubic Bézier from the current point through `(c1x,c1y)` and `(c2x,c2y)` to `(x,y)`. */
  def curveTo(c1x: Double, c1y: Double, c2x: Double, c2y: Double, x: Double, y: Double): Unit = ()

  /** Add a circular arc of radius `r` about `(cx,cy)` from angle `a0` to `a1`; `negative` sweeps clockwise. */
  def arc(cx: Double, cy: Double, r: Double, a0: Double, a1: Double, negative: Boolean): Unit = ()

  def closePath(): Unit = ()

  /** Fill the current path with the current colour. When `preserve` is true the path is kept (so it can then
    * be stroked over the fill); otherwise it is cleared, like the underlying rasterizer's fill. */
  def fillPath(preserve: Boolean = false): Unit = ()

  /** Stroke the current path with the current colour, width, dash, caps and joins; clears the path. */
  def strokePath(): Unit = ()

  /** Intersect the clip region with the current path, for the rest of the enclosing [[gsave]]/[[grestore]]. */
  def clipPath(): Unit = ()

  /** Set the stroke dash pattern (alternating on/off lengths) with a starting phase; empty means solid. */
  def setDash(pattern: Seq[Double], offset: Double): Unit = ()

  def setLineCap(cap: LineCap): Unit    = ()
  def setLineJoin(join: LineJoin): Unit = ()

  def destroy(): Unit

  /** Release this pass's output without handing it back — for the throwaway intermediate passes of a multi-pass
    * run (see [[Passes]]), whose pages are discarded. The default is [[destroy]]; a backend that hands its
    * collected page surfaces to the caller (rather than freeing them in destroy) frees them here instead.
    */
  def discard(): Unit = destroy()

  def getDocument: DocumentMode = document

  /** Where this typesetter looks for font files, in order. Seeded with the roots that have always been searched
    * — the current directory, [[Typesetter.fontsDir]] if a host set one, and `$TEXISHHOME` — and extended by a
    * host through [[registerFontSource]]. The core embedded in the artifact is not in this list: it is consulted
    * after everything in it, so that a font tree carrying the same path shadows it.
    *
    * Declared before the core faces are loaded, because loading them reads it. */
  private val fontSources = ArrayBuffer[FontSource](DirectoryFontSource("."))

  if Typesetter.fontsDir.nonEmpty then fontSources += DirectoryFontSource(Typesetter.fontsDir)

  PlatformEnv.get("TEXISHHOME").filter(_.nonEmpty).foreach(home => fontSources += DirectoryFontSource(home))

  loadCoreFonts()

  /** Register the wider set of typefaces texish bundles — the complex-script faces, the CJK cuts, the
    * alternative text families. These are far too large to compile into the artifact (~151MB against the core's
    * ~6.1MB), so their files come from a font tree on disk, reached through a registered font source. Loading
    * them is the host's decision: the CLI calls this after locating the tree its installation ships, an
    * application does so if it has one to offer, and a program embedding texish as a library need never call it
    * at all and still gets a working engine from [[loadCoreFonts]] alone.
    *
    * Tolerant by design, because a font tree may be partial: a family whose files no source has is skipped and
    * remembered, so that naming it later fails with "texish bundles this, but its files were not found" rather
    * than the bare "not found" a typo earns. Call it before typesetting begins — a face registered mid-document
    * cannot retroactively set the text already laid out.
    */
  def loadBundledCatalogue(): Unit =
    loadingCatalogue = true
    try loadCatalogueFonts()
    finally loadingCatalogue = false

  protected def loadCatalogueFonts(): Unit = {
  loadTypeface(
    "noto",
    "fonts/NotoSerif/NotoSerif",
    "ﬃﬄﬁﬂ",
    Set(),
    "Regular",
    "Bold",
    "Italic",
    ("Bold", "Italic"),
  )
  loadTypeface(
    "noto",
    "fonts/NotoSans/NotoSans",
    "ﬃﬄﬁﬂﬀ",
    Set("sans"),
    "Black",
    ("Black", "Italic"),
    "Bold",
    ("Bold", "Italic"),
    "ExtraBold",
    ("ExtraBold", "Italic"),
    "ExtraLight",
    ("ExtraLight", "Italic"),
    "Italic",
    "Light",
    ("Light", "Italic"),
    "Medium",
    ("Medium", "Italic"),
    "Regular",
    "SemiBold",
    ("SemiBold", "Italic"),
    "Thin",
    ("Thin", "Italic"),
  )
  loadTypeface(
    "gentium",
    "fonts/GentiumPlus-6.200/GentiumPlus",
    "ﬃﬄﬁﬂﬀ",
    Set(),
    "Regular",
    "Bold",
    "Italic",
    ("Bold", "Italic"),
  )
  // Charm, a Thai face with a Latin companion, in a regular and a bold cut. Thai is written without spaces
  // between words and uses them to separate phrases instead, so its line breaking needs the per-character
  // pass (see Thai and KnuthPlass.appendCharacterBreaks); the script is left to right and needs no reordering.
  loadTypeface(
    "charm",
    "fonts/Charm/Charm",
    Ligatures.ALL,
    Set(),
    "Regular",
    "Bold",
  )
  aliasTypeface("thai", "charm")
  loadTypeface(
    "nosifer",
    "fonts/Nosifer/Nosifer",
    Ligatures.ALL,
    Set(),
    "Regular",
  )
  loadTypeface(
    "rubik-wet-paint",
    "fonts/Rubik_Wet_Paint/RubikWetPaint",
    Ligatures.ALL,
    Set(),
    "Regular",
  )

  // Cinzel — an all-capitals titling serif in the Trajan tradition, for display lines: chapter openings, covers,
  // running heads. It has no lowercase and no italic, so the styled cuts sit on the weight axis alone; \font
  // cinzel 24 sets the regular weight, and the heavier cuts are the free-axis tags \font cinzel 24 medium,
  // semibold, bold, extrabold, black. Only the text-representation ligatures (the curly quotes and dashes it
  // carries as punctuation) are enabled — the f-ligatures are lowercase forms this face does not have.
  loadTypeface(
    "cinzel",
    "fonts/Cinzel/Cinzel",
    Ligatures.TEXT_REPRESENTATIONS,
    Set(),
    "Regular",
    "Medium",
    "SemiBold",
    "Bold",
    "ExtraBold",
    "Black",
  )

  val gentiumbookMissing = Set(
    `LONG LEFT RIGHT ARROW`,
    `LONG LEFT RIGHT DOUBLE ARROW`,
    `LONG LEFTWARDS ARROW`,
    `LONG RIGHTWARDS ARROW`,
    `LONG LEFTWARDS DOUBLE ARROW`,
    `LONG RIGHTWARDS DOUBLE ARROW`,
    `LEFTWARDS DOUBLE ARROW`,
    `RIGHTWARDS DOUBLE ARROW`,
    `LEFT RIGHT DOUBLE ARROW`,
  )

  loadTypeface(
    "gentiumbook",
    "fonts/GentiumBookPlus/GentiumBookPlus",
    Ligatures.ALL diff gentiumbookMissing,
    Set(),
    "Regular",
    "Bold",
    "Italic",
    ("Bold", "Italic"),
  )
  overrideBaseline("gentium", 0.8)
  loadTypeface("pt", "fonts/PTSansNarrow/PTSansNarrow", "ﬁﬂ", Set(), "Regular", "Bold")

  // Noto Serif CJK, the bundled Chinese faces for the CJK line breaking the engine does on its own (see
  // CJK and KnuthPlass.appendCJKItems): \font cjksc 12 regular for Simplified, cjktc for Traditional, each
  // with a bold cut so \font cjksc 12 bold sets real bold rather than substituting the regular weight. They
  // are loaded by file rather than through loadTypeface, whose "-Style.ttf" naming and bundled-only weight
  // set do not fit these single hand-instanced cuts. At 74MB for the four regions these are far and away the
  // largest thing the catalogue carries, which is the plainest illustration of why it is not embedded.
  loadFont("cjksc", "fonts/NotoSerifCJK/NotoSerifSC-Regular.ttf", Set.empty, Set.empty)
  loadFont("cjktc", "fonts/NotoSerifCJK/NotoSerifTC-Regular.ttf", Set.empty, Set.empty)
  loadFont("cjksc", "fonts/NotoSerifCJK/NotoSerifSC-Bold.ttf", Set.empty, Set("bold"))
  loadFont("cjktc", "fonts/NotoSerifCJK/NotoSerifTC-Bold.ttf", Set.empty, Set("bold"))

  // The Japanese and Korean members of the same family. Han (kanji/hanja) has region-specific glyph forms —
  // the Japanese cut draws them the Japanese way rather than the Chinese, and the Korean cut carries the
  // Hangul syllables the Chinese cuts have no glyph for at all. \font cjkjp 12 (alias \font japanese) and
  // \font cjkkr 12 (alias \font korean), each with a bold cut. Japanese sets left to right with the same
  // kinsoku breaking as Chinese; Korean is written with interword spaces and breaks at them like Latin (see
  // CJK.isCJK), so its Hangul gets no free per-character break — only a heavily penalized last-resort one,
  // which keeps a run with no space in it from running off the page (see CJK.lastResortBetween).
  loadFont("cjkjp", "fonts/NotoSerifCJK/NotoSerifJP-Regular.otf", Set.empty, Set.empty)
  loadFont("cjkjp", "fonts/NotoSerifCJK/NotoSerifJP-Bold.otf", Set.empty, Set("bold"))
  aliasTypeface("japanese", "cjkjp")
  loadFont("cjkkr", "fonts/NotoSerifCJK/NotoSerifKR-Regular.otf", Set.empty, Set.empty)
  loadFont("cjkkr", "fonts/NotoSerifCJK/NotoSerifKR-Bold.otf", Set.empty, Set("bold"))
  aliasTypeface("korean", "cjkkr")

  // Noto Serif Hebrew, the bundled face for right-to-left Hebrew: \font hebrew 12 sets it, with a bold cut
  // so \font hebrew 12 bold is a real bold weight. These are static Regular and Bold instances drawn from
  // the variable upstream (wght 400 and 700 at default width). The reordering into visual order is the
  // engine's (see Bidi and ParagraphMode); the font supplies the glyphs and, for pointed text, the marks.
  // Loaded by file because the single instanced cuts do not fit loadTypeface's "-Style.ttf" naming.
  loadFont("hebrew", "fonts/NotoSerifHebrew/NotoSerifHebrew-Regular.ttf", Set.empty, Set.empty)
  loadFont("hebrew", "fonts/NotoSerifHebrew/NotoSerifHebrew-Bold.ttf", Set.empty, Set("bold"))

  // Ezra SIL, the scholarly Biblical-Hebrew face cut in the style of the Biblia Hebraica Stuttgartensia, for
  // pointed Hebrew set the way a critical edition sets it: \font ezra 12, with its heavier companion cut (Ezra
  // SIL SR) mapped to the bold slot so \font ezra 12 bold selects it. Right to left like the Noto Hebrew face,
  // so the engine reorders into visual order (see Bidi and ParagraphMode) and the font carries the consonants,
  // the vowel points and the cantillation marks, which it positions through its GPOS. Loaded by file, like the
  // Noto Hebrew cuts.
  loadFont("ezra", "fonts/EzraSIL/SILEOT.ttf", Set.empty, Set.empty)
  loadFont("ezra", "fonts/EzraSIL/SILEOTSR.ttf", Set.empty, Set("bold"))

  // Noto Naskh Arabic, the bundled face for right-to-left Arabic (and Persian, Urdu, … in the same script):
  // \font arabic 12 sets it, with a bold cut so \font arabic 12 bold is a real bold weight. These are static
  // Regular and Bold instances drawn from the variable upstream (wght 400 and 700). The Naskh style is the
  // book hand the joining engine targets; the cursive forms come from the font's GSUB, selected per letter by
  // the engine (see ArabicShaping and Gsub), and the bidi reordering into visual order is the engine's (see
  // Bidi and ParagraphMode). Loaded by file, like the Hebrew and CJK cuts.
  loadFont("arabic", "fonts/NotoNaskhArabic/NotoNaskhArabic-Regular.ttf", Set.empty, Set.empty)
  loadFont("arabic", "fonts/NotoNaskhArabic/NotoNaskhArabic-Bold.ttf", Set.empty, Set("bold"))

  // Amiri, a traditional Naskh book face in the Bulaq (Būlāq) tradition, an alternative to Noto Naskh for
  // right-to-left Arabic: \font amiri 12, with a real bold cut and slanted (italic) cuts for its Latin. The
  // cursive joining and the bidi reordering are the engine's exactly as for the Noto face — shaping keys off
  // the Arabic script of the text, not the typeface name — and the font supplies the joined forms and the many
  // classical ligatures through its GSUB. The "-Style.ttf" cuts fit loadTypeface; no ligature set is enabled,
  // as Arabic documents type their punctuation literally.
  loadTypeface(
    "amiri",
    "fonts/Amiri/Amiri",
    Set.empty[String],
    Set(),
    "Regular",
    "Bold",
    "Italic",
    ("Bold", "Italic"),
  )

  // Noto Serif Devanagari, the bundled face for Devanagari (Hindi, Marathi, …): \font devanagari 12 sets it,
  // or the alias \font hindi 12, each with a bold cut so \font devanagari 12 bold is a real bold weight. These
  // are static Regular and Bold instances drawn from the variable upstream (wght 400 and 700). Devanagari is
  // left to right, so no bidi is involved; the cluster segmentation and the pre-base short-i reordering are
  // the engine's (see Devanagari and IndicShaper), and the font supplies the conjunct, half-form and vowel-sign
  // glyphs through its GSUB and positions the marks through its GPOS. Loaded by file, like the Hebrew, Arabic
  // and CJK cuts. Devanagari sets its punctuation like a Latin text face — the same curly quotes, en/em dashes and ellipsis,
  // all of which this font carries — so it enables the text representations and the `` ``''/''`` `` and dash
  // shorthands become the proper typographic characters, just as they do in the roman body. (The right-to-left
  // Hebrew and Arabic faces do not: Hebrew's quotes are the mirrored pair and Arabic's are guillemets, so those
  // documents type the marks literally rather than through the ASCII shorthands.)
  loadFont("devanagari", "fonts/NotoSerifDevanagari/NotoSerifDevanagari-Regular.ttf", Ligatures.TEXT_REPRESENTATIONS, Set.empty)
  loadFont("devanagari", "fonts/NotoSerifDevanagari/NotoSerifDevanagari-Bold.ttf", Ligatures.TEXT_REPRESENTATIONS, Set("bold"))
  aliasTypeface("hindi", "devanagari")

  // Noto Serif Bengali, the bundled face for the Bengali–Assamese script: \font bengali 12 sets it, or the
  // alias \font assamese 12, each with a bold cut. Static Regular and Bold instances drawn from the variable
  // upstream (wght 400 and 700). Bengali is left to right; the engine does the cluster segmentation, the
  // pre-base i/e/ai reordering and the two-part o/au decomposition (see Bengali and IndicShaper), and the font
  // supplies the conjunct, ya-phalaa, reph and vowel-sign glyphs through its GSUB and positions the marks
  // through its GPOS. Loaded by file like the other complex-script cuts.
  // Like the Devanagari face it sets its punctuation as a Latin text face, so the curly-quote and dash
  // shorthands become proper typographic characters in a Bengali run.
  loadFont("bengali", "fonts/NotoSerifBengali/NotoSerifBengali-Regular.ttf", Ligatures.TEXT_REPRESENTATIONS, Set.empty)
  loadFont("bengali", "fonts/NotoSerifBengali/NotoSerifBengali-Bold.ttf", Ligatures.TEXT_REPRESENTATIONS, Set("bold"))
  aliasTypeface("assamese", "bengali")

  // Noto Serif Gurmukhi, the bundled face for Gurmukhi (Punjabi as written in India): \font gurmukhi 12 sets
  // it, or the alias \font punjabi 12, each with a bold cut. Gurmukhi is left to right; the engine does the
  // cluster segmentation and the pre-base sihari (i-sign) reordering (see Gurmukhi and IndicShaper), and the
  // font supplies the subjoined pairin, conjunct and vowel-sign glyphs through its GSUB and positions the
  // marks — tippi, addak, the below-base vowel signs — through its GPOS. Gurmukhi has no reph. Loaded by file
  // like the other complex-script cuts. Like the other Indic faces it sets its punctuation as a Latin text
  // face, so the curly-quote and dash shorthands come out typographic.
  loadFont("gurmukhi", "fonts/NotoSerifGurmukhi/NotoSerifGurmukhi-Regular.ttf", Ligatures.TEXT_REPRESENTATIONS, Set.empty)
  loadFont("gurmukhi", "fonts/NotoSerifGurmukhi/NotoSerifGurmukhi-Bold.ttf", Ligatures.TEXT_REPRESENTATIONS, Set("bold"))
  aliasTypeface("punjabi", "gurmukhi")

  // Noto Serif Telugu, the bundled face for Telugu: \font telugu 12 sets it, with a bold cut. Telugu is left
  // to right; the engine does the cluster segmentation (see Telugu and IndicShaper) and the font does the
  // rest, which for this script is a great deal — a consonant and its vowel sign commonly fuse into one
  // glyph, and a virama-joined consonant is drawn as a subscript beneath the base, placed by GPOS. Nothing
  // reorders and there is no reph. Loaded by file like the other complex-script cuts. Like the other Indic
  // faces it sets its punctuation as a Latin text face.
  loadFont("telugu", "fonts/NotoSerifTelugu/NotoSerifTelugu-Regular.ttf", Ligatures.TEXT_REPRESENTATIONS, Set.empty)
  loadFont("telugu", "fonts/NotoSerifTelugu/NotoSerifTelugu-Bold.ttf", Ligatures.TEXT_REPRESENTATIONS, Set("bold"))

  // JetBrains Mono — a dedicated code face for setting source code listings in a document, distinct from the
  // typewriter *role* (\mono, Latin Modern Mono) used for inline code in running text: that one is cut to match
  // the Latin Modern body, while this is a screen-bred programming face with a large character set, a tall
  // x-height and disambiguated glyphs (0/O, 1/l/I) better suited to a block of code. No ligatures — code is set
  // literally — across the family's full weight range. Selected by name, e.g. \font jetbrains 9 regular.
  loadTypeface(
    "jetbrains",
    "fonts/JetBrainsMono/static/JetBrainsMono",
    "",
    Set(),
    "Bold",
    ("Bold", "Italic"),
    "ExtraBold",
    ("ExtraBold", "Italic"),
    "ExtraLight",
    ("ExtraLight", "Italic"),
    "Italic",
    "Light",
    ("Light", "Italic"),
    "Medium",
    ("Medium", "Italic"),
    "Regular",
    "SemiBold",
    ("SemiBold", "Italic"),
    "Thin",
    ("Thin", "Italic"),
  )
  val alegreyaMissing = Set(
    `LATIN SMALL LIGATURE FFI`,
    `LATIN SMALL LIGATURE FFL`,
    `LATIN SMALL LIGATURE FF`,
    `LONG LEFT RIGHT ARROW`,
    `LONG LEFT RIGHT DOUBLE ARROW`,
    `LONG LEFTWARDS ARROW`,
    `LONG RIGHTWARDS ARROW`,
    `LONG LEFTWARDS DOUBLE ARROW`,
    `LONG RIGHTWARDS DOUBLE ARROW`,
    `LEFTWARDS DOUBLE ARROW`,
    `RIGHTWARDS DOUBLE ARROW`,
    `LEFT RIGHT DOUBLE ARROW`,
  )

  loadTypeface(
    "alegreya",
    "fonts/Alegreya/static/Alegreya",
    Ligatures.ALL diff alegreyaMissing,
    Set(),
    "Black",
    ("Black", "Italic"),
    "Bold",
    ("Bold", "Italic"),
    "ExtraBold",
    ("ExtraBold", "Italic"),
    "Italic",
    "Medium",
    ("Medium", "Italic"),
    "Regular",
    "SemiBold",
    ("SemiBold", "Italic"),
  )
  loadTypeface(
    "alegreya",
    "fonts/AlegreyaSC/AlegreyaSC",
    Ligatures.ALL diff alegreyaMissing,
    Set("smallcaps"),
    "Black",
    ("Black", "Italic"),
    "Bold",
    ("Bold", "Italic"),
    "ExtraBold",
    ("ExtraBold", "Italic"),
    "Italic",
    "Medium",
    ("Medium", "Italic"),
    "Regular",
  )

  // EB Garamond — Georg Duffner's revival of Claude Garamont's sixteenth-century roman, a book face for
  // running text: \font ebgaramond 12, with matching italic, the heavier weights on the free axis (medium,
  // semibold, extrabold) and their italics, and a real bold. Given the f-ligatures and the text
  // representations (curly quotes, dashes) it carries, but not the arrow ligatures, so a run sets clean without
  // risking a missing-glyph box.
  val ebGaramondLigatures = "ﬃﬄﬁﬂﬀ".map(_.toString).toSet ++ Ligatures.TEXT_REPRESENTATIONS
  loadTypeface(
    "ebgaramond",
    "fonts/EB_Garamond/static/EBGaramond",
    ebGaramondLigatures,
    Set(),
    "Regular",
    "Italic",
    "Medium",
    ("Medium", "Italic"),
    "SemiBold",
    ("SemiBold", "Italic"),
    "Bold",
    ("Bold", "Italic"),
    "ExtraBold",
    ("ExtraBold", "Italic"),
  )

  // Petaluma, a handwritten-style SMuFL music face and the alternative to Bravura, which is core. A document
  // choosing it is choosing something outside the guaranteed baseline, so it lives here rather than there.
  loadFont("petaluma", "fonts/Petaluma/Petaluma.otf", Set(), Set())
  }

  /** Register the faces compiled into the artifact — the engine's guaranteed baseline, present on every host
    * whatever its filesystem holds. That is the Latin Modern super-family (the roman body with its mono and sans
    * roles, its small-caps and slanted cuts), Latin Modern Math, New Computer Modern as the glyph-fallback face,
    * and Bravura for the music package.
    *
    * Strict, unlike [[loadBundledCatalogue]]: these bytes ship inside the artifact, so a face that will not open
    * is a broken build rather than an installation without it, and saying so at once beats rendering a document
    * in a substituted face nobody chose. The one path by which the bytes could come from elsewhere is a font tree
    * that shadows them (see [[FontSource]]), which is deliberate — a host may ship newer cuts.
    */
  protected def loadCoreFonts(): Unit = {
  // Latin Modern Roman — GUST's OpenType successor to Computer Modern and the text companion to Latin Modern
  // Math, so a document can set body text in the same family the math is set in (and a logo's hand-tuned kerns
  // land as designed). The 10-point optical size is the body face; its four core styles are registered by
  // file, since the .otf names don't follow loadTypeface's "-Style.ttf" convention.
  val lmLigatures = "ﬃﬄﬁﬂﬀ".map(_.toString).toSet ++ Ligatures.TEXT_REPRESENTATIONS
  loadFont("lmroman", "fonts/LatinModernRoman/lmroman10-regular.otf", lmLigatures, Set.empty)
  loadFont("lmroman", "fonts/LatinModernRoman/lmroman10-bold.otf", lmLigatures, Set("bold"))
  loadFont("lmroman", "fonts/LatinModernRoman/lmroman10-italic.otf", lmLigatures, Set("italic"))
  loadFont("lmroman", "fonts/LatinModernRoman/lmroman10-bolditalic.otf", lmLigatures, Set("bold", "italic"))
  loadFont("lmroman", "fonts/LatinModernRoman/lmromanslant10-regular.otf", lmLigatures, Set("slanted"))
  // The small-caps cut of the body face, so \smallcaps / \scshape / \smallcaps set capitals-and-small-capitals in
  // the same family as the roman text. The oblique cut backs the small-caps shape inside italic context (\italic
  // around \smallcaps), registered under the {smallcaps, italic} style so that combination resolves too. Small caps
  // gets only the text-representation ligatures (dashes, quotes) and not the f-ligatures (ﬁ, ﬂ, …): the cut has no
  // lowercase f, so it carries no f-ligature glyphs, and applying them would map e.g. "fi" to a glyph the font
  // lacks — dropping the pair from the output (a small-caps "fiable" set as "able").
  loadFont("lmroman", "fonts/LatinModernRoman/lmromancaps10-regular.otf", Ligatures.TEXT_REPRESENTATIONS, Set("smallcaps"))
  loadFont("lmroman", "fonts/LatinModernRoman/lmromancaps10-oblique.otf", Ligatures.TEXT_REPRESENTATIONS, Set("smallcaps", "italic"))

  // The typewriter and sans-serif members of the Latin Modern super-family, registered as the mono and sans
  // *roles* of the same `lmroman` typeface — so \mono and \sans select them by setting the role axis while
  // keeping the current weight and slope, and a document that switches its body super-family carries its code and
  // headings to the matching cuts. Latin Modern Mono is set without ligatures (code is literal); its bold comes
  // from the companion lmmonolt weight, the only one cut bold. Latin Modern Sans uses an oblique for its slope,
  // mapped to the same `italic` tag the roman role uses, so the slope axis reads uniformly across roles.
  loadFont("lmroman", "fonts/LatinModernMono/lmmono10-regular.otf", Set.empty, Set("mono"))
  loadFont("lmroman", "fonts/LatinModernMono/lmmonolt10-bold.otf", Set.empty, Set("mono", "bold"))
  loadFont("lmroman", "fonts/LatinModernMono/lmmono10-italic.otf", Set.empty, Set("mono", "italic"))
  loadFont("lmroman", "fonts/LatinModernMono/lmmonolt10-boldoblique.otf", Set.empty, Set("mono", "bold", "italic"))
  loadFont("lmroman", "fonts/LatinModernSans/lmsans10-regular.otf", lmLigatures, Set("sans"))
  loadFont("lmroman", "fonts/LatinModernSans/lmsans10-bold.otf", lmLigatures, Set("sans", "bold"))
  loadFont("lmroman", "fonts/LatinModernSans/lmsans10-oblique.otf", lmLigatures, Set("sans", "italic"))
  loadFont("lmroman", "fonts/LatinModernSans/lmsans10-boldoblique.otf", lmLigatures, Set("sans", "bold", "italic"))

  // New Computer Modern — a Computer Modern redrawing carrying full polytonic Greek and Cyrillic. It is loaded as
  // its own selectable face (\font newcm …) and, more usefully, registered as the glyph-fallback face: when a run
  // hits a codepoint lmroman has no glyph for (a Greek word in a footnote, say), that codepoint is set from NewCM
  // instead of a missing-glyph box. Because NewCM *is* Computer Modern, the substituted glyphs sit invisibly beside
  // the surrounding Latin Modern text. All four cuts are loaded, not just the regular one, so a substituted run
  // keeps the weight and slope of the text around it — a bold Cyrillic heading sets bold (see fallbackFontAt).
  loadFont("newcm", "fonts/NewComputerModern/NewCM10-Regular.otf", lmLigatures, Set.empty)
  loadFont("newcm", "fonts/NewComputerModern/NewCM10-Bold.otf", lmLigatures, Set("bold"))
  loadFont("newcm", "fonts/NewComputerModern/NewCM10-Italic.otf", lmLigatures, Set("italic"))
  loadFont("newcm", "fonts/NewComputerModern/NewCM10-BoldItalic.otf", lmLigatures, Set("bold", "italic"))
  fallbackTypeface = Some("newcm")

  // The default math font: Latin Modern Math in its SMaFL form, an OpenType font with a full MATH table whose
  // cmap has been extended to give every size-variant and assembly glyph a private-use codepoint (see the
  // SmaflConvertMain build tool). Loaded by file directly (not loadTypeface, whose naming assumes a .ttf)
  // since it is a CFF/.otf and stands alone with no style variants. Math mode reads its MATH table through the
  // SFNT seam (see mathTableFor); the SMaFL cmap lets the in-browser canvas backend draw stretchy glyphs
  // through the browser's hinted text path. The extra cmap entries are inert on the outline-filling backends.
  loadFont("lmmath", "fonts/LatinModernMath/LatinModernMath-SMaFL.otf", Set(), Set())

  // SMuFL (Standard Music Font Layout) music fonts. SMuFL fixes the codepoint of every notation glyph (clefs,
  // noteheads, flags, rests, accidentals, time-signature figures, and more) in the Unicode Private Use Area and
  // the convention that one staff space is a quarter of the em, so the music package can set glyphs with
  // \fontglyph at a size of four staff spaces and have the font's own coordinates map straight onto the staff —
  // and can swap one face for another. Both the faces texish bundles are SIL OFL. Bravura is the reference font,
  // and is core so that \use{music} — an embedded package — can always draw; the handwritten-style Petaluma is
  // an alternative the catalogue carries. Drawn by glyph index through the same seam math mode uses.
  loadFont("bravura", "fonts/Bravura/Bravura.otf", Set(), Set())
  }

  init(1, 1)
  // TeX's body size. `set(defaultParameters)` derives \baselineskip from it as 1.2x, giving TeX's
  // 10pt on 12pt leading, and it matches the size ladder in the base package, whose \normalsize is 10.
  selectFont("lmroman", 10, Set("regular"))
  set(defaultParameters)
  push(document)
  push(new PageMode(this))

  def push(m: Mode): Unit = modeStack push m

  def mode: Mode = modeStack.top

  def setFont(f: Font): Unit = setFont(f.renderFont.asInstanceOf[RenderFont])

  // The engine works in big points (1/72 inch), on every backend. DPI exists
  // only at the device boundary, inside raster backends.
  def pt: Double = 1

  def in: Double = 72

  def cm: Double = in / 2.54

  def mm: Double = cm / 10

  /** The named paper sizes the engine knows, as `(width, height)` in point space. Shared by the `\geometry`
    * primitive and the CLI's `--paper` flag so the two never drift apart. Names are matched case-insensitively. */
  def paperSize(name: String): Option[(Double, Double)] = name.toLowerCase match
    case "letter" => Some((8.5 * in, 11 * in))
    case "legal"  => Some((8.5 * in, 14 * in))
    case "a3"     => Some((297 * mm, 420 * mm))
    case "a4"     => Some((210 * mm, 297 * mm))
    case "a5"     => Some((148 * mm, 210 * mm))
    case "a6"     => Some((105 * mm, 148 * mm))
    case _        => None

  infix def get(name: String): Option[Value] = scopes.top.get(name)

  infix def getVar(name: String): Value =
    scopes.top.getOrElse(name, throw TexishException(s"variable '$name' not found"))

  infix def getGlue(name: String): Glue = getVar(name) match
    case Value.Native(g: Glue)           => g
    case Value.Glue(n, st, sh, sto, sho) => Glue(n, st, sh, sto, sho)
    case Value.Dimen(p)                  => Glue(p) // a plain dimension is rigid glue
    case Value.Num(n)                    => Glue(n)
    case v                               => throw TexishException(s"variable '$name' is not glue: ${Value.display(v)}")

  infix def getNumber(name: String): Double = getVar(name) match
    case Value.Num(n)   => n
    case Value.Dimen(p) => p
    case v              => throw TexishException(s"variable '$name' is not a number: ${Value.display(v)}")

  def set(name: String, value: Any): Unit =
    scopes(0) += (name -> Value.from(value))

  // global: set in every open scope (each is an independent snapshot), so the value is visible now and remains
  // after every enclosing group exits down to the outermost scope
  def setGlobal(name: String, value: Any): Unit =
    val v = Value.from(value)
    for i <- 0 until scopes.length do scopes(i) = scopes(i) + (name -> v)

  def set(pairs: Seq[(String, Any)]): Unit =
    scopes(0) ++= pairs.map((k, v) => (k, Value.from(v)))

  def enter(): Unit =
    scopes push scopes.top
    set("saved_font", currentFont)
    set("saved_color", currentColor)

  def exit(): Unit =
    val prev = scopes.pop()

    prev.get("saved_font") match
      case Some(Value.Native(font: Font)) => currentFont = font
      case _                              =>

    prev.get("saved_color") match
      case Some(Value.Native(color: Color)) => currentColor = color
      case _                                =>

  /** The pen at document level: the colour in force outside every open group. Material that is set apart from
    * the text that called for it — a footnote, whose marker may fall inside a coloured span but whose note is
    * set at the foot in body colour — resets to this, so it follows whatever ink the document (or a previewer
    * inverting the page) actually chose rather than a fixed black.
    *
    * Each [[enter]] records the pen it opened with, so the outermost group's record is the pen outside every
    * group; with no group open the current pen already is it. A document that brackets its whole body in one
    * group and colours it there has put its ink inside that group, and its notes follow the pen outside it. */
  def documentColor: Color =
    if scopes.length < 2 then currentColor
    else
      scopes(scopes.length - 2).get("saved_color") match
        case Some(Value.Native(color: Color)) => color
        case _                                => currentColor

  // A font's style is one open set of tags, but the tags fall on a few independent axes — series (the weight),
  // slope (upright / italic / slanted), caps (small-capitals), and role (the typewriter / sans member of a
  // super-family). Within an axis the values are mutually exclusive: a face is bold *or* light, italic *or*
  // slanted, the mono role *or* the sans role — never two at once. The set stays open (a font may carry weights
  // the engine has never heard of, or a future width axis) by classifying only the tags it knows and leaving the
  // rest free-combining. selecting an axis value (below) drops any rival on the same axis first.
  enum StyleAxis:
    case Series, Slope, Caps, Role, Free

  def axisOf(tag: String): StyleAxis = tag.toLowerCase match
    case "bold" | "light" | "medium" | "semibold" | "extrabold" | "black" | "thin" | "extralight" | "demibold" =>
      StyleAxis.Series
    case "italic" | "slanted" | "oblique" => StyleAxis.Slope
    case "smallcaps"                      => StyleAxis.Caps
    case "sans" | "mono"                  => StyleAxis.Role
    case _                                => StyleAxis.Free

  /** Select one value on a style axis, replacing whatever rival currently holds that axis (italic kicks slanted,
    * the mono role kicks the sans role). An empty value clears the axis — its NFSS reset (upright, medium, serif).
    * Free-axis tags have no rivals, so this just adds the tag. */
  def setAxis(axis: StyleAxis, value: String): Font =
    val cleared = currentFont.style.filterNot(t => axisOf(t) == axis)
    setStyle(if value.isEmpty then cleared else cleared + value.toLowerCase)

  def italic(): Font    = setAxis(StyleAxis.Slope, "italic")
  def noitalic(): Unit  = removeStyle("italic")
  def slanted(): Font   = setAxis(StyleAxis.Slope, "slanted")
  def noslanted(): Unit = removeStyle("slanted")
  def bold(): Font      = setAxis(StyleAxis.Series, "bold")
  def nobold(): Unit    = removeStyle("bold")
  def smallcaps(): Font   = setAxis(StyleAxis.Caps, "smallcaps")
  def nosmallcaps(): Unit = removeStyle("smallcaps")

  // The family-role axis selects the typewriter (\mono) or sans-serif (\sans) member of the current
  // super-family, keeping the series and slope; serif() clears the role back to the roman default.
  def mono(): Font  = setAxis(StyleAxis.Role, "mono")
  def sans(): Font  = setAxis(StyleAxis.Role, "sans")
  def serif(): Font = setAxis(StyleAxis.Role, "")

  def setStyle(style: Set[String]): Font = selectFont(currentFont.typeface, currentFont.size, style)

  def typeface(name: String): Font = selectFont(name, currentFont.size, Set.empty)

  def setStyle(style: String*): Font = setStyle(style.toSet)

  def addStyle(style: String*): Font = setStyle(currentFont.style ++ style)

  def removeStyle(style: String*): Font = setStyle(currentFont.style -- style)

  /** Add a place to look for font files, after everywhere already registered and before the embedded core. This
    * is how a host points the engine at fonts it ships or manages: the CLI registers the tree beside its own
    * binary, an application registers a folder of its own. Register before typesetting begins, since a source
    * added later cannot supply a face the document has already asked for.
    */
  def registerFontSource(source: FontSource): Unit = fontSources += source

  /** Forget every registered source, leaving only the core embedded in the artifact. For a host that wants the
    * engine to draw with exactly what it was given and nothing the machine happens to have lying about — and for
    * tests, which must be able to reproduce a bare installation on a machine with a full font tree on it. */
  def clearFontSources(): Unit = fontSources.clear()

  /** The sources consulted for one load, in order: the directory of the document or module doing the loading,
    * then everything registered, then the embedded core last.
    *
    * The `from` root is what lets a host that is not run from the document's directory — an editor, a preview app
    * — find a font sitting beside the document, and it comes first so that a document's own font files shadow the
    * bundled ones, the same order in which `\use` resolves a module. The engine's own core and catalogue loads
    * pass no `from`. */
  private def fontSearchPath(from: String): LazyList[FontSource] =
    (if from == "." then LazyList.empty else LazyList(DirectoryFontSource(from))) #:::
      fontSources.to(LazyList) #:::
      LazyList(EmbeddedFontSource)

  /** Open a face from one source, preferring a file over bytes: a backend reads a face from a path lazily, where
    * bytes must be materialised whole and — on the backends whose font API does not copy — kept alive for the
    * life of the face. */
  private def openFrom(source: FontSource, path: String): Option[FontFace] =
    source.file(path).map(loadFont) orElse source.bytes(path).map(loadFontBytes(_, path))

  /** Guarded because a filesystem-less host reaches for platform APIs that may be absent; there nothing is
    * absolute and every face comes from the embedded core, which is exactly right. */
  private def isAbsolutePath(path: String): Boolean =
    try Path(path).isAbsolute
    catch case _: Throwable => false

  private def openAbsolute(path: String): Option[FontFace] =
    try Option.when(Path(path).exists)(loadFont(path))
    catch case _: Throwable => None

  /** Open a face for a relative or absolute font path. An absolute path names one file and no source can move
    * it; a relative one is offered to each source in turn. None when nothing has it — which for a catalogue load
    * means that family is not available in this installation, and for a `\loadfont` means the document named a
    * file that is not there. */
  private def openFace(path: String, from: String): Option[FontFace] =
    if isAbsolutePath(path) then openAbsolute(path)
    else fontSearchPath(from).iterator.flatMap(openFrom(_, path)).nextOption()

  /** True while [[loadBundledCatalogue]] is running. Inside that window a face no font source has is skipped
    * rather than fatal, and a family operation naming a skipped family is a no-op — so a partial font tree gives
    * a host whatever it does have instead of failing outright. Everywhere else a font that cannot be opened is an
    * error: in [[loadCoreFonts]] because those bytes are in the artifact, and in a `\loadfont` because the
    * document named a file that is not there. */
  private var loadingCatalogue = false

  /** The catalogue families this typesetter tried to register and could not, each against the first font file it
    * went looking for. Read by [[unavailableNote]] to tell a document that named such a family that the family is
    * real and the *files* are what is missing — which points at the installation rather than at the document.
    * Insertion-ordered so a host reporting the whole set lists it in catalogue order. */
  private val unavailable = mutable.LinkedHashMap[String, String]()

  /** Every family [[loadBundledCatalogue]] attempted, whether or not its files were found. This is the catalogue's
    * membership as the code actually defines it; [[Typesetter.BundledFamilies]] is the same list written down for
    * hosts that have not loaded it, and a test holds the two together. */
  private val attempted = mutable.LinkedHashSet[String]()

  private[texish] def attemptedCatalogueFamilies: Set[String] = attempted.toSet

  /** Why a typeface name did not resolve, when the engine can say something better than "not found": that the
    * name is one texish bundles and either its files are missing or the catalogue was never loaded. Both readings
    * send the reader to the installation instead of to a spell-check of their own document. */
  private def unavailableNote(typeface: String): Option[String] =
    if unavailable contains typeface then
      Some(
        s"typeface '$typeface' is one texish bundles, but its font files were not found — " +
          s"no font source has '${unavailable(typeface)}'")
    else if Typesetter.BundledFamilies contains typeface then
      Some(
        s"typeface '$typeface' is one texish bundles, but the bundled catalogue has not been loaded — " +
          "call loadBundledCatalogue() with a font tree available")
    else None

  /** Register a font file under a typeface name. `from` is the directory a relative `path` is resolved against
    * before any registered source — the document's own directory, for a `\loadfont` in a document. It defaults
    * to the current directory, which is how the engine loads the faces it bundles (see [[fontSearchPath]]). */
  def loadFont(
      typeface:  String,
      path:      String,
      ligatures: Set[String],
      styleSet:  Set[String],
      from:      String = ".",
  ): Unit =
    if loadingCatalogue then attempted += typeface

    openFace(path, from) match
      case Some(face) => registerFont(typeface, face, ligatures, styleSet)
      case None if loadingCatalogue =>
        unavailable.getOrElseUpdate(typeface, path) // this installation does not have that face
        ()
      case None => throw TexishException(s"font file '$path' not found")

  /** Register an already-opened face as one style of a typeface — the step [[loadFont]] takes once it has a
    * face in hand. A host holding a font as bytes rather than as a file (a resource bundle, a face fetched at
    * runtime, a font pack shipped as its own artifact) opens it with [[loadFontBytes]] and registers it here.
    * Loading two faces into the same style of the same family is an error: the second would silently shadow the
    * first, which is never what a caller means. */
  def registerFont(typeface: String, face: FontFace, ligatures: Set[String], styleSet: Set[String]): Unit =
    typefaces get typeface match
      case None => typefaces(typeface) = Typeface(mutable.HashMap(styleSet -> (face, ligatures)), None)
      case Some(Typeface(fonts, _)) =>
        if fonts contains styleSet then
          throw TexishException(
            s"font for typeface '$typeface' with style '${styleSet.mkString(", ")}' has already been loaded")
        else fonts(styleSet) = (face, ligatures)

  /** Give an already-loaded typeface a second name — `\font hindi` for the Devanagari family, `\font korean` for
    * the Korean CJK one. The alias shares the faces the family already opened rather than opening the files a
    * second time: loading a font by name twice would leave the backend holding two faces over the same file,
    * which for the large CJK cuts is a real duplication, and an alias is not a separate family in any case.
    */
  def aliasTypeface(alias: String, target: String): Unit =
    if loadingCatalogue then attempted += alias

    typefaces get target match
      // The family it names was skipped for want of its files. Carry the missing path over to the alias so that
      // \font hindi reports what \font devanagari would, rather than reading as a name texish has never heard of.
      case None if loadingCatalogue =>
        unavailable.get(target).foreach(path => unavailable.getOrElseUpdate(alias, path))
        ()
      case None => throw TexishException(s"typeface '$target' not found")
      case Some(tf) =>
        if typefaces contains alias then throw TexishException(s"typeface '$alias' has already been loaded")
        typefaces(alias) = tf

  def loadTypeface(
      typeface: String,
      basepath: String,
      ligatures: String | Set[String],
      every: Set[String],
      styles: (Product | String)*,
  ): Unit =
    for style <- styles do
      val (styleName, styleSet) =
        style match
          case s: String  => (s, if s.toLowerCase == "regular" then Set.empty else Set(s.toLowerCase))
          case p: Product => (p.productIterator.mkString, p.productIterator.map(_.toString.toLowerCase).toSet)

      loadFont(
        typeface,
        s"$basepath-$styleName.ttf",
        ligatures match
          case l: String      => l map (_.toString) toSet
          case s: Set[String] => s,
        styleSet ++ every.map(_.toLowerCase),
      )
  end loadTypeface

  def overrideBaseline(typeface: String, baseline: Double): Unit =
    typefaces get typeface match
      case None if loadingCatalogue => () // the family it names was skipped for want of its files
      case None                     => throw TexishException(s"typeface '$typeface' not found")
      case Some(Typeface(fonts, _)) => typefaces(typeface) = Typeface(fonts, Some(baseline))

  /** Whether a typeface of this name has been loaded — used to validate a family name a document names (e.g.
    * the typewriter and sans families \ttdefault / \sfdefault point at) before it is stored. */
  def hasTypeface(name: String): Boolean = typefaces.contains(name)

  def selectFont(typeface: String, size: Double, style: String*): Font = selectFont(typeface, size, style.toSet)

  def selectFont(typeface: String, size: Double, styleSet: Set[String]): Font =
    val f = makeFont(typeface, size, styleSet)

    if f != currentFont then
      currentFont = f
      // Track the font with TeX's default leading (\baselineskip = 1.2 × the size), so changing the font size
      // keeps proportional line spacing. A document that wants a specific leading sets baselineskip after.
      set("baselineskip", Glue(f.size * 1.2))

    currentFont

  def makeFont(typeface: String, size: Double, styleSet: Set[String]): Font =
    typefaces get typeface match
      case None =>
        throw TexishException(unavailableNote(typeface) getOrElse s"font for typeface '$typeface' not found")
      case Some(Typeface(fonts, baseline)) =>
        val wanted = styleSet.map(_.toLowerCase).filterNot(_ == "regular")
        // Resolve the exact style if it was loaded; otherwise substitute the nearest available cut by dropping the
        // least essential axes in turn — small caps first, then slope, then weight — keeping the family role (the
        // mono/sans member) as long as possible. So a mono face with no small-caps or slanted cut falls back to
        // upright mono rather than failing, as LaTeX's font substitution does.
        val inFamily =
          fonts
            .get(wanted)
            .orElse(LazyList(StyleAxis.Caps, StyleAxis.Slope, StyleAxis.Series)
              .scanLeft(wanted)((k, axis) => k.filterNot(t => axisOf(t) == axis))
              .tail
              .flatMap(fonts.get)
              .headOption)

        inFamily match
          case Some((face, ligatures)) =>
            val derivedFont = makeFont(face, size)

            // Small caps was asked for but no dedicated small-caps cut exists (the caps axis fell back to the
            // ordinary face). The render layer may then synthesize small caps through the font's `smcp` feature.
            val syntheticSmallcaps = wanted.contains("smallcaps") && !fonts.contains(wanted)

            Font(
              typeface,
              size,
              charWidth(derivedFont, ' '),
              getTextExtents("x", derivedFont).height,
              styleSet,
              derivedFont,
              baseline,
              ligatures,
              syntheticSmallcaps,
            )

          case None =>
            // \mono or \sans on a family that carries no such member: resolve the document's typewriter or
            // sans-serif family instead (\ttdefault / \sfdefault; Latin Modern by default), the way LaTeX's
            // \ttfamily / \sffamily resolve independent of the text family. The named family may be a super-family
            // that carries a mono/sans member cut (Latin Modern, whose typewriter lives under the "mono" style),
            // or a standalone monospaced family whose plain cut *is* the typewriter (JetBrains Mono); the role tag
            // is kept for the former and dropped for the latter, so both resolve. Weight and slope carry over. A
            // family that provides the member resolved above and never reaches here.
            wanted.collectFirst { case s if axisOf(s) == StyleAxis.Role => s } match
              case Some(role) =>
                val fam = if role == "mono" then monoDefaultTypeface else sansDefaultTypeface
                if fam == typeface || !typefaces.contains(fam) then
                  throw TexishException(
                    s"font for typeface '$typeface' with style '${styleSet.mkString(", ")}' has not been loaded")
                else
                  val stripped  = styleSet.filterNot(s => axisOf(s) == StyleAxis.Role)
                  val keepsRole = typefaces.get(fam).exists(_.fonts.keys.exists(_.contains(role)))
                  makeFont(fam, size, if keepsRole then stripped + role else stripped)
              case None =>
                throw TexishException(
                  s"font for typeface '$typeface' with style '${styleSet.mkString(", ")}' has not been loaded")

  infix def add(text: String): Typesetter = add(charBox(text))

  def charBox(s: String): CharBox =
    val rep =
      if representations then Ligatures.replace(s, Ligatures.REPRESENTATIONS, currentFont.ligatures) else s

    new CharBox(this, if ligatures then Ligatures(rep, currentFont.ligatures) else rep)

  /** The glyph-fallback font at a given size and style, if a fallback face is registered — the face used to set
    * codepoints the active font has no glyph for. Built at `size` and in `style` so the substitute matches the
    * surrounding text: a Cyrillic word inside a bold heading is set from the fallback's bold cut, not its regular
    * one, so the weight does not break mid-phrase.
    *
    * The family role (the mono/sans axis) is dropped. The fallback stands in for the primary face's *coverage*,
    * not its family, and it is a single roman family; keeping the role would send makeFont to the typewriter or
    * sans-serif default instead, which is no likelier to have the glyph. Weight, slope and caps carry over, and
    * makeFont substitutes the nearest loaded cut, so a fallback face missing the exact combination degrades to
    * what it has rather than failing. */
  def fallbackFontAt(size: Double, style: Set[String] = Set.empty): Option[Font] =
    val wanted = style.filterNot(s => axisOf(s) == StyleAxis.Role)
    fallbackTypeface.filter(typefaces.contains).map(tf => makeFont(tf, size, wanted))

  /** Split a text run into maximal pieces, each set in one font: the primary font over the stretches it has glyphs
    * for, the fallback font over the stretches it does not (where the fallback does). When the primary covers the
    * whole run — the overwhelmingly common case — the run comes back as a single piece, so ordinary Latin text is
    * untouched and pays only one glyph-coverage scan. A codepoint neither font has stays with the primary (and draws
    * its missing-glyph box, exactly as before). */
  def coverageSegments(text: String, primary: Font): Array[(String, Font)] =
    val prf = primary.renderFont.asInstanceOf[RenderFont]
    var gap = false
    var i   = 0
    while i < text.length && !gap do
      val cp = text.codePointAt(i)
      if glyphIndex(prf, cp) == 0 then gap = true
      i += Character.charCount(cp)

    if !gap then Array((text, primary))
    else
      fallbackFontAt(primary.size, primary.style) match
        case None => Array((text, primary))
        case Some(fb) =>
          val frf = fb.renderFont.asInstanceOf[RenderFont]
          val out = new ArrayBuffer[(String, Font)]
          val sb  = new StringBuilder
          var cur: Font = primary
          var j = 0
          while j < text.length do
            val cp     = text.codePointAt(j)
            val n      = Character.charCount(cp)
            val chosen = if glyphIndex(prf, cp) == 0 && glyphIndex(frf, cp) != 0 then fb else primary
            if sb.nonEmpty && (chosen ne cur) then
              out += ((sb.toString, cur)); sb.clear()
            cur = chosen
            sb ++= text.substring(j, j + n)
            j += n
          if sb.nonEmpty then out += ((sb.toString, cur))
          out.toArray

  /** A strut sized to the current baselineskip: TeX splits `\strutbox` roughly 0.7 above the baseline and 0.3 below,
    * so a strutted line stands one baselineskip tall and leading comes out regular whatever the line's glyphs are. */
  def strut: StrutBox =
    val leading = getGlue("baselineskip").naturalSize
    new StrutBox(leading * 0.7, leading * 0.3)

  infix def add(box: Box): Typesetter =
    ensureInitializedForContent()
    mode add box
    this

  def glue(
      naturalWidth: Double,
      stretch: Double = 0,
      shrink: Double = 0,
      stretchOrder: Int = 0,
      shrinkOrder: Int = 0,
  ): Typesetter =
    add(Glue(naturalWidth, stretch, shrink, stretchOrder, shrinkOrder))

  def fil: Typesetter = add(FilGlue)

  def fill: Typesetter = add(FillGlue)

  def noBreakSpace: Typesetter = add(getGlue("spaceskip").noBreak)

  def result: Box = mode.result

  def pop: Mode = modeStack.pop()

  def halign: Typesetter =
    push(new HAlignMode(this))
    this

  // ---- Hyphenation ----

  /** The language this document is written in, or `None` when it is not declared. It decides two things: the
    * hyphenation patterns the line breaker uses, and the typography a language sets its punctuation by —
    * French, for one, spaces its high punctuation differently from English (see [[FrenchSpacing]]). This is
    * per-document state — `\usehyphenation` / `\loadhyphenation` / `\language` set it on the typesetter — so
    * two documents (or two concurrent test suites) never affect each other. The hyphenation pattern tables
    * themselves are a shared, append-only cache in the [[Hyphenation]] object; only the *selection* is here. */
  var language: Option[String] = None

  // ---- Math mode ----

  /** The typeface math mode sets symbols in. Defaults to Latin Modern Math; an application can point it at
    * another OpenType math font it has loaded. */
  var mathTypeface: String = "lmmath"

  private val mathTableCache = mutable.HashMap[String, Option[MathTable]]()

  /** The parsed OpenType `MATH` table for a font's typeface, read once through the SFNT seam and cached by
    * typeface name. `None` when the typeface carries no `MATH` table (an ordinary text font), in which case
    * math layout falls back to TeX's traditional constants. */
  def mathTableFor(font: Font): Option[MathTable] =
    mathTableCache.getOrElseUpdate(
      font.typeface, {
        val rf = font.renderFont.asInstanceOf[RenderFont]

        sfntTable(rf, "MATH").map { bytes =>
          val upm = sfntTable(rf, "head").map(h => ByteCursor(h, 18).u16).getOrElse(1000)

          MathTable.parse(bytes, upm)
        }
      },
    )

  /** Begin math. Inline (`$…$`) ensures a horizontal list is open so the finished box lands in the running
    * text, and sets the list in text style. Display (`$$…$$`) ends the current paragraph first — the display
    * is set on its own, between vertical glue — and sets the list in the larger display style, where big
    * operators stack their limits and fractions open up. */
  def enterMath(display: Boolean = false): MathMode =
    if display then paragraph() // a $$…$$ breaks the paragraph; the display rides the vertical list
    else start                  // a $…$ in vertical mode starts a paragraph, exactly as text would

    val mf    = makeFont(mathTypeface, currentFont.size, Set.empty)
    val style = if display then MathStyle.Display else MathStyle.Text
    val m     = new MathMode(this, new MathFont(this, mf, mathTableFor(mf)), style)

    push(m)
    m

  /** End math. An inline list lays out and drops its box into the running text. A display list lays out and is
    * placed on its own centred line, with `\abovedisplayskip` above and `\belowdisplayskip` below; an
    * equation number set by `\eqno` is flushed to the right margin on that line. */
  def exitMath(): Unit =
    mode match
      case m: MathMode if m.style.isDisplay =>
        val box = m.exit // pop the math mode and lay its list out

        if box ne null then placeDisplay(box, m.eqno)
      case _ => mode.done()

  /** Place a finished display formula on the vertical list: glue above, a line holding the centred formula
    * (and its equation number, if any, flush right) at the page width, then glue below. */
  private def placeDisplay(box: Box, eqno: Option[Box]): Unit =
    add(getGlue("abovedisplayskip"))

    hbox(getNumber("hsize"))
    fil
    add(box)
    fil
    eqno.foreach(add)
    done()

    add(getGlue("belowdisplayskip"))

  def op(operation: String): Typesetter =
    modeStack.top.op(operation)
    this

  def hbox(toSize: Double | Null = null, spread: Double | Null = null): Typesetter =
    push(new HBoxBuilder(this, toSize, spread, reorderBidi = true))
    this

  def vbox(toSize: Double | Null = null, spread: Double | Null = null): Typesetter =
    push(new VBoxBuilder(this, toSize, spread))
    this

  // A \vtop: like \vbox but with the reference point on the first line's baseline.
  def vtop(toSize: Double | Null = null, spread: Double | Null = null): Typesetter =
    push(new VBoxBuilder(this, toSize, spread, top = true))
    this

  def image(
      path: String,
      width: Option[Double] = None,
      height: Option[Double] = None,
      scale: Option[Double] = None,
  ): Typesetter =
    add(new ImageBox(this, path, width, height, scale))
    this

  def done(): Unit = mode.done()

  def newpage(): Unit =
    paragraph()
    mode match
      case p: PageMode => p.newpage()
      case _           =>

  def paragraph(): Unit =
    mode match
      case p: ParagraphMode => p.done()
      case _                =>

  def end(): Unit =
    while modeStack.nonEmpty do done()

  def start: Typesetter =
    mode match
      case v: VerticalMode =>
        val paragraphMode = new ParagraphMode(this)

        push(paragraphMode)

        if indentParagraph then paragraphMode add HSpaceBox(getNumber("parindent"), indent = true)
      case _ =>

    this

  // Leave vertical mode for a horizontal command. In TeX a horizontal command — a character, inline glue like
  // \hskip, a shifted box like a superscript — seen while the main vertical list is being built begins a paragraph:
  // \parindent is inserted (unless \noindent set it flush), \everypar runs, and the material lands in that
  // paragraph rather than stranding on the vertical list. This is that transition. It respects the running indent
  // state, so the paragraph a verse number or other inline item opens sets flush after a heading and indented after
  // a body paragraph, exactly as prose does. A no-op inside a paragraph or box. A bare \hbox is *not* a horizontal
  // command — it is a legal vertical-list item — and does not call this; that is why \hbox and \hskip differ here.
  def leaveVmode: Typesetter = start

  def indent: Typesetter =
    indentParagraph = true
    start

  def noindent: Typesetter =
    indentParagraph = false
    start

  private def defaultParameters =
    List(
      "baselineskip"  -> Glue(currentFont.size * 1.2 * pt),
      "lineskip"      -> Glue(1 * pt),
      "lineskiplimit" -> 0.0,
      "spaceskip"     -> interwordGlue(currentFont.space),
      "xspaceskip"    -> xinterwordGlue(currentFont.space),
      "hsize"         -> 6.5 * in,
      "vsize"         -> 9 * in,
      "parindent"     -> 20 * pt, // TeX's \parindent
      "parfillskip"   -> FilGlue, // 0pt plus 1fil, so the last line of a paragraph fills out to the margin
      "leftskip"      -> ZeroGlue,
      "rightskip"     -> ZeroGlue,
      "parskip"       -> Glue(0, 1 * pt), // TeX's \parskip: 0pt plus 1pt between paragraphs
      "hangindent"    -> 0.0,
      "hangafter"     -> 1.0,
      "pardir"        -> 0.0, // paragraph base writing direction: 0 left-to-right, 1 right-to-left
      "wrapsep"       -> 12 * pt, // gutter between a wrapped figure and the text flowing around it
      "hoffset"       -> 1 * in,
      "voffset"       -> 1 * in,
      "pageno"        -> 1.0,
      "headsep"       -> in / 4, // bottom of the running header to the top of the body
      "footskip"      -> in / 4, // bottom of the body to the top of the running footer
      "topskip"       -> Glue(10 * pt), // TeX's \topskip: distance from the page top to the first baseline
      "raggedbottom"  -> 0.0, // nonzero: pad page bottoms with fil instead of stretching the page's glue

      // display math ($$…$$): the glue set above and below a displayed formula
      "abovedisplayskip" -> Glue(currentFont.size * 0.83 * pt, currentFont.size * 0.17 * pt, currentFont.size * 0.17 * pt),
      "belowdisplayskip" -> Glue(currentFont.size * 0.83 * pt, currentFont.size * 0.17 * pt, currentFont.size * 0.17 * pt),

      // footnotes (see InsertBox): the space above the separator rule, the footnote text size as a fraction
      // of the surrounding font, the leading of the note as a multiple of that reduced size, and the footnote
      // counter advanced by each \footnote. The scale is `footnotescale`, not `footnotesize`, so it does not
      // collide with the LaTeX-style `\footnotesize` size declaration a document package defines under that name.
      // `footnoteleading` is tighter than the body's 1.2 on purpose: a footnote reads as a compact aside, not a
      // scaled-down copy of the running text.
      "footnotesep"     -> Glue(currentFont.size * pt),
      "footnotescale"   -> 0.8,
      "footnoteleading" -> 1.0,
      "footnoteno"      -> 0.0,

      // top floats (see FloatBox): the space between two floats stacked in a page's float area, and the space
      // between that float area and the body text below it
      "floatsep"     -> Glue(currentFont.size * 0.85 * pt),
      "textfloatsep" -> Glue(currentFont.size * 1.4 * pt),

      // mark state, maintained by the page builder as pages ship (see MarkBox)
      "topmark"   -> "", // the last mark of the previous page
      "firstmark" -> "", // the first mark on the page being shipped
      "botmark"   -> "", // the last mark on the page being shipped

      // Knuth-Plass line breaking parameters
      "tolerance"       -> 200.0,  // max acceptable badness
      "pretolerance"    -> 100.0,  // first pass tolerance (no hyphenation)
      "hyphenpenalty"   -> 50.0,   // penalty for hyphenating
      "exhyphenpenalty" -> 50.0,   // penalty for explicit hyphen break
      "linepenalty"     -> 10.0,   // penalty per line
      "adjdemerits"     -> 10000.0, // penalty for adjacent tight/loose lines
      "emergencystretch" -> 0.0,   // extra per-line stretch in the final pass for paragraphs that won't justify

      // Page-break penalties between the lines of a paragraph. The page builder breaks by cost (badness + penalty),
      // so a value between 0 and 9999 biases the break away from this position; 10000 forbids a break here and
      // -10000 forces one. The defaults forbid orphans (a paragraph's first line alone at a page bottom) and widows
      // (its last line alone at a page top).
      "clubpenalty"      -> 10000.0, // against breaking after a paragraph's first line
      "widowpenalty"     -> 10000.0, // against breaking before a paragraph's last line
      "interlinepenalty" -> 0.0,     // between all other lines

      // Against a page break that lands inside a wrapped figure's band, which would strand the figure on one page
      // and the text it narrows on the next. Forbidding it keeps the whole wrap together; the page builder breaks
      // above the figure instead and moves it wholesale to the next page (a figure taller than the page is exempt,
      // since forbidding every break inside it would leave nowhere to break).
      "wrappenalty"      -> 10000.0,

      //
      "imageScaling" -> 1.0,

      // The physical sheet, as in LaTeX/pdfTeX. (Classic TeX has no page-size concept — it sizes only the text
      // area, \hsize × \vsize, and leaves the sheet to the output driver.) The text block is placed at
      // \hoffset/\voffset from the corner, so the margins are paperwidth − hoffset − hsize and likewise vertically.
      "paperwidth"   -> 8.5 * in,
      "paperheight"  -> 11 * in,
    )

end Typesetter

case class TextExtents(
    xBearing: Double,
    yBearing: Double,
    width: Double,
    height: Double,
    xAdvance: Double,
    yAdvance: Double,
)
