package io.github.edadma.texish

//import pprint.pprintln

import io.github.edadma.texish.opentype.{ByteCursor, Gpos, Gsub, IndicShaper, MathTable}
import io.github.edadma.texish.parser.Value

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized
import scala.language.postfixOps

/** The geometry of a single rendered fragment cropped to its ink, in points: its `width` and `height`, and
  * `baseline` — the distance from the top of the cropped box down to the math baseline. An inline math element
  * is aligned to the surrounding text by setting its CSS `vertical-align` to `baseline - height` (pt), which
  * drops the element so its math baseline, not its bottom edge, sits on the text baseline. */
final case class FragmentMetrics(width: Double, height: Double, baseline: Double)

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

  def loadFont(path: String): FontFace

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

  loadBundledFonts()

  /** Load the bundled typefaces this backend draws with, registering each style of each family. The JVM and
    * Native hosts load the full set from disk; the Scala.js host, which has no filesystem and must keep the
    * download small, overrides this to load only the embedded fonts it ships. */
  protected def loadBundledFonts(): Unit = {
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
  loadTypeface(
    "charm",
    "fonts/Charm/Charm",
    Ligatures.ALL,
    Set(),
    "Regular",
    "Bold",
  )
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
  // set do not fit these single hand-instanced cuts. These are large faces — the host with no filesystem (the
  // in-browser build) overrides loadBundledFonts and does not ship them.
  loadFont("cjksc", "fonts/NotoSerifCJK/NotoSerifSC-Regular.ttf", Set.empty, Set.empty)
  loadFont("cjktc", "fonts/NotoSerifCJK/NotoSerifTC-Regular.ttf", Set.empty, Set.empty)
  loadFont("cjksc", "fonts/NotoSerifCJK/NotoSerifSC-Bold.ttf", Set.empty, Set("bold"))
  loadFont("cjktc", "fonts/NotoSerifCJK/NotoSerifTC-Bold.ttf", Set.empty, Set("bold"))

  // Noto Serif Hebrew, the bundled face for right-to-left Hebrew: \font hebrew 12 sets it, with a bold cut
  // so \font hebrew 12 bold is a real bold weight. These are static Regular and Bold instances drawn from
  // the variable upstream (wght 400 and 700 at default width). The reordering into visual order is the
  // engine's (see Bidi and ParagraphMode); the font supplies the glyphs and, for pointed text, the marks.
  // Loaded by file because the single instanced cuts do not fit loadTypeface's "-Style.ttf" naming. As with
  // the CJK faces, the filesystem-less in-browser build overrides loadBundledFonts and does not ship them.
  loadFont("hebrew", "fonts/NotoSerifHebrew/NotoSerifHebrew-Regular.ttf", Set.empty, Set.empty)
  loadFont("hebrew", "fonts/NotoSerifHebrew/NotoSerifHebrew-Bold.ttf", Set.empty, Set("bold"))

  // Noto Naskh Arabic, the bundled face for right-to-left Arabic (and Persian, Urdu, … in the same script):
  // \font arabic 12 sets it, with a bold cut so \font arabic 12 bold is a real bold weight. These are static
  // Regular and Bold instances drawn from the variable upstream (wght 400 and 700). The Naskh style is the
  // book hand the joining engine targets; the cursive forms come from the font's GSUB, selected per letter by
  // the engine (see ArabicShaping and Gsub), and the bidi reordering into visual order is the engine's (see
  // Bidi and ParagraphMode). Loaded by file, like the Hebrew and CJK cuts; the filesystem-less in-browser
  // build overrides loadBundledFonts and does not ship them.
  loadFont("arabic", "fonts/NotoNaskhArabic/NotoNaskhArabic-Regular.ttf", Set.empty, Set.empty)
  loadFont("arabic", "fonts/NotoNaskhArabic/NotoNaskhArabic-Bold.ttf", Set.empty, Set("bold"))

  // Noto Serif Devanagari, the bundled face for Devanagari (Hindi, Marathi, …): \font devanagari 12 sets it,
  // or the alias \font hindi 12, each with a bold cut so \font devanagari 12 bold is a real bold weight. These
  // are static Regular and Bold instances drawn from the variable upstream (wght 400 and 700). Devanagari is
  // left to right, so no bidi is involved; the cluster segmentation and the pre-base short-i reordering are
  // the engine's (see Devanagari and IndicShaper), and the font supplies the conjunct, half-form and vowel-sign
  // glyphs through its GSUB and positions the marks through its GPOS. Loaded by file, like the Hebrew, Arabic
  // and CJK cuts; the filesystem-less in-browser build overrides loadBundledFonts and does not ship them.
  // Devanagari sets its punctuation like a Latin text face — the same curly quotes, en/em dashes and ellipsis,
  // all of which this font carries — so it enables the text representations and the `` ``''/''`` `` and dash
  // shorthands become the proper typographic characters, just as they do in the roman body. (The right-to-left
  // Hebrew and Arabic faces do not: Hebrew's quotes are the mirrored pair and Arabic's are guillemets, so those
  // documents type the marks literally rather than through the ASCII shorthands.)
  loadFont("devanagari", "fonts/NotoSerifDevanagari/NotoSerifDevanagari-Regular.ttf", Ligatures.TEXT_REPRESENTATIONS, Set.empty)
  loadFont("devanagari", "fonts/NotoSerifDevanagari/NotoSerifDevanagari-Bold.ttf", Ligatures.TEXT_REPRESENTATIONS, Set("bold"))
  loadFont("hindi", "fonts/NotoSerifDevanagari/NotoSerifDevanagari-Regular.ttf", Ligatures.TEXT_REPRESENTATIONS, Set.empty)
  loadFont("hindi", "fonts/NotoSerifDevanagari/NotoSerifDevanagari-Bold.ttf", Ligatures.TEXT_REPRESENTATIONS, Set("bold"))

  // Noto Serif Bengali, the bundled face for the Bengali–Assamese script: \font bengali 12 sets it, or the
  // alias \font assamese 12, each with a bold cut. Static Regular and Bold instances drawn from the variable
  // upstream (wght 400 and 700). Bengali is left to right; the engine does the cluster segmentation, the
  // pre-base i/e/ai reordering and the two-part o/au decomposition (see Bengali and IndicShaper), and the font
  // supplies the conjunct, ya-phalaa, reph and vowel-sign glyphs through its GSUB and positions the marks
  // through its GPOS. Loaded by file like the other complex-script cuts; the in-browser build does not ship it.
  // Like the Devanagari face it sets its punctuation as a Latin text face, so the curly-quote and dash
  // shorthands become proper typographic characters in a Bengali run.
  loadFont("bengali", "fonts/NotoSerifBengali/NotoSerifBengali-Regular.ttf", Ligatures.TEXT_REPRESENTATIONS, Set.empty)
  loadFont("bengali", "fonts/NotoSerifBengali/NotoSerifBengali-Bold.ttf", Ligatures.TEXT_REPRESENTATIONS, Set("bold"))
  loadFont("assamese", "fonts/NotoSerifBengali/NotoSerifBengali-Regular.ttf", Ligatures.TEXT_REPRESENTATIONS, Set.empty)
  loadFont("assamese", "fonts/NotoSerifBengali/NotoSerifBengali-Bold.ttf", Ligatures.TEXT_REPRESENTATIONS, Set("bold"))

  // JetBrains Mono — a dedicated code face for setting source code listings in a document, distinct from the
  // typewriter *role* (\texttt, Latin Modern Mono) used for inline code in running text: that one is cut to match
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
  // The small-caps cut of the body face, so \textsc / \scshape / \smallcaps set capitals-and-small-capitals in
  // the same family as the roman text. The oblique cut backs the small-caps shape inside italic context (\emph
  // around \textsc), registered under the {smallcaps, italic} style so that combination resolves too. Small caps
  // gets only the text-representation ligatures (dashes, quotes) and not the f-ligatures (ﬁ, ﬂ, …): the cut has no
  // lowercase f, so it carries no f-ligature glyphs, and applying them would map e.g. "fi" to a glyph the font
  // lacks — dropping the pair from the output (a small-caps "fiable" set as "able").
  loadFont("lmroman", "fonts/LatinModernRoman/lmromancaps10-regular.otf", Ligatures.TEXT_REPRESENTATIONS, Set("smallcaps"))
  loadFont("lmroman", "fonts/LatinModernRoman/lmromancaps10-oblique.otf", Ligatures.TEXT_REPRESENTATIONS, Set("smallcaps", "italic"))

  // The typewriter and sans-serif members of the Latin Modern super-family, registered as the mono and sans
  // *roles* of the same `lmroman` typeface — so \texttt and \textsf select them by setting the role axis while
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
  // the surrounding Latin Modern text.
  loadFont("newcm", "fonts/NewComputerModern/NewCM10-Regular.otf", lmLigatures, Set.empty)
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
  // and can swap one face for another. Bravura is the reference font; Petaluma is a handwritten-style face.
  // Both are SIL OFL. Drawn by glyph index through the same seam math mode uses.
  loadFont("bravura", "fonts/Bravura/Bravura.otf", Set(), Set())
  loadFont("petaluma", "fonts/Petaluma/Petaluma.otf", Set(), Set())
  }

  init(1, 1)
  selectFont("lmroman", 14, Set("regular"))
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

  infix def getVar(name: String): Value = scopes.top.getOrElse(name, sys.error(s"variable '$name' not found"))

  infix def getGlue(name: String): Glue = getVar(name) match
    case Value.Native(g: Glue)           => g
    case Value.Glue(n, st, sh, sto, sho) => Glue(n, st, sh, sto, sho)
    case Value.Dimen(p)                  => Glue(p) // a plain dimension is rigid glue
    case Value.Num(n)                    => Glue(n)
    case v                               => sys.error(s"variable '$name' is not glue: ${Value.display(v)}")

  infix def getNumber(name: String): Double = getVar(name) match
    case Value.Num(n)   => n
    case Value.Dimen(p) => p
    case v              => sys.error(s"variable '$name' is not a number: ${Value.display(v)}")

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

  // The family-role axis selects the typewriter (\texttt) or sans-serif (\textsf) member of the current
  // super-family, keeping the series and slope; serif() clears the role back to the roman default.
  def mono(): Font  = setAxis(StyleAxis.Role, "mono")
  def sans(): Font  = setAxis(StyleAxis.Role, "sans")
  def serif(): Font = setAxis(StyleAxis.Role, "")

  def setStyle(style: Set[String]): Font = selectFont(currentFont.typeface, currentFont.size, style)

  def typeface(name: String): Font = selectFont(name, currentFont.size, Set.empty)

  def setStyle(style: String*): Font = setStyle(style.toSet)

  def addStyle(style: String*): Font = setStyle(currentFont.style ++ style)

  def removeStyle(style: String*): Font = setStyle(currentFont.style -- style)

  def loadFont(typeface: String, path: String, ligatures: Set[String], styleSet: Set[String]): Unit =
    val font = loadFont(path)

    typefaces get typeface match
      case None => typefaces(typeface) = Typeface(mutable.HashMap(styleSet -> (font, ligatures)), None)
      case Some(Typeface(fonts, _)) =>
        if fonts contains styleSet then
          sys.error(s"font for typeface '$typeface' with style '${styleSet.mkString(", ")}' has already been loaded")
        else fonts(styleSet) = (font, ligatures)
  end loadFont

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
      case None                     => sys.error(s"typeface '$typeface' not found")
      case Some(Typeface(fonts, _)) => typefaces(typeface) = Typeface(fonts, Some(baseline))

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
      case None => sys.error(s"font for typeface '$typeface' not found")
      case Some(Typeface(fonts, baseline)) =>
        val wanted = styleSet.map(_.toLowerCase).filterNot(_ == "regular")
        // Resolve the exact style if it was loaded; otherwise substitute the nearest available cut by dropping the
        // least essential axes in turn — small caps first, then slope, then weight — keeping the family role (the
        // mono/sans member) as long as possible. So a mono face with no small-caps or slanted cut falls back to
        // upright mono rather than failing, as LaTeX's font substitution does.
        val (face, ligatures) =
          fonts
            .get(wanted)
            .orElse(LazyList(StyleAxis.Caps, StyleAxis.Slope, StyleAxis.Series)
              .scanLeft(wanted)((k, axis) => k.filterNot(t => axisOf(t) == axis))
              .tail
              .flatMap(fonts.get)
              .headOption)
            .getOrElse(
              sys.error(s"font for typeface '$typeface' with style '${styleSet.mkString(", ")}' has not been loaded"),
            )
        val derivedFont = makeFont(face, size)

        Font(
          typeface,
          size,
          charWidth(derivedFont, ' '),
          getTextExtents("x", derivedFont).height,
          styleSet,
          derivedFont,
          baseline,
          ligatures,
        )

  infix def add(text: String): Typesetter = add(charBox(text))

  def charBox(s: String): CharBox =
    val rep =
      if representations then Ligatures.replace(s, Ligatures.REPRESENTATIONS, currentFont.ligatures) else s

    new CharBox(this, if ligatures then Ligatures(rep, currentFont.ligatures) else rep)

  /** The glyph-fallback font at a given size, if a fallback face is registered — the face used to set codepoints the
    * active font has no glyph for. Built at `size` so the substitute matches the surrounding text's size. */
  def fallbackFontAt(size: Double): Option[Font] =
    fallbackTypeface.filter(typefaces.contains).map(tf => makeFont(tf, size, Set.empty))

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
      fallbackFontAt(primary.size) match
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

  /** The hyphenation language active for this document, or `None` for no hyphenation. This is per-document
    * state — `\usehyphenation` / `\loadhyphenation` / `\language` set it on the typesetter — so two documents
    * (or two concurrent test suites) never affect each other's line breaking. The pattern tables themselves
    * are a shared, append-only cache in the [[Hyphenation]] object; only the *selection* lives here. */
  var hyphenationLanguage: Option[String] = None

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
