package io.github.edadma.texish

//import pprint.pprintln

import io.github.edadma.texish.opentype.{ByteCursor, MathTable}
import io.github.edadma.texish.parser.Value

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized
import scala.language.postfixOps

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

  case class Typeface(
      fonts: mutable.HashMap[Set[String], FontFace],
      baseline: Option[Double],
      ligatures: Set[String],
  )

  /** Shipout-time page decoration: produces the running header and footer boxes (either may be null) for the page
    * being shipped. Evaluated once per page, after `pageno` is set to the shipping page's number, so material like
    * the page number is always current. The language layer installs one that builds the boxes from the `headline`
    * and `footline` macros.
    */
  var pageDecorator: (() => (Box | Null, Box | Null)) | Null = null

  protected[texish] var document: DocumentMode         = new DocumentMode(this)
  protected val typefaces                                  = new mutable.HashMap[String, Typeface]
  private val scopes                                       = mutable.Stack[Map[String, Value]](Map.empty)
  protected[texish] val modeStack: mutable.Stack[Mode] = new mutable.Stack
  var indentParagraph: Boolean                             = false // todo: this should go into page mode maybe
  private var contentInitialized                           = false

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

      init(width, height)
      contentInitialized = true
    }
  }

  def init(width: Double, height: Double): Unit

  def createPageTarget: Any

  def draw(box: Box, xoffset: Double = 0, yoffset: Double = 0): Unit = box.draw(this, xoffset, yoffset + box.ascent)

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

  def getDocument: DocumentMode = document

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

  private val gentiumbookMissing = Set(
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
  // The monospaced face (\texttt, \url): Latin Modern Mono, the Computer Modern typewriter's OpenType
  // successor, so code is set in the same family as the roman body and the math. The four core styles are
  // loaded by file (the .otf names don't follow loadTypeface's "-Style.ttf" convention); bold comes from the
  // companion lmmonolt weight, the only one with a bold cut. No ligatures: code text is set literally.
  loadFont("mono", "fonts/LatinModernMono/lmmono10-regular.otf", Set.empty, Set.empty)
  loadFont("mono", "fonts/LatinModernMono/lmmonolt10-bold.otf", Set.empty, Set("bold"))
  loadFont("mono", "fonts/LatinModernMono/lmmono10-italic.otf", Set.empty, Set("italic"))
  loadFont("mono", "fonts/LatinModernMono/lmmonolt10-boldoblique.otf", Set.empty, Set("bold", "italic"))

  private val alegreyaMissing = Set(
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
  private val lmLigatures = "ﬃﬄﬁﬂﬀ".map(_.toString).toSet ++ Ligatures.TEXT_REPRESENTATIONS
  loadFont("lmroman", "fonts/LatinModernRoman/lmroman10-regular.otf", lmLigatures, Set.empty)
  loadFont("lmroman", "fonts/LatinModernRoman/lmroman10-bold.otf", lmLigatures, Set("bold"))
  loadFont("lmroman", "fonts/LatinModernRoman/lmroman10-italic.otf", lmLigatures, Set("italic"))
  loadFont("lmroman", "fonts/LatinModernRoman/lmroman10-bolditalic.otf", lmLigatures, Set("bold", "italic"))
  loadFont("lmroman", "fonts/LatinModernRoman/lmromanslant10-regular.otf", lmLigatures, Set("slanted"))

  // The default math font: Latin Modern Math, an OpenType font with a full MATH table. Loaded by file
  // directly (not loadTypeface, whose naming assumes a .ttf) since it is a CFF/.otf and stands alone with
  // no style variants. Math mode reads its MATH table through the SFNT seam (see mathTableFor).
  loadFont("lmmath", "fonts/LatinModernMath/LatinModernMath-Regular.otf", Set(), Set())

  // Bravura — the reference font for SMuFL (Standard Music Font Layout). It carries the whole vocabulary of
  // music notation (clefs, noteheads, flags, rests, accidentals, time-signature figures, and more) at fixed
  // codepoints in the Unicode Private Use Area, each drawn to the convention that one staff space is a quarter
  // of the em. The music package sets it with \fontglyph at a size of four staff spaces, so the font's own
  // coordinates map straight onto the staff. Drawn by glyph index through the same seam math mode uses.
  loadFont("bravura", "fonts/Bravura/Bravura.otf", Set(), Set())

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

  def italic(): Unit = addStyle("italic")

  def noitalic(): Unit = removeStyle("italic")

  def bold(): Unit = addStyle("bold")

  def nobold(): Unit = removeStyle("bold")

  def smallcaps(): Unit = addStyle("smallcaps")

  def nosmallcaps(): Unit = removeStyle("smallcaps")

  def setStyle(style: Set[String]): Font = selectFont(currentFont.typeface, currentFont.size, style)

  def typeface(name: String): Font = selectFont(name, currentFont.size, Set.empty)

  def setStyle(style: String*): Font = setStyle(style.toSet)

  def addStyle(style: String*): Font = setStyle(currentFont.style ++ style)

  def removeStyle(style: String*): Font = setStyle(currentFont.style -- style)

  def loadFont(typeface: String, path: String, ligatures: Set[String], styleSet: Set[String]): Unit =
    val font = loadFont(path)

    typefaces get typeface match
      case None => typefaces(typeface) = Typeface(mutable.HashMap(styleSet -> font), None, ligatures)
      case Some(Typeface(fonts, _, _)) =>
        if fonts contains styleSet then
          sys.error(s"font for typeface '$typeface' with style '${styleSet.mkString(", ")}' has already been loaded")
        else fonts(styleSet) = font
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
      case None                                => sys.error(s"typeface '$typeface' not found")
      case Some(Typeface(fonts, _, ligatures)) => typefaces(typeface) = Typeface(fonts, Some(baseline), ligatures)

  def selectFont(typeface: String, size: Double, style: String*): Font = selectFont(typeface, size, style.toSet)

  def selectFont(typeface: String, size: Double, styleSet: Set[String]): Font =
    val f = makeFont(typeface, size, styleSet)

    if f != currentFont then
      currentFont = f
      set("baselineskip", Glue(f.size))

    currentFont

  def makeFont(typeface: String, size: Double, styleSet: Set[String]): Font =
    typefaces get typeface match
      case None => sys.error(s"font for typeface '$typeface' not found")
      case Some(Typeface(fonts, baseline, ligatures)) =>
        val font =
          fonts.getOrElse(
            styleSet map (_.toLowerCase) filterNot (_ == "regular"),
            sys.error(
              s"font for typeface '$typeface' with style '${styleSet.mkString(", ")}' has not been loaded",
            ),
          )
        val derivedFont = makeFont(font, size)

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
    push(new HBoxBuilder(this, toSize, spread))
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

        if indentParagraph then paragraphMode add HSpaceBox(getNumber("parindent"))
      case _ =>

    this

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
      "spaceskip"     -> Glue(currentFont.space, 1),
      "xspaceskip"    -> Glue(currentFont.space * 1.5, 1),
      "hsize"         -> 6.5 * in,
      "vsize"         -> 9 * in,
      "parindent"     -> in / 2,
      "parfillskip"   -> FilGlue,
      "leftskip"      -> ZeroGlue,
      "rightskip"     -> ZeroGlue,
      "parskip"       -> FilGlue,
      "hangindent"    -> 0.0,
      "hangafter"     -> 1.0,
      "hoffset"       -> 1 * in,
      "voffset"       -> 1 * in,
      "pageno"        -> 1.0,
      "headsep"       -> in / 4, // bottom of the running header to the top of the body
      "footskip"      -> in / 4, // bottom of the body to the top of the running footer
      "topskip"       -> Glue(currentFont.size * 1.2 * pt), // distance from the page top to the first baseline
      "raggedbottom"  -> 0.0, // nonzero: pad page bottoms with fil instead of stretching the page's glue

      // display math ($$…$$): the glue set above and below a displayed formula
      "abovedisplayskip" -> Glue(currentFont.size * 0.83 * pt, currentFont.size * 0.17 * pt, currentFont.size * 0.17 * pt),
      "belowdisplayskip" -> Glue(currentFont.size * 0.83 * pt, currentFont.size * 0.17 * pt, currentFont.size * 0.17 * pt),

      // footnotes (see InsertBox): the space above the separator rule, the footnote text size as a fraction
      // of the surrounding font, and the footnote counter advanced by each \footnote
      "footnotesep"  -> Glue(currentFont.size * pt),
      "footnotesize" -> 0.8,
      "footnoteno"   -> 0.0,

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

      // Page-break penalties between the lines of a paragraph. The page builder is first-fit, so values between
      // 0 and 9999 don't bias anything — what matters is 10000 (forbid) and -10000 (force). The defaults forbid
      // orphans (a paragraph's first line alone at a page bottom) and widows (its last line alone at a page top).
      "clubpenalty"      -> 10000.0, // against breaking after a paragraph's first line
      "widowpenalty"     -> 10000.0, // against breaking before a paragraph's last line
      "interlinepenalty" -> 0.0,     // between all other lines

      //
      "imageScaling" -> 1.0,
      "pagewidth"    -> 8.5 * in,
      "pageheight"   -> 11 * in,
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
