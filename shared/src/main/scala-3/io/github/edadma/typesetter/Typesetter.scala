package io.github.edadma.typesetter

import pprint.pprintln

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized
import scala.language.postfixOps

abstract class Typesetter:

  val output: String

  var debug: Boolean           = false
  var currentFont: Font        = uninitialized
  var currentColor: Color      = Color("black")
  val converter                = UnitConverter(this)
  val pages                    = new ArrayBuffer[Any]
  var ligatures: Boolean       = true
  var representations: Boolean = true

  case class Typeface(
      fonts: mutable.HashMap[Set[String], Any], // todo: find a nicer target independent type other than Any
      baseline: Option[Double],
      ligatures: Set[String],
  )

  protected[typesetter] var document: Document = uninitialized
  protected val typefaces                      = new mutable.HashMap[String, Typeface]
  private val scopes                           = mutable.Stack[Map[String, Any]](Map.empty)
  /*protected[typesetter]*/
  protected[typesetter] val modeStack: mutable.Stack[Mode] = mutable.Stack(null) // gets set by setDocument
  var indentParagraph: Boolean = true // todo: this should go into page mode maybe

  def initTarget(): Unit

  def createPageTarget(width: Double, height: Double): Any

  def draw(box: Box, xoffset: Double = 0, yoffset: Double = 0): Unit = box.draw(this, xoffset, yoffset + box.ascent)

  def ejectPageTarget(): Unit

  def getDPI: Double

  def setFont(font: Any): Unit

  def setColor(color: Color): Unit

  def drawString(text: String, x: Double, y: Double): Unit

  def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit

  def drawRect(x: Double, y: Double, width: Double, height: Double): Unit

  def fillRect(x: Double, y: Double, width: Double, height: Double): Unit

  def loadFont(path: String): Any

  def getTextExtents(text: String, font: Any): TextExtents

  def makeFont(font: Any, size: Double): Any

  def charWidth(font: Any, c: Char): Double

  def loadImage(path: String): (Any, Int, Int)

  def drawImage(image: Any, x: Double, y: Double): Unit

  def destroy(): Unit

  def setDocument(d: Document): Unit = {
    document = d
    modeStack(modeStack.length - 1) = d
    d.ts = this
    d.init()
  }

  def getDocument: Document = document

  initTarget()

  loadTypeface(
    "noto",
    "fonts/NotoSerif/NotoSerif",
    "\uFB03\uFB04\uFB01\uFB02",
    Set(),
    "Regular",
    "Bold",
    "Italic",
    ("Bold", "Italic"),
  )
  loadTypeface(
    "noto",
    "fonts/NotoSans/NotoSans",
    "\uFB03\uFB04\uFB01\uFB02\uFB00",
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
    "\uFB03\uFB04\uFB01\uFB02\uFB00",
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
  loadTypeface("pt", "fonts/PTSansNarrow/PTSansNarrow", "\uFB01\uFB02", Set(), "Regular", "Bold")
  loadTypeface(
    "mono",
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

  selectFont("gentiumbook", 18, Set("regular"))
  set(defaultParameters)
  setDocument(new SimpleDocument)
  modeStack push new PageMode(this)

  def mode: Mode = modeStack.top

  def setFont(f: Font): Unit = setFont(f.renderFont)

  def in: Double = getDPI

  def pt: Double = in / 72

  def cm: Double = in / 2.54

  def mm: Double = cm / 10

  infix def get(name: String): Any = scopes.top.getOrElse(name, UNDEFINED)

  infix def getGlue(name: String): Glue = get(name).asInstanceOf[Glue]

  infix def getNumber(name: String): Double = get(name).asInstanceOf[Double]

  def set(name: String, value: Any): Unit = scopes(0) += (name -> value)

  def set(pairs: Seq[(String, Any)]): Unit =
    scopes(0) ++=
      pairs map {
        case (k, n: Number) => (k, n.doubleValue)
        case (k, v)         => (k, v)
      }

  def enter(): Unit = scopes push scopes.top

  def exit(): Unit =
    scopes.pop

    scopes.top get "font" match
      case Some(font: Font) => currentFont = font
      case Some(o)          => sys.error(s"font object has wrong type: '${o.getClass}'")
      case None             =>

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
      if scopes.size > 1 && !scopes(1).contains("font") then scopes(1) += ("font" -> currentFont)

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
    mode add box
    this

  def glue(naturalWidth: Double, stretch: Double = 0, shrink: Double = 0): Typesetter =
    add(Glue(naturalWidth, stretch, shrink))

  def fil: Typesetter = add(FilGlue)

  def fill: Typesetter = add(FillGlue)

  def noBreakSpace: Typesetter = add(getGlue("spaceskip").noBreak)

  def result: Box = mode.result

  def pop(): Unit = modeStack.pop

  def hbox(toSize: Double | Null = null): Typesetter =
    modeStack push new HBoxBuilder(this, toSize)
    this

  def image(path: String): Typesetter =
    add(new ImageBox(this, path))
    this

  def done(): Unit =
    mode.done()

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

  def start(): Unit =
    mode match
      case v: VerticalMode =>
        val paragraphMode = new ParagraphMode(this)

        modeStack push paragraphMode

        if indentParagraph then paragraphMode add HSpaceBox(getNumber("parindent"))
      case _ =>

  def indent(): Unit =
    indentParagraph = true
    start()

  def noindent(): Unit =
    indentParagraph = false
    start()

  private def defaultParameters =
    List(
      "baselineskip" -> Glue(
        currentFont.size,
          /** 1.2 */
//          * pt,
      ),
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
