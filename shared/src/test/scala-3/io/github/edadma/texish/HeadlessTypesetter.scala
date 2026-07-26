package io.github.edadma.texish

/** A headless Typesetter with fixed metrics, for testing the engine and language layer without a rendering backend or
  * font files. Every character is 6 units wide; drawing is a no-op.
  *
  * Loads the bundled catalogue by default, because most tests are about the engine rather than about which faces an
  * installation has, and they run against the source tree's own `fonts/` folder. Pass `catalogue = false` for a test
  * that is specifically about the bare installation — a library consumer with the embedded core and nothing else.
  */
class HeadlessTypesetter(catalogue: Boolean = true) extends Typesetter:

  if catalogue then loadBundledCatalogue()


  type ImageHandle = Unit
  type FontFace    = String
  type RenderFont  = String

  val output = ""

  def init(width: Double, height: Double): Unit = ()

  def createPageTarget: Any = ()

  def ejectPageTarget(): Unit = ()

  def setFont(font: RenderFont): Unit = ()

  def setColor(color: Color): Unit = ()

  def setLineWidth(width: Double): Unit = ()

  def drawString(text: String, x: Double, y: Double): Unit = ()

  def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = ()

  def drawRect(x: Double, y: Double, width: Double, height: Double): Unit = ()

  def fillRect(x: Double, y: Double, width: Double, height: Double): Unit = ()

  def loadFont(path: String): FontFace = path

  def loadFontBytes(bytes: Array[Byte], name: String): FontFace = name

  def getTextExtents(text: String, font: RenderFont): TextExtents =
    TextExtents(0, -8, text.length * 6, 10, text.length * 6, 0)

  def makeFont(font: FontFace, size: Double): RenderFont = font

  def charWidth(font: RenderFont, c: Char): Double = 6

  // Glyph seam: indices are the codepoint itself, and every glyph has the same fixed metrics as a
  // character. drawGlyph is a no-op here; a recording subclass can override it to capture placements.
  def glyphIndex(font: RenderFont, codepoint: Int): Int = codepoint

  def glyphExtents(font: RenderFont, glyph: Int): TextExtents = TextExtents(0, -8, 6, 10, 6, 0)

  def drawGlyph(font: RenderFont, glyph: Int, x: Double, y: Double): Unit = ()

  def sfntTable(font: RenderFont, tag: String): Option[Array[Byte]] = None

  def loadImage(path: String): (ImageHandle, Int, Int) = ((), 1, 1)

  def drawImage(image: ImageHandle, x: Double, y: Double, w: Double, h: Double): Unit = ()

  def destroy(): Unit = ()
