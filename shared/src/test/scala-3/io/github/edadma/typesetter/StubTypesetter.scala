package io.github.edadma.typesetter

/** A headless Typesetter with fixed metrics, for testing the engine and language layer without a rendering backend or
  * font files. Every character is 6 units wide; drawing is a no-op.
  */
class StubTypesetter extends Typesetter:

  type ImageHandle = Unit

  val output = ""

  def init(width: Double, height: Double): Unit = ()

  def createPageTarget: Any = ()

  def ejectPageTarget(): Unit = ()

  def setFont(font: Any): Unit = ()

  def setColor(color: Color): Unit = ()

  def setLineWidth(width: Double): Unit = ()

  def drawString(text: String, x: Double, y: Double): Unit = ()

  def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = ()

  def drawRect(x: Double, y: Double, width: Double, height: Double): Unit = ()

  def fillRect(x: Double, y: Double, width: Double, height: Double): Unit = ()

  def loadFont(path: String): Any = path

  def getTextExtents(text: String, font: Any): TextExtents =
    TextExtents(0, -8, text.length * 6, 10, text.length * 6, 0)

  def makeFont(font: Any, size: Double): Any = font

  def charWidth(font: Any, c: Char): Double = 6

  def loadImage(path: String): (ImageHandle, Int, Int) = ((), 1, 1)

  def drawImage(image: ImageHandle, x: Double, y: Double): Unit = ()

  def destroy(): Unit = ()
