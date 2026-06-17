package io.github.edadma.texish

/** A transparent wrapper that turns its content into a clickable hyperlink. It has exactly the dimensions of
  * the box it wraps, so it takes part in line layout as if it were that box; at draw time it brackets the
  * content's drawing with a link annotation carrying `uri`. Only a backend that emits annotations (the PDF
  * output) acts on it — see [[Typesetter.beginLink]]; every other backend draws the content plainly. */
class LinkBox(val box: Box, val uri: String) extends ContentBox:

  val ascent: Double   = box.ascent
  val descent: Double  = box.descent
  val width: Double    = box.width
  val xAdvance: Double = box.xAdvance

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    t.beginLink(uri)
    box.draw(t, x, y)
    t.endLink()

  override def toString: String = s"LinkBox(uri=$uri, box=$box)"
