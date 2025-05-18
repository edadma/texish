package io.github.edadma.typesetter

class ImageBox(t: Typesetter, path: String, scaledWidth: Option[Double] = None, scaledHeight: Option[Double] = None)
    extends NoGlueBox:
  private val (image, imageWidth, imageHeight) = t.loadImage(path)
  private val imageScaling                     = t.getNumber("imageScaling")

  val width: Double    = scaledWidth getOrElse imageWidth * imageScaling
  val ascent: Double   = scaledHeight getOrElse imageHeight * imageScaling
  val descent: Double  = 0
  val xAdvance: Double = width

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    t.drawImage(image.asInstanceOf[t.ImageHandle], x / imageScaling, (y - ascent) / imageScaling)
