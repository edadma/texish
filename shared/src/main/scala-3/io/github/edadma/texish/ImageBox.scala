package io.github.edadma.texish

/** A box holding a raster image. The natural size is the image's pixel dimensions scaled by `imageScaling`.
  * A requested `width` and/or `height` (in points) overrides that size; giving only one of them scales the
  * other to preserve the aspect ratio, as `\includegraphics[width=…]` does. `scale` multiplies the natural
  * size and applies only when neither an explicit width nor height is given. */
class ImageBox(
    t: Typesetter,
    path: String,
    reqWidth: Option[Double] = None,
    reqHeight: Option[Double] = None,
    scale: Option[Double] = None,
) extends NoGlueBox:
  private val (image, imageWidth, imageHeight) = t.loadImage(path)
  private val imageScaling                     = t.getNumber("imageScaling")

  private val naturalWidth  = imageWidth * imageScaling
  private val naturalHeight = imageHeight * imageScaling

  val (width: Double, ascent: Double) = (reqWidth, reqHeight) match
    case (Some(w), Some(h)) => (w, h)
    case (Some(w), None)    => (w, if naturalWidth == 0 then 0.0 else w * naturalHeight / naturalWidth)
    case (None, Some(h))    => (if naturalHeight == 0 then 0.0 else h * naturalWidth / naturalHeight, h)
    case (None, None) =>
      val s = scale.getOrElse(1.0)
      (naturalWidth * s, naturalHeight * s)

  val descent: Double  = 0
  val xAdvance: Double = width

  // (x, y) is the box's reference point on the baseline; the image fills the rectangle of size width × ascent
  // sitting on it, so its top-left is (x, y - ascent). The backend scales the source pixels to that point size.
  def draw(t: Typesetter, x: Double, y: Double): Unit =
    t.drawImage(image.asInstanceOf[t.ImageHandle], x, y - ascent, width, ascent)

/** Like [[ImageBox]], but for an image already built in memory (e.g. an inline `\defbitmap`) rather than loaded
  * from a file: it is handed the backend image handle and its pixel size directly, and sizes itself the same way. */
class HandleImageBox(
    t: Typesetter,
    image: Any,
    imageWidth: Int,
    imageHeight: Int,
    reqWidth: Option[Double] = None,
    reqHeight: Option[Double] = None,
    scale: Option[Double] = None,
) extends NoGlueBox:
  private val imageScaling = t.getNumber("imageScaling")
  private val naturalWidth  = imageWidth * imageScaling
  private val naturalHeight = imageHeight * imageScaling

  val (width: Double, ascent: Double) = (reqWidth, reqHeight) match
    case (Some(w), Some(h)) => (w, h)
    case (Some(w), None)    => (w, if naturalWidth == 0 then 0.0 else w * naturalHeight / naturalWidth)
    case (None, Some(h))    => (if naturalHeight == 0 then 0.0 else h * naturalWidth / naturalHeight, h)
    case (None, None) =>
      val s = scale.getOrElse(1.0)
      (naturalWidth * s, naturalHeight * s)

  val descent: Double  = 0
  val xAdvance: Double = width

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    t.drawImage(image.asInstanceOf[t.ImageHandle], x, y - ascent, width, ascent)
