package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, DocumentMode, HBox, HeadlessTypesetter, ImageBox, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** \includegraphics places a raster image, sized by an optional key=value list. On the stub the source image
  * is 1×1, so the requested width/height/scale land directly on the resulting ImageBox, where they are read
  * back. \linewidth resolves to the current hsize (6.5in = 468pt by default).
  */
class IncludeGraphicsTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def fixture(): (HeadlessTypesetter, CapturingDocument, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    (t, doc, proc)

  private def render(src: String): Seq[Box] =
    val (t, doc, proc) = fixture()
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\set raggedbottom {1}\n" + src + "\n"))
    t.end()
    doc.shipped.toSeq

  private def images(b: Box): List[ImageBox] = b match
    case i: ImageBox => List(i)
    case h: HBox     => h.boxes.toList.flatMap(images)
    case v: VBox     => v.boxes.toList.flatMap(images)
    case _           => Nil

  private def firstImage(src: String): ImageBox = render(src).flatMap(images).head

  "with no options the image keeps its natural size" in {
    val img = firstImage("\\includegraphics{frog.png}")
    img.width shouldBe 1.0
    img.ascent shouldBe 1.0
  }

  "width sets the width and scales the height to keep the aspect ratio" in {
    val img = firstImage("\\includegraphics[width=200pt]{frog.png}")
    img.width shouldBe 200.0
    img.ascent shouldBe 200.0 // square source, so height tracks width
  }

  "height sets the height and scales the width" in {
    val img = firstImage("\\includegraphics[height=50pt]{frog.png}")
    img.ascent shouldBe 50.0
    img.width shouldBe 50.0
  }

  "width and height together set both independently" in {
    val img = firstImage("\\includegraphics[width=100pt,height=40pt]{frog.png}")
    img.width shouldBe 100.0
    img.ascent shouldBe 40.0
  }

  "scale multiplies the natural size" in {
    val img = firstImage("\\includegraphics[scale=3]{frog.png}")
    img.width shouldBe 3.0
    img.ascent shouldBe 3.0
  }

  "a factor times \\linewidth resolves to that fraction of hsize" in {
    val img = firstImage("\\includegraphics[width=0.5\\linewidth]{frog.png}")
    img.width shouldBe 234.0 // 0.5 * 6.5in (468pt)
  }

  "a bare \\linewidth means the full line width" in {
    val img = firstImage("\\includegraphics[width=\\linewidth]{frog.png}")
    img.width shouldBe 468.0
  }

  "an unknown option is an error" in {
    val (_, _, proc) = fixture()
    intercept[ParserException](proc.process("\\includegraphics[depth=2pt]{frog.png}"))
  }

  "a malformed length is an error" in {
    val (_, _, proc) = fixture()
    intercept[ParserException](proc.process("\\includegraphics[width=wide]{frog.png}"))
  }
