package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, CharBox, Color, DocumentMode, FrameBox, HBox, HeadlessTypesetter, TexishException, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** \color sets the pen colour for the rest of the current group; \textcolor sets it for just its argument. Both
  * are checked by rendering on the stub and reading the colour each CharBox captured at creation. The colour
  * must apply to the right text and revert at the group's close.
  */
class ColorTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def renderT(src: String): (HeadlessTypesetter, Seq[Box]) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\set raggedbottom {1}\n" + src + "\n"))
    t.end()
    (t, doc.shipped.toSeq)

  private def render(src: String): Seq[Box] = renderT(src)._2

  /** The fill colour of the first \colorbox / \fcolorbox FrameBox in the output. */
  private def fillOf(boxes: Seq[Box]): Color =
    def frames(b: Box): List[FrameBox] = b match
      case f: FrameBox => List(f)
      case h: HBox     => h.boxes.toList.flatMap(frames)
      case v: VBox     => v.boxes.toList.flatMap(frames)
      case _           => Nil
    boxes.toList.flatMap(frames).collectFirst { case f if f.bgColor != null => f.bgColor.asInstanceOf[Color] }.get

  private def chars(b: Box): List[(String, Color)] = b match
    case c: CharBox => List((c.text, c.color))
    case h: HBox    => h.boxes.toList.flatMap(chars)
    case v: VBox    => v.boxes.toList.flatMap(chars)
    case _          => Nil

  /** The colour of the first CharBox whose text contains `mark`. */
  private def colorOf(boxes: Seq[Box], mark: String): Color =
    boxes.toList.flatMap(chars).collectFirst { case (s, c) if s.contains(mark) => c }.get

  private val blue  = Color("blue")
  private val red   = Color("red")
  private val black = Color("black")

  "\\textcolor sets the colour of its argument and reverts after" in {
    val boxes = render("\\textcolor{blue}{Q} R")
    colorOf(boxes, "Q") shouldBe blue
    colorOf(boxes, "R") shouldBe black
  }

  "\\color sets the colour for the rest of its group and reverts at the close" in {
    val boxes = render("{\\color{red}S} T")
    colorOf(boxes, "S") shouldBe red
    colorOf(boxes, "T") shouldBe black
  }

  "a #RRGGBB hex code is accepted" in {
    val boxes = render("\\textcolor{#0000ff}{Z} W")
    colorOf(boxes, "Z") shouldBe blue // #0000ff is blue
  }

  "an unknown colour name is an error" in {
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    intercept[TexishException](proc.process("\\color{notacolour}X"))
  }

  "an #RRGGBBAA hex code carries its own alpha" in {
    val c = Color("#20304080")
    c.redInt shouldBe 0x20
    c.greenInt shouldBe 0x30
    c.blueInt shouldBe 0x40
    c.alpha shouldBe (0x80 / 255.0 +- 1e-9)
  }

  "the transparent keyword is fully clear" in {
    Color("transparent") shouldBe Color.TRANSPARENT
    Color("transparent").alpha shouldBe 0.0
  }

  "a named colour takes the alpha parameter" in {
    Color("white", 0.5).alpha shouldBe (0.5 +- 1e-9)
  }

  "a translucent pen colour reaches the CharBox through an 8-digit code" in {
    val z = colorOf(render("\\textcolor{#0000ff80}{Z} W"), "Z")
    z.blueInt shouldBe 0xff
    z.alpha shouldBe (0x80 / 255.0 +- 1e-9)
  }

  "\\color's optional [alpha] makes the pen translucent and still reverts" in {
    val boxes = render("{\\color[0.5]{red}S} T")
    val s     = colorOf(boxes, "S")
    s.redInt shouldBe 0xff
    s.alpha shouldBe (0.5 +- 1e-9)
    colorOf(boxes, "T") shouldBe black // reverts at the group close, fully opaque
  }

  "\\colorbox fills translucently from an [alpha] or an 8-digit code" in {
    fillOf(render("\\colorbox[0.4]{black}{x}")).alpha shouldBe (0.4 +- 1e-9)
    fillOf(render("\\colorbox{#00000066}{x}")).alpha shouldBe (0x66 / 255.0 +- 1e-9)
  }

  "\\pagecolor sets the page background colour, with alpha and transparent" in {
    val (opaque, _) = renderT("\\pagecolor[0.6]{black} x")
    opaque.backgroundColor.redInt shouldBe 0x00
    opaque.backgroundColor.alpha shouldBe (0.6 +- 1e-9)

    val (clear, _) = renderT("\\pagecolor{transparent} x")
    clear.backgroundColor shouldBe Color.TRANSPARENT
  }
