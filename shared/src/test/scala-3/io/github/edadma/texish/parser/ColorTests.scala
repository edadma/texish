package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, CharBox, Color, DocumentMode, HBox, HeadlessTypesetter, VBox}
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

  private def render(src: String): Seq[Box] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\set raggedbottom {1}\n" + src + "\n"))
    t.end()
    doc.shipped.toSeq

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
    intercept[ParserException](proc.process("\\color{notacolour}X"))
  }
