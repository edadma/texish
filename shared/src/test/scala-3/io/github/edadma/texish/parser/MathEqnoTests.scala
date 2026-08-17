package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, DocumentMode, GlyphBox, HBox, HeadlessTypesetter, TexishException, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Equation numbers on a display: `\eqno` flushes the number to the right margin and `\leqno` to the left, which
  * is the only difference between them. Which side a document numbers on is a house style — set once, by the
  * macro that wraps the display — so the side is chosen by which command is used rather than per equation.
  *
  * The number is set in math at text size, and the formula stays centred on the measure either way, because the
  * number is placed outside the two fils that centre it.
  */
class MathEqnoTests extends AnyFreeSpec with Matchers:

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

  private def glyphs(b: Box): List[Int] = b match
    case g: GlyphBox => List(g.glyph)
    case h: HBox     => h.boxes.toList.flatMap(glyphs)
    case v: VBox     => v.boxes.toList.flatMap(glyphs)
    case _           => Nil

  private def allGlyphs(boxes: Seq[Box]): List[Int] = boxes.toList.flatMap(glyphs)

  private def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  "\\eqno sets the number after the formula, \\leqno before it" in {
    // glyph indices are the codepoint itself on the stub, so the drawn order reads directly. A letter in math is
    // set from the math-italic block, so the formula's x is U+1D465 rather than ASCII x.
    val x    = 0x1D465
    val open = '('.toInt

    val right = allGlyphs(render("$$x \\eqno (1)$$"))
    val left  = allGlyphs(render("$$x \\leqno (1)$$"))

    right.indexOf(x) should be < right.indexOf(open) // formula first, number flushed right after it
    left.indexOf(open) should be < left.indexOf(x)   // number flushed left, formula after it

    right.sorted shouldBe left.sorted // the same material either way; only the order differs
  }

  "both number a display without disturbing what follows" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("$$a + b = c \\eqno (3.1)$$ and text after.")

    val (_, p2) = fixture()
    noException should be thrownBy p2.process("$$a + b = c \\leqno (3.1)$$ and text after.")
  }

  "neither is allowed in inline math" in {
    for name <- Seq("eqno", "leqno") do
      val (_, proc) = fixture()
      val ex        = the[TexishException] thrownBy proc.process(s"$$x \\$name (1)$$")
      ex.getMessage should include("only allowed in display math")
  }
