package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, CharBox, DocumentMode, GlyphBox, HBox, HeadlessTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** \[ ... \] are LaTeX's display-math delimiters, equivalent to the engine's $$ ... $$: \[ opens a display
  * formula and \] closes it. Unlike the toggling $$, they are distinct tokens, so each guards against misuse.
  * The non-breaking tie ~ (an active character) puts an unbreakable space between two words. Both are checked
  * on the fixed-metric stub, where glyph indices are the codepoint itself.
  */
class DisplayMathTests extends AnyFreeSpec with Matchers:

  // U+00A0 NO-BREAK SPACE, the character the active ~ emits.
  private val nbsp: Char = 0x00a0.toChar

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

  private def glyphs(b: Box): List[Int] = b match
    case g: GlyphBox => List(g.glyph)
    case h: HBox     => h.boxes.toList.flatMap(glyphs)
    case v: VBox     => v.boxes.toList.flatMap(glyphs)
    case _           => Nil

  private def texts(b: Box): String = b match
    case c: CharBox => c.text
    case h: HBox    => h.boxes.toList.map(texts).mkString
    case v: VBox    => v.boxes.toList.map(texts).mkString
    case _          => ""

  private def allGlyphs(boxes: Seq[Box]): List[Int] = boxes.toList.flatMap(glyphs)
  private def allText(boxes: Seq[Box]): String       = boxes.map(texts).mkString

  "\\[ ... \\] sets a display formula" in {
    // '=' is set as the math relation U+003D and the digit 1 upright; their presence proves the math ran
    val gs = allGlyphs(render("\\[ x = 1 \\]"))
    gs should contain(0x3d)      // '=' relation
    gs should contain('1'.toInt) // upright digit
  }

  "a display formula set with \\[ matches the $$ form" in {
    allGlyphs(render("\\[ a + b \\]")) shouldBe allGlyphs(render("$$ a + b $$"))
  }

  "\\] without a matching \\[ is an error" in {
    val (_, _, proc) = fixture()
    intercept[ParserException](proc.process("text \\]"))
  }

  "\\[ inside math mode is an error" in {
    val (_, _, proc) = fixture()
    intercept[ParserException](proc.process("$ x \\[ y $"))
  }

  "~ ties two words with a non-breaking space" in {
    // the active ~ emits U+00A0, an unbreakable space riding the text path between the two words
    allText(render("Figure~1")).exists(_ == nbsp) shouldBe true
  }
