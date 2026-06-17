package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, CharBox, DocumentMode, GlyphBox, HBox, HeadlessTypesetter, MathAtom, MathFont, MathSymbols, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** \text sets a run of ordinary words upright inside a formula (through the text/string path, in the body font),
  * and \mathcal sets its letters in the calligraphic (script) alphabet. The script alphabet is checked at the
  * symbol-table level (the codepoint remapping, including the Letterlike-block exceptions) and both primitives
  * are checked end-to-end on the fixed-metric stub: \mathcal emits a glyph at the script codepoint (glyph indices
  * are the codepoint itself on the stub), and \text emits CharBoxes carrying the literal words.
  */
class MathTextCalTests extends AnyFreeSpec with Matchers:

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

  private def texts(b: Box): String = b match
    case c: CharBox => c.text
    case h: HBox    => h.boxes.toList.map(texts).mkString
    case v: VBox    => v.boxes.toList.map(texts).mkString
    case _          => ""

  private def allGlyphs(boxes: Seq[Box]): List[Int] = boxes.toList.flatMap(glyphs)
  private def allText(boxes: Seq[Box]): String       = boxes.map(texts).mkString

  // a math font over the stub's current (text) font: no MATH table, fixed 6-wide glyphs
  private def mathFont(t: HeadlessTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  "scriptLetter maps the contiguous block and the Letterlike-block exceptions" in {
    MathSymbols.scriptLetter('A') shouldBe Some(0x1D49C) // contiguous capital
    MathSymbols.scriptLetter('N') shouldBe Some(0x1D4A9)
    MathSymbols.scriptLetter('B') shouldBe Some(0x212C)  // exception (SCRIPT CAPITAL B)
    MathSymbols.scriptLetter('L') shouldBe Some(0x2112)  // exception
    MathSymbols.scriptLetter('a') shouldBe Some(0x1D4B6) // contiguous small
    MathSymbols.scriptLetter('e') shouldBe Some(0x212F)  // exception (SCRIPT SMALL E)
    MathSymbols.scriptLetter('7') shouldBe None          // no script form
  }

  "mathcalNode builds an ordinary atom at the script codepoint" in {
    val mf = mathFont(new HeadlessTypesetter)
    MathSymbols.mathcalNode(mf, 'N'.toInt) match
      case Some(a: MathAtom) => a.nucleusCp shouldBe Some(0x1D4A9)
      case other             => fail(s"expected a script atom, got $other")
    MathSymbols.mathcalNode(mf, '7'.toInt) shouldBe None
  }

  "\\mathcal sets its letter in the script alphabet" in {
    allGlyphs(render("$\\mathcal{N}$")) should contain(0x1D4A9)
  }

  "\\mathcal handles a Letterlike-block letter" in {
    allGlyphs(render("$\\mathcal{B}$")) should contain(0x212C)
  }

  "\\text sets words upright through the text path inside math" in {
    // the words ride the string seam as CharBoxes, so they survive as literal text in the box tree
    allText(render("$x \\text{ for } y$")) should include("for")
  }

  "\\text and \\mathcal compose in one formula" in {
    val boxes = render("$\\mathcal{N}(0, \\text{var})$")
    allGlyphs(boxes) should contain(0x1D4A9)
    allText(boxes) should include("var")
  }
