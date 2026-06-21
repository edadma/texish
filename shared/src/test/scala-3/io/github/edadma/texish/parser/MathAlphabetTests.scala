package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, DocumentMode, GlyphBox, HBox, HeadlessTypesetter, MathAlphabet, MathSymbols, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The math alphabets \mathbf, \mathit, \mathrm, \mathsf, \mathtt, \mathbb and \mathfrak each remap a letter to
  * its codepoint in a Mathematical Alphanumeric block. The remapping is checked at the symbol-table level
  * (contiguous arithmetic, the Letterlike-block exceptions, and which alphabets carry digit shapes) and the
  * primitives are checked end-to-end on the fixed-metric stub, where a glyph index is the codepoint itself.
  */
class MathAlphabetTests extends AnyFreeSpec with Matchers:

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

  // ---- the codepoint mapping on its own --------------------------------------

  "bold maps letters and digits in the contiguous block" in {
    MathSymbols.alphabetCodepoint(MathAlphabet.Bold, 'A') shouldBe Some(0x1D400)
    MathSymbols.alphabetCodepoint(MathAlphabet.Bold, 'x') shouldBe Some(0x1D431)
    MathSymbols.alphabetCodepoint(MathAlphabet.Bold, '5') shouldBe Some(0x1D7D3)
  }

  "italic small h is the Planck-constant slot, and italic has no digit shapes" in {
    MathSymbols.alphabetCodepoint(MathAlphabet.Italic, 'h') shouldBe Some(0x210E)
    MathSymbols.alphabetCodepoint(MathAlphabet.Italic, 'x') shouldBe Some(0x1D465)
    MathSymbols.alphabetCodepoint(MathAlphabet.Italic, '5') shouldBe None
  }

  "roman is upright ASCII" in {
    MathSymbols.alphabetCodepoint(MathAlphabet.Roman, 'A') shouldBe Some(0x41)
    MathSymbols.alphabetCodepoint(MathAlphabet.Roman, '5') shouldBe Some(0x35)
  }

  "sans-serif and typewriter map into their own blocks" in {
    MathSymbols.alphabetCodepoint(MathAlphabet.SansSerif, 'A') shouldBe Some(0x1D5A0)
    MathSymbols.alphabetCodepoint(MathAlphabet.Typewriter, 'a') shouldBe Some(0x1D68A)
  }

  "blackboard-bold uses the Letterlike-block exceptions" in {
    MathSymbols.alphabetCodepoint(MathAlphabet.BlackboardBold, 'A') shouldBe Some(0x1D538) // contiguous
    MathSymbols.alphabetCodepoint(MathAlphabet.BlackboardBold, 'R') shouldBe Some(0x211D)  // exception (ℝ)
    MathSymbols.alphabetCodepoint(MathAlphabet.BlackboardBold, 'N') shouldBe Some(0x2115)  // exception (ℕ)
    MathSymbols.alphabetCodepoint(MathAlphabet.BlackboardBold, '1') shouldBe Some(0x1D7D9)
  }

  "fraktur uses the Letterlike-block exceptions and has no digit shapes" in {
    MathSymbols.alphabetCodepoint(MathAlphabet.Fraktur, 'A') shouldBe Some(0x1D504) // contiguous
    MathSymbols.alphabetCodepoint(MathAlphabet.Fraktur, 'H') shouldBe Some(0x210C)  // exception (ℌ)
    MathSymbols.alphabetCodepoint(MathAlphabet.Fraktur, '5') shouldBe None
  }

  "a non-alphanumeric character has no alphabet form" in {
    MathSymbols.alphabetCodepoint(MathAlphabet.Bold, '+') shouldBe None
  }

  // ---- the primitives end-to-end ---------------------------------------------

  "\\mathbf sets a bold letter" in {
    allGlyphs(render("$\\mathbf{x}$")) should contain(0x1D431)
  }

  "\\mathbb reaches the blackboard-bold reals through the exception" in {
    allGlyphs(render("$\\mathbb{R}$")) should contain(0x211D)
  }

  "\\mathfrak reaches a fraktur letter through the exception" in {
    allGlyphs(render("$\\mathfrak{H}$")) should contain(0x210C)
  }

  "\\mathsf and \\mathtt set their letters in the sans and mono blocks" in {
    allGlyphs(render("$\\mathsf{A}$")) should contain(0x1D5A0)
    allGlyphs(render("$\\mathtt{a}$")) should contain(0x1D68A)
  }

  "\\mathrm sets letters upright" in {
    allGlyphs(render("$\\mathrm{A}$")) should contain(0x41)
  }

  "a digit inside \\mathit falls back to its ordinary upright shape" in {
    // italic has no digit form, so the 5 stays at its plain codepoint rather than vanishing
    allGlyphs(render("$\\mathit{x5}$")) should (contain(0x1D465) and contain(0x35))
  }
