package io.github.edadma.texish.parser

import io.github.edadma.texish.{Builder, CharBox, HBox, HSpaceBox, HeadlessTypesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream

// \noindent opens a paragraph with no first-line indent. When a heading macro ends with \noindent and a blank
// line follows in the document, that \noindent opens an *empty* paragraph which the blank line then closes.
// Closing an empty paragraph must leave the no-indent state in place (and emit no line), so the real paragraph
// that follows is still flush left — this is the "no indent after a heading" behaviour.
class NoindentParagraphTests extends AnyFreeSpec with Matchers:

  // Every line opens with a zero-width \leftskip glue box; the first-line indent, when present, is a rigid
  // HSpaceBox of \parindent width (36 in the stub's default geometry) sitting before the first character. A
  // real indent is thus a wide HSpaceBox somewhere in the run of boxes preceding the first CharBox.
  private def startsIndented(line: HBox): Boolean =
    line.boxes
      .takeWhile(!_.isInstanceOf[CharBox])
      .exists(b => b.isInstanceOf[HSpaceBox] && b.width > 1.0)

  // The width of everything before the first character on a line: the zero-width leftskip glue, plus any
  // indent or stray leading space. A flush line has only the zero-width glue here, so this is ~0.
  private def leadingWidth(line: HBox): Double =
    line.boxes.takeWhile(!_.isInstanceOf[CharBox]).map(_.width).sum

  private def run(body: TypesetterHandler => Unit): List[HBox] =
    val t = new HeadlessTypesetter
    t.set("vsize", 1.0e9)
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new ByteArrayOutputStream) {
      body(handler)
      t.paragraph()
    }
    t.mode.asInstanceOf[Builder].list.collect { case h: HBox => h }.toList

  private def process(src: String): List[HBox] =
    val t = new HeadlessTypesetter
    t.set("vsize", 1.0e9)
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new ByteArrayOutputStream) {
      proc.process(src)
      t.paragraph()
    }
    t.mode.asInstanceOf[Builder].list.collect { case h: HBox => h }.toList

  "closing an empty \\noindent paragraph emits no line and keeps the next paragraph flush" in {
    // \noindent opens an empty paragraph (as a heading macro does); a paragraph break closes it; the real
    // paragraph after it is the only line, and it opens flush left.
    val ls = run { handler =>
      val t = handler.typesetter
      t.noindent     // open an empty, un-indented paragraph
      t.paragraph()  // a blank line closes it
      handler.text("Body paragraph after the heading.")
    }
    ls.length shouldBe 1
    startsIndented(ls.head) shouldBe false
  }

  "the paragraph after the flush one indents again" in {
    // The flush state is consumed by the paragraph that uses it; the next paragraph indents normally.
    val ls = run { handler =>
      val t = handler.typesetter
      t.noindent
      t.paragraph()
      handler.text("First body paragraph.")
      t.paragraph()
      handler.text("Second body paragraph.")
    }
    ls.length shouldBe 2
    startsIndented(ls(0)) shouldBe false
    startsIndented(ls(1)) shouldBe true
  }

  "the \\noindent primitive skips the space after it, so the text starts flush" in {
    // A control word absorbs its trailing space in TeX. The \noindent primitive followed by a literal space
    // then text must begin the line with the text, not a leading interword space (a one-character indent).
    val ls = process("\\noindent The quick brown fox jumps over the lazy dog.")
    ls.length shouldBe 1
    leadingWidth(ls.head) should be < 1.0
  }

  "the \\indent primitive still indents, and skips only the following space" in {
    // \indent keeps its first-line indent; the space after the control word is absorbed, but the indent box is
    // the real lead, so the line is indented (not flush) and the text follows the indent, not a stray space.
    val ls = process("\\indent The quick brown fox jumps over the lazy dog.")
    ls.length shouldBe 1
    startsIndented(ls.head) shouldBe true
  }
