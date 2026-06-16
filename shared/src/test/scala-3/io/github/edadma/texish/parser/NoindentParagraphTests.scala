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
