package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, CharBox, HBox, HeadlessTypesetter, OverlayBox, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream

// A cutout reserves a rectangle of the galley that running text flows around. These tests drive the low-level
// \cutout primitive (pure geometry, no content) and check that exactly the lines whose vertical band overlaps the
// rectangle are narrowed, on the correct side, and that the narrowing carries into following paragraphs.
class CutoutTests extends AnyFreeSpec with Matchers:

  // The width of everything before the first character on a line: the left margin glue plus any indent. With
  // \noindent there is no indent, so for a left cutout of inset w this is ~w, and for a flush line it is ~0.
  private def leadingWidth(line: HBox): Double =
    line.boxes.takeWhile(!_.isInstanceOf[CharBox]).map(_.width).sum

  private def lines(src: String): List[HBox] =
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

  // a paragraph long enough to break into many lines at the default 6.5in measure
  private val body =
    "\\noindent " + ("The quick brown fox jumps over the lazy dog. " * 12)

  "a left cutout taller than the paragraph indents every line by its inset" in {
    val ls = lines("\\cutout{72}{100000}{l}" + body)
    ls.length should be > 3
    all(ls.map(leadingWidth)) should (be >= 71.0 and be <= 73.0)
  }

  "a left cutout shorter than the paragraph indents only the lines it overlaps" in {
    // a short cutout narrows the opening lines and then releases the rest to the full measure
    val ls = lines("\\cutout{72}{30}{l}" + body)
    leadingWidth(ls.head) should (be >= 71.0 and be <= 73.0)
    leadingWidth(ls.last) should be < 1.0
  }

  "a right cutout keeps lines flush left but narrows the measure, forcing more lines" in {
    val plain   = lines(body)
    val wrapped = lines("\\cutout{72}{100000}{r}" + body)
    all(wrapped.map(leadingWidth)) should be < 1.0     // still flush left
    wrapped.length should be > plain.length            // but narrower, so more lines
  }

  "a zero-height cutout overlaps no line and changes nothing" in {
    val plain  = lines(body)
    val withIt = lines("\\cutout{72}{0}{l}" + body)
    withIt.length shouldBe plain.length
    leadingWidth(withIt.head) should be < 1.0
  }

  "a tall cutout wraps the paragraphs that follow it, not just the first" in {
    // two paragraphs under one tall cutout: the second paragraph's lines are still indented
    val ls = lines("\\cutout{72}{100000}{l}" + body + "\n\n" + body)
    all(ls.map(leadingWidth)) should (be >= 71.0 and be <= 73.0)
  }

  "an OverlayBox reports no metrics so it neither advances nor inflates layout" in {
    val dummy = new Box:
      val ascent   = 3.0
      val descent  = 2.0
      val width    = 4.0
      val isSpace  = false
      val xAdvance = 4.0
      def draw(t: Typesetter, x: Double, y: Double): Unit = ()
    val ov = new OverlayBox(dummy, 5.0, 9.0)
    ov.ascent shouldBe 0.0
    ov.descent shouldBe 0.0
    ov.width shouldBe 0.0
    ov.xAdvance shouldBe 0.0
  }
