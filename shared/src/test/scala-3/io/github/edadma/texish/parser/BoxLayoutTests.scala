package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, CharBox, HBox, HeadlessTypesetter, HSpaceBox, RaiseBox, Typesetter, VerticalBox, VSpaceBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The LaTeX box-layout commands: \mbox / \makebox (horizontal boxes of an explicit width and alignment), \parbox
  * and the \minipage machinery (\beginminipage / \endminipage — a paragraph set in a box of a given width), and the
  * \newlength / \setlength / \addtolength length registers. The HeadlessTypesetter gives every character width 6,
  * so "AB" is 12 wide; widths below are in points (1pt == 1 unit here).
  */
class BoxLayoutTests extends AnyFreeSpec with Matchers:

  private val tol = 1e-9

  /** Records every box handed to `add`, so the box an end-to-end command finally places is `added.last`. */
  private class Capture extends HeadlessTypesetter:
    val added = ArrayBuffer[Box]()
    override infix def add(box: Box): Typesetter =
      added += box
      super.add(box)

  private def fixture(): (Capture, Processor) =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    t.document = new io.github.edadma.texish.DocumentMode(t)
    (t, proc)

  "\\mbox" - {
    "boxes its content at natural width" in {
      val (t, proc) = fixture()
      proc.process("\\mbox{AB}")
      val box = t.added.last
      box shouldBe a[HBox]
      box.width shouldBe (12.0 +- tol)
    }
  }

  "\\makebox" - {
    "sets the box to the requested width" in {
      val (t, proc) = fixture()
      proc.process("\\makebox[60pt]{AB}")
      t.added.last.width shouldBe (60.0 +- tol)
    }

    "centres by padding with equal fil glue on both sides" in {
      val (t, proc) = fixture()
      proc.process("\\makebox[60pt]{AB}")
      val box = t.added.last.asInstanceOf[HBox]
      // the 48pt of slack (60 - 12) splits evenly, the fil glue set to 24pt a side
      box.boxes.head shouldBe a[HSpaceBox]
      box.boxes.last shouldBe a[HSpaceBox]
      box.boxes.head.width shouldBe (24.0 +- tol)
      box.boxes.last.width shouldBe (24.0 +- tol)
    }

    "flush-left puts the glue only after the content" in {
      val (t, proc) = fixture()
      proc.process("\\makebox[60pt][l]{AB}")
      val box = t.added.last.asInstanceOf[HBox]
      box.boxes.head shouldBe a[CharBox]
      box.boxes.last shouldBe a[HSpaceBox]
      box.boxes.last.width shouldBe (48.0 +- tol)
    }

    "flush-right puts the glue only before the content" in {
      val (t, proc) = fixture()
      proc.process("\\makebox[60pt][r]{AB}")
      val box = t.added.last.asInstanceOf[HBox]
      box.boxes.head shouldBe a[HSpaceBox]
      box.boxes.last shouldBe a[CharBox]
      box.boxes.head.width shouldBe (48.0 +- tol)
    }

    "with no width is a natural-width box, like \\mbox" in {
      val (t, proc) = fixture()
      proc.process("\\makebox{AB}")
      t.added.last.width shouldBe (12.0 +- tol)
    }
  }

  "\\parbox" - {
    "wraps a centred box in a RaiseBox and restores hsize" in {
      val (t, proc) = fixture()
      t.set("hsize", 200.0)
      proc.process("\\parbox{60pt}{AB}")
      t.added.last shouldBe a[RaiseBox]
      t.getNumber("hsize") shouldBe (200.0 +- tol)
    }

    "[t] aligns on the first line — a plain vertical box, not raised" in {
      val (t, proc) = fixture()
      t.set("hsize", 200.0)
      proc.process("\\parbox[t]{60pt}{AB}")
      t.added.last shouldBe a[VerticalBox]
    }

    "resolves a \\linewidth fraction against hsize" in {
      val (t, proc) = fixture()
      t.set("hsize", 200.0)
      proc.process("\\parbox[t]{0.5\\linewidth}{AB}")
      // the body set at half the line width; hsize is restored afterwards
      t.getNumber("hsize") shouldBe (200.0 +- tol)
      t.added.last shouldBe a[VerticalBox]
    }

    "[height] fixes the box height, whatever the amount of text" in {
      val (t, proc) = fixture()
      t.set("hsize", 200.0)
      proc.process("\\parbox[t][100pt][t]{60pt}{AB}")
      (t.added.last.ascent + t.added.last.descent) shouldBe (100.0 +- tol)

      val (t2, proc2) = fixture()
      t2.set("hsize", 200.0)
      proc2.process("\\parbox[t][100pt][t]{60pt}{A B C D E F G H}") // wraps to several lines, still 100 tall
      (t2.added.last.ascent + t2.added.last.descent) shouldBe (100.0 +- tol)
    }

    "a trailing newline in the body does not leak a spurious break into the following text" in {
      // A \parbox (here full-width, in a \colorbox) whose body ends with a source newline must clear its
      // pending-newline state, or the newline after the box becomes a paragraph break and an empty full-width
      // line — which the line-setter reported as an underfull box (the lower-third recipe hit this).
      val out = new java.io.ByteArrayOutputStream
      Console.withOut(out) {
        val (t, proc) = fixture()
        proc.process(
          "\\set paperwidth {500}\n\\set paperheight {300}\n\\geometry margin:0\n\\set fboxsep {0}\n" +
            "\\vfill\n\\noindent\\colorbox{black}{\\parbox[c][100pt][c]{\\linewidth}{\nHi\n}}\n",
        )
        t.end()
      }
      out.toString should not include "underfull"
    }

    "[inner-pos] holds the content top, bottom, or centred within the fixed height" in {
      // fill above the content vs below it: top-aligned fills below, bottom-aligned fills above, centred fills both
      def fills(src: String): (Double, Double) =
        val (t, proc) = fixture()
        t.set("hsize", 200.0)
        proc.process(src)
        val boxes     = t.added.last.asInstanceOf[VerticalBox].boxes.toList
        val firstLine = boxes.indexWhere(_.isInstanceOf[HBox])
        val lastLine  = boxes.lastIndexWhere(_.isInstanceOf[HBox])
        def space(bs: List[Box]) = bs.collect { case s: VSpaceBox => s.height }.sum
        (space(boxes.take(firstLine)), space(boxes.drop(lastLine + 1)))

      val (topAbove, topBelow) = fills("\\parbox[t][100pt][t]{60pt}{AB}")
      topBelow should be > topAbove // content held at the top

      val (botAbove, botBelow) = fills("\\parbox[t][100pt][b]{60pt}{AB}")
      botAbove should be > botBelow // content held at the bottom

      val (cAbove, cBelow) = fills("\\parbox[t][100pt][c]{60pt}{AB}")
      cAbove shouldBe (cBelow +- 1.0) // centred: equal fill above and below
    }
  }

  "\\beginminipage / \\endminipage" - {
    "box a body at a width and restore hsize" in {
      val (t, proc) = fixture()
      t.set("hsize", 300.0)
      proc.process("\\beginminipage{80pt}AB\\endminipage")
      t.added.last shouldBe a[RaiseBox]
      t.getNumber("hsize") shouldBe (300.0 +- tol)
    }

    "[b] aligns on the last line — a plain vertical box" in {
      val (t, proc) = fixture()
      t.set("hsize", 300.0)
      proc.process("\\beginminipage[b]{80pt}AB\\endminipage")
      t.added.last shouldBe a[VerticalBox]
    }

    "[height] fixes the minipage height like \\parbox" in {
      val (t, proc) = fixture()
      t.set("hsize", 300.0)
      proc.process("\\beginminipage[t][120pt][t]{80pt}AB\\endminipage")
      (t.added.last.ascent + t.added.last.descent) shouldBe (120.0 +- tol)
    }
  }

  "lengths" - {
    "\\newlength initialises a length to zero" in {
      val (t, proc) = fixture()
      proc.process("\\newlength{gap}")
      t.getNumber("gap") shouldBe (0.0 +- tol)
    }

    "\\setlength assigns a dimension" in {
      val (t, proc) = fixture()
      proc.process("\\setlength{gap}{12pt}")
      t.getNumber("gap") shouldBe (12.0 +- tol)
    }

    "\\addtolength adds to a length" in {
      val (t, proc) = fixture()
      proc.process("\\setlength{gap}{12pt}\\addtolength{gap}{3pt}")
      t.getNumber("gap") shouldBe (15.0 +- tol)
    }

    "a length reads back into \\calc" in {
      val (t, proc) = fixture()
      proc.process("\\setlength{gap}{10pt}\\set doubled {\\calc{gap * 2}}")
      t.getNumber("doubled") shouldBe (20.0 +- tol)
    }
  }
