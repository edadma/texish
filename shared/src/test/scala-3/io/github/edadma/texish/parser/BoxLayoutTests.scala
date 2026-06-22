package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, CharBox, HBox, HeadlessTypesetter, HSpaceBox, RaiseBox, Typesetter, VerticalBox}
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
