package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, Color, GlyphBox, PictureBox, PictureOp, HeadlessTypesetter, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** A mark a drawing command colours itself — an arrowhead, a placed glyph — takes the picture's `\stroke` or
  * `\fill` when the picture names one, and otherwise the pen in force where the picture sits. That fallback is
  * the point of these tests: it used to be a literal black, so an uncoloured arrow or glyph in a document set in
  * another ink (a coloured scheme, a previewer inverting the page for a dark background) came out the one colour
  * that could not be read. Shapes painted through the picture's own fill/stroke state are unaffected — with
  * neither set they still draw nothing, exactly as before.
  *
  * `\color` is the declaration form (it sets the pen for the rest of the enclosing group); `\textcolor` opens a
  * group of its own. Both are exercised, since a picture inside a local coloured span should follow that span.
  */
class PictureInkTests extends AnyFreeSpec with Matchers:

  private class Capture extends HeadlessTypesetter:
    val pictures = ArrayBuffer[PictureBox]()
    override infix def add(box: Box): Typesetter =
      box match
        case pb: PictureBox => pictures += pb
        case _              =>
      super.add(box)

  private def run(src: String): Vector[PictureOp] =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.process(src)
    t.pictures should have size 1
    t.pictures.head.displayList

  /** Every colour a Paint op in this display list draws with, fills and strokes alike. */
  private def inks(ops: Vector[PictureOp]): Set[Color] =
    ops.collect { case PictureOp.Paint(f, s) => f.toSet ++ s.toSet }.flatten.toSet

  /** The colour of the single glyph placed by this display list. */
  private def glyphInk(ops: Vector[PictureOp]): Color =
    ops.collect { case PictureOp.Place(g: GlyphBox, _, _, _) => g.color } match
      case Vector(c) => c
      case other     => fail(s"expected exactly one placed glyph, got ${other.length}")

  "an uncoloured arrow takes the document's ink" in {
    val ops = run("\\color{teal}\\picture width:1in height:1in { \\arrow{0 0 20 0} }")
    inks(ops) shouldBe Set(Color("teal"))
  }

  "an explicit stroke still wins over the document's ink" in {
    val ops = run("\\color{teal}\\picture width:1in height:1in { \\stroke{crimson} \\arrow{0 0 20 0} }")
    inks(ops) shouldBe Set(Color("crimson"))
  }

  // An arrow is a stroke, so \stroke is consulted first — but a picture that named only a \fill has still named
  // a colour, and the arrow should use it rather than fall through to the pen.
  "a picture that set only a fill lends that colour to an arrow" in {
    val ops = run("\\color{teal}\\picture width:1in height:1in { \\fill{navy} \\arrow{0 0 20 0} }")
    inks(ops) shouldBe Set(Color("navy"))
  }

  "an uncoloured arrowhead takes the document's ink" in {
    val ops = run("\\color{teal}\\picture width:1in height:1in { \\arrowhead head:dot size:8 {0 0 10 0} }")
    inks(ops) shouldBe Set(Color("teal"))
  }

  "an uncoloured glyph takes the document's ink" in {
    val ops = run("\\color{teal}\\picture width:1in height:1in { \\glyph{10 10}{65} }")
    glyphInk(ops) shouldBe Color("teal")
  }

  "an explicit fill still wins over the document's ink for a glyph" in {
    val ops = run("\\color{teal}\\picture width:1in height:1in { \\fill{crimson} \\glyph{10 10}{65} }")
    glyphInk(ops) shouldBe Color("crimson")
  }

  "an uncoloured fontglyph takes the document's ink" in {
    val ops = run("\\color{teal}\\picture width:1in height:1in { \\fontglyph{10 10}{bravura}{32}{57424} }")
    glyphInk(ops) shouldBe Color("teal")
  }

  // The picture is inline material, so a local coloured span is the pen where it sits — unlike a footnote, which
  // is set apart at the foot of the page and resets to the document's ink.
  "a picture inside a coloured span follows that span" in {
    val ops = run("\\textcolor{crimson}{\\picture width:1in height:1in { \\arrow{0 0 20 0} }}")
    inks(ops) shouldBe Set(Color("crimson"))
  }

  "with no colour named anywhere the ink is still black" in {
    val ops = run("\\picture width:1in height:1in { \\arrow{0 0 20 0} }")
    inks(ops) shouldBe Set(Color("black"))
  }

  // Shapes go through the picture's own fill/stroke state, which the pen does not seed: a picture that names no
  // colour draws no shapes, as it always has. Only the self-coloured marks fall back to the pen.
  "an ordinary shape is still unpainted when the picture names no colour" in {
    val ops = run("\\color{teal}\\picture width:1in height:1in { \\circle{10 10 5} }")
    ops should contain(PictureOp.Paint(None, None))
    inks(ops) shouldBe empty
  }
