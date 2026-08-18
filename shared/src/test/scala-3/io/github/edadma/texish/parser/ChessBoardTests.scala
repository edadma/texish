package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, GlyphBox, HeadlessTypesetter, PictureBox, PictureOp, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.ArrayBuffer

/** The diagram the `chess` package draws: one \picture box holding sixty-four filled squares, a piece glyph at the
  * centre of each occupied one, a border and the coordinate labels. The headless glyph seam makes a placed glyph's
  * index its own codepoint, so a test can say which piece landed on which square — which is the only thing a
  * diagram really has to get right.
  *
  * Geometry, from the default square of 17pt: the coordinate margin is 0.85 squares (14.45) on the left and
  * bottom, the far edge 0.2 (3.4), so the board is 8*17 + 14.45 + 3.4 = 153.85 square, and the centre of a1 is at
  * (14.45 + 8.5, 14.45 + 8.5).
  */
class ChessBoardTests extends AnyFreeSpec with Matchers:

  private class Capture extends HeadlessTypesetter:
    val pictures = ArrayBuffer[PictureBox]()
    override infix def add(box: Box): Typesetter =
      box match
        case pb: PictureBox => pictures += pb
        case _              =>
      super.add(box)

  private def board(src: String): PictureBox =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\use{chess}\n" + src))
    t.pictures should not be empty
    t.pictures.head

  /** Placed glyphs as (codepoint, x, y) — the centre of the square each piece stands on. */
  private def glyphs(p: PictureBox): Vector[(Int, Double, Double)] =
    p.displayList.collect { case PictureOp.Place(g: GlyphBox, _, x, y) => (g.glyph, x, y) }

  /** Every subpath start, which for this drawing is one per filled square plus one for the border. */
  private def corners(p: PictureBox): Vector[(Double, Double)] =
    p.displayList.collect { case PictureOp.MoveTo(x, y) => (x, y) }

  private def at(p: PictureBox, x: Double, y: Double): Option[Int] =
    glyphs(p).find { case (_, gx, gy) => gx === x && gy === y }.map(_._1)

  private val WhiteKing  = 0x2654
  private val WhiteRook  = 0x2656
  private val WhitePawn  = 0x2659
  private val BlackKing  = 0x265a
  private val BlackQueen = 0x265b

  "the empty board" - {
    "is eight squares of the configured size, plus the coordinate margin" in {
      val p = board("\\fenboard{8/8/8/8/8/8/8/8}\\showboard")
      p.width shouldBe 153.85 +- 0.001
      p.height shouldBe 153.85 +- 0.001
    }
    "is sixty-four filled squares and one border" in {
      corners(board("\\fenboard{8/8/8/8/8/8/8/8}\\showboard")).size shouldBe 65
    }
    "holds no pieces" in {
      glyphs(board("\\fenboard{8/8/8/8/8/8/8/8}\\showboard")) shouldBe empty
    }
    "shrinks by the coordinate margin when the coordinates are turned off" in {
      val p = board("\\set chesscoords {0}\\fenboard{8/8/8/8/8/8/8/8}\\showboard")
      p.width shouldBe (8 * 17 + 2 * 3.4) +- 0.001
    }
  }

  "the initial array" - {
    "is thirty-two pieces" in {
      glyphs(board("\\newgame\\showboard")).size shouldBe 32
    }
    "puts the white rooks in the near corners and the black king on e8" in {
      val p = board("\\newgame\\showboard")
      at(p, 22.95, 22.95) shouldBe Some(WhiteRook)   // a1
      at(p, 141.95, 22.95) shouldBe Some(WhiteRook)  // h1
      at(p, 90.95, 141.95) shouldBe Some(BlackKing)  // e8
      at(p, 22.95, 39.95) shouldBe Some(WhitePawn)   // a2
    }
  }

  "\\chessflip draws the same position from Black's side" in {
    val p = board("\\newgame\\set chessflip {1}\\showboard")
    // a1 is now the far right corner and h1 the near left; the black king lands where White's stood.
    at(p, 141.95, 141.95) shouldBe Some(WhiteRook)
    at(p, 22.95, 141.95) shouldBe Some(WhiteRook)
    at(p, 73.95, 22.95) shouldBe Some(BlackKing)
  }

  "a diagram follows the moves that were played" in {
    val p = board("\\newgame\\hidemoves{1.e4 e5 2.Qh5}\\showboard")
    at(p, 90.95, 22.95) shouldBe Some(WhiteKing)     // e1, the king has not moved
    at(p, 90.95, 39.95) shouldBe None                // e2, the pawn has left
    at(p, 90.95, 90.95) shouldBe Some(0x265f)        // e5 is Black's pawn, where it answered
    at(p, 141.95, 90.95) shouldBe Some(0x2655)       // h5, where the queen went
  }

  "\\chesshighlight tints the squares it names, one filled rectangle each" in {
    val plain     = corners(board("\\fenboard{8/8/8/8/8/8/8/8}\\showboard"))
    val tinted    = corners(board("\\set chesshighlight {e4 d5}\\fenboard{8/8/8/8/8/8/8/8}\\showboard"))
    tinted.size shouldBe plain.size + 2
  }

  "a promoted piece is drawn as what it promoted to" in {
    val p = board("\\fenboard{8/P6k/8/8/8/8/6K1/8 w}\\hidemoves{1.a8=Q}\\showboard")
    at(p, 22.95, 141.95) shouldBe Some(0x2655)
    glyphs(p).map(_._1) should not contain WhitePawn
  }

  "the pieces are the twelve Miscellaneous Symbols codepoints, White's and Black's apart" in {
    val p = board("\\fenboard{4k3/8/8/8/8/8/8/3QK3 w}\\showboard")
    glyphs(p).map(_._1).toSet shouldBe Set(BlackKing, WhiteKing, 0x2655)
    at(p, 90.95, 141.95) shouldBe Some(BlackKing)
    at(p, 73.95, 22.95) shouldBe Some(0x2655)
  }

  "an arbitrary position is drawn without any moves being played" in {
    val p = board("\\fendiagram{4k3/8/8/8/8/8/8/3qK3 b}")
    at(p, 73.95, 22.95) shouldBe Some(BlackQueen)
  }
