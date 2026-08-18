package io.github.edadma.texish.parser

import io.github.edadma.texish.HeadlessTypesetter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The `chess` package (packages/chess.texish) as a move engine. Algebraic notation names where a piece arrives
  * and leaves where it came from to be worked out, so almost everything here is about that inference: which piece
  * could have reached the square, which of two the notation's own file or rank letter picks out, and which one is
  * left when neither is given because the other may not legally move.
  *
  * The position is checked through \chessfen, which writes it back as a FEN placement field and side to move —
  * the inverse of the \fenboard that sets it. That makes each test one string, and makes a wrong piece on a wrong
  * square say exactly which.
  */
class ChessTests extends AnyFreeSpec with Matchers:

  private def handler(src: String): TypesetterHandler =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\use{chess}\n" + src))
    handler

  /** The position a source reaches, as FEN. */
  private def fen(src: String): String =
    Value.display(handler(src + "\n\\ckfen").get("chessfenvalue"))

  /** What a source sets, with the piece figurines written back as their letters so a test can read them. */
  private def typeset(src: String): String =
    val t       = new HeadlessTypesetter
    val h       = new TypesetterHandler(t)
    val proc    = new Processor(h)
    registerTypesettingPrimitives(proc, h)
    val out = h.capture(Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\use{chess}\n" + src)))
    out

  private val Start = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w"

  "the initial array" - {
    "\\newgame is the position every game starts from" in {
      fen("\\newgame") shouldBe Start
    }
    "\\fenboard round-trips a position through \\chessfen" in {
      val p = "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/2N2N2/PPPP1PPP/R1BQK2R b"
      fen(s"\\fenboard{$p}") shouldBe p
    }
    "a FEN with no side to move is White's" in {
      fen("\\fenboard{8/8/8/8/8/8/8/K6k}") shouldBe "8/8/8/8/8/8/8/K6k w"
    }
  }

  "a pawn move names only where the pawn arrives" - {
    "one step" in {
      fen("\\newgame\\hidemoves{1.e3}") shouldBe "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b"
    }
    "two steps, which is legal only from the home rank" in {
      fen("\\newgame\\hidemoves{1.e4}") shouldBe "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b"
    }
    "Black's pawns run the other way" in {
      fen("\\newgame\\hidemoves{1.e4 e5}") shouldBe "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w"
    }
    "a capture is disambiguated by the file the pawn came from" in {
      fen("\\newgame\\hidemoves{1.e4 d5 2.exd5}") shouldBe "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b"
    }
  }

  "the sliding pieces reach the first piece along each direction" - {
    "a bishop out of the opening" in {
      fen("\\newgame\\hidemoves{1.e4 e5 2.Bc4}") shouldBe "rnbqkbnr/pppp1ppp/8/4p3/2B1P3/8/PPPP1PPP/RNBQK1NR b"
    }
    "a queen along a diagonal" in {
      fen("\\newgame\\hidemoves{1.e4 e5 2.Qh5}") shouldBe "rnbqkbnr/pppp1ppp/8/4p2Q/4P3/8/PPPP1PPP/RNB1KBNR b"
    }
    "a rook stops at the piece in the way, so only the reachable one is a candidate" in {
      // Both rooks are on the first rank and both would reach d1 along it, but the bishop on c1 stops the one
      // on a1 — so only the rook on e1 is a candidate and the move needs no disambiguating letter.
      fen("\\fenboard{7k/8/8/8/8/8/8/R1B1R2K w}\\hidemoves{1.Rd1}") shouldBe "7k/8/8/8/8/8/8/R1BR3K b"
    }
  }

  "when two pieces could arrive, the notation says which" - {
    "a file letter" in {
      fen("\\fenboard{7k/8/8/8/8/8/8/R3K2R w}\\hidemoves{1.Rae1}") shouldBe "7k/8/8/8/8/8/8/4R2R b"
    }
    "a rank digit" in {
      fen("\\fenboard{7k/8/8/8/8/R7/8/R5K1 w}\\hidemoves{1.R1a2}") shouldBe "7k/8/8/8/8/R7/R7/6K1 b"
    }
    "and when it says nothing, the pinned piece is the one that cannot have moved" in {
      // Knights on b4 and f4 both reach d3, and neither is named — so the move is the b4 knight's, because the
      // f4 knight is pinned to its king by the rook on f1.
      fen("\\fenboard{5k2/8/8/8/1n3n2/8/8/K4R2 b}\\hidemoves{1...Nd3}") shouldBe "5k2/8/8/8/5n2/3n4/8/K4R2 w"
    }
  }

  "the moves that change more than the two squares they name" - {
    "castling on the king's side moves the rook to f1" in {
      fen("\\fenboard{r3k2r/8/8/8/8/8/8/R3K2R w}\\hidemoves{1.O-O O-O}") shouldBe "r4rk1/8/8/8/8/8/8/R4RK1 w"
    }
    "castling on the queen's side puts the king on c1 and the rook on d1" in {
      fen("\\fenboard{r3k2r/8/8/8/8/8/8/R3K2R w}\\hidemoves{1.O-O-O O-O-O}") shouldBe "2kr3r/8/8/8/8/8/8/2KR3R w"
    }
    "a promotion arrives as the piece after the =" in {
      fen("\\fenboard{8/P6k/8/8/8/8/6K1/8 w}\\hidemoves{1.a8=Q+}") shouldBe "Q7/7k/8/8/8/8/6K1/8 b"
      fen("\\fenboard{8/P6k/8/8/8/8/6K1/8 w}\\hidemoves{1.a8=N}") shouldBe "N7/7k/8/8/8/8/6K1/8 b"
    }
    "Black promotes to a black piece" in {
      fen("\\fenboard{8/6K1/8/8/8/8/p6k/8 b}\\hidemoves{1...a1=Q}") shouldBe "8/6K1/8/8/8/8/7k/q7 w"
    }
    "en passant takes a pawn that is on neither named square" in {
      // 3.exd6 arrives on an empty d6; the pawn it takes is the one that has just run past, on d5.
      fen("\\newgame\\hidemoves{1.e4 Nf6 2.e5 d5 3.exd6}") shouldBe
        "rnbqkb1r/ppp1pppp/3P1n2/8/8/8/PPPP1PPP/RNBQKBNR b"
    }
  }

  "a whole game plays to the position it is known to reach" - {
    "the Ruy Lopez, nine moves in" in {
      fen("\\newgame\\hidemoves{1.e4 e5 2.Nf3 Nc6 3.Bb5 a6 4.Ba4 Nf6 5.O-O Be7 6.Re1 b5 7.Bb3 d6 8.c3 O-O 9.h3}") shouldBe
        "r1bq1rk1/2p1bppp/p1np1n2/1p2p3/4P3/1BP2N1P/PP1P1PP1/RNBQR1K1 b"
    }
    "Anderssen–Kieseritzky 1851, the Immortal Game, to the mate" in {
      fen(
        "\\newgame\\hidemoves{1.e4 e5 2.f4 exf4 3.Bc4 Qh4+ 4.Kf1 b5 5.Bxb5 Nf6 6.Nf3 Qh6 7.d3 Nh5 8.Nh4 Qg5 " +
          "9.Nf5 c6 10.g4 Nf6 11.Rg1 cxb5 12.h4 Qg6 13.h5 Qg5 14.Qf3 Ng8 15.Bxf4 Qf6 16.Nc3 Bc5 17.Nd5 Qxb2 " +
          "18.Bd6 Bxg1 19.e5 Qxa1+ 20.Ke2 Na6 21.Nxg7+ Kd8 22.Qf6+ Nxf6 23.Be7# 1-0}",
      ) shouldBe "r1bk3r/p2pBpNp/n4n2/1p1NP2P/6P1/3P4/P1P1K3/q5b1 b"
    }
  }

  "the three ways to give a line of moves" - {
    "\\hidemoves plays without printing anything" in {
      typeset("\\newgame\\hidemoves{1.e4 e5}").trim shouldBe ""
    }
    "\\showmoves prints but leaves the position where it was" in {
      fen("\\newgame\\hidemoves{1.e4}\\showmoves{1...e5 2.Nf3}") shouldBe
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b"
    }
    "\\mainline both prints and plays" in {
      fen("\\newgame\\mainline{1.e4 e5}") shouldBe "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w"
    }
  }

  "a printed line numbers itself from the position, not from the input" - {
    "the numbers a document writes are ignored" in {
      typeset("\\newgame\\set chessfigurines {0}\\mainline{7.e4 e5 12.Nf3}").trim shouldBe "1.e4 e5 2.Nf3"
    }
    "a line that opens on Black's move says so" in {
      typeset("\\newgame\\set chessfigurines {0}\\hidemoves{1.e4 e5 2.Nf3}\\mainline{2...Nc6 3.Bb5}").trim shouldBe
        "2...Nc6 3.Bb5"
    }
    "a word that is only a move number is skipped" in {
      typeset("\\newgame\\set chessfigurines {0}\\mainline{1. e4 e5}").trim shouldBe "1.e4 e5"
    }
    "a result ends the line and is set as written" in {
      typeset("\\newgame\\set chessfigurines {0}\\mainline{1.e4 e5 1/2-1/2}").trim shouldBe "1.e4 e5 1/2-1/2"
    }
  }

  "a move the position cannot account for is skipped, and the rest of the line still plays" in {
    // No knight reaches e4 from the initial array, so 1.Ne4 is a transcription slip. It is skipped, but the side
    // to move still passes to Black — otherwise every move after it would be read for the wrong side.
    fen("\\newgame\\hidemoves{1.Ne4 e5}") shouldBe "rnbqkbnr/pppp1ppp/8/4p3/8/8/PPPPPPPP/RNBQKBNR w"
  }
