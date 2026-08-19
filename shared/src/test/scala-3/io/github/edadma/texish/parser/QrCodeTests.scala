package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, HeadlessTypesetter, PictureBox, PictureOp, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.ArrayBuffer

/** The `qrcode` package (packages/qrcode.texish): an ISO/IEC 18004 encoder written in the document language, and
  * the picture it draws from the result. There is no QR primitive in the engine — the segment packing, the
  * Reed-Solomon check codewords over GF(256), the zig-zag placement and the eight-way mask search are all \calc,
  * \for and \put over sequences.
  *
  * **The expected symbols here are frozen fixtures, and that is deliberate.** They were produced by this package
  * and checked module-for-module against a separate, independently written encoder over every case below —
  * alphanumeric, numeric and byte mode, all four error-correction levels, multi-block symbols, the alignment
  * patterns of the middle versions and the version-information block of version 7 and up. Frozen, they keep
  * testing the encoder against something other than itself; recomputed from a live oracle, they would only ever
  * agree with whatever the package currently does.
  *
  * A symbol that is wrong by one module does not scan, and nothing about it looks wrong, so the small cases are
  * written out in full, where a diff says which module moved. The larger ones are pinned by their digest, since
  * sixty rows of sixty characters would document nothing the first two do not.
  */
class QrCodeTests extends AnyFreeSpec with Matchers:

  private class Capture extends HeadlessTypesetter:
    val pictures = ArrayBuffer[PictureBox]()
    override infix def add(box: Box): Typesetter =
      box match
        case pb: PictureBox => pictures += pb
        case _              =>
      super.add(box)

  private def run(src: String): (Capture, TypesetterHandler) =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\use{qrcode}\n" + src))
    (t, handler)

  /** The symbol a source computes, one row of `.` and `#` per line of modules. */
  private def matrix(src: String): Vector[String] =
    run(src)._2.get("qrmatrixvalue") match
      case Value.Seq(items) => items.map(Value.display)
      case other            => fail(s"no matrix: $other")

  private def digest(rows: Vector[String]): String = SymbolFingerprint.of(rows)

  private def picture(src: String): PictureBox =
    val (t, _) = run(src)
    t.pictures should not be empty
    t.pictures.head

  /** One subpath start per drawn rectangle: the background, then one for each dark module. */
  private def rects(p: PictureBox): Vector[(Double, Double)] =
    p.displayList.collect { case PictureOp.MoveTo(x, y) => (x, y) }

  private val Finder =
    Vector("#######", "#.....#", "#.###.#", "#.###.#", "#.###.#", "#.....#", "#######")

  // ---- The shape of a symbol -------------------------------------------------------

  "a symbol has the shape the standard prescribes" - {
    "a finder pattern sits in three of the four corners" in {
      val m = matrix("\\qrmatrix{HELLO}")
      m.take(7).map(_.take(7)) shouldBe Finder
      m.take(7).map(_.takeRight(7)) shouldBe Finder
      m.takeRight(7).map(_.take(7)) shouldBe Finder
    }

    "the timing patterns alternate along row and column six" in {
      val m = matrix("\\qrmatrix{HELLO}")
      for i <- 8 until m.length - 8 do
        withClue(s"row 6, column $i: ") { m(6)(i) shouldBe (if i % 2 == 0 then '#' else '.') }
        withClue(s"column 6, row $i: ") { m(i)(6) shouldBe (if i % 2 == 0 then '#' else '.') }
    }

    "a version-1 symbol is 21 modules square, and each further version adds four" in {
      matrix("\\qrmatrix{HELLO}").length shouldBe 21
      matrix("\\qrmatrix{https://example.com}").length shouldBe 25
      matrix("\\qrmatrix{The quick brown fox jumps over the lazy dog}").length shouldBe 33
      matrix("\\qrmatrix{" + "A" * 120 + "}").length shouldBe 45
    }

    "every row is as long as the symbol is tall" in {
      val m = matrix("\\qrmatrix{https://example.com}")
      all(m.map(_.length)) shouldBe m.length
    }
  }

  // ---- The encodings ---------------------------------------------------------------

  "alphanumeric mode, version 1 at the quartile level" in {
    matrix("\\qrmatrix{HELLO}") shouldBe Vector(
      "#######..#..#.#######",
      "#.....#...#...#.....#",
      "#.###.#.....#.#.###.#",
      "#.###.#..##.#.#.###.#",
      "#.###.#.##.##.#.###.#",
      "#.....#..###..#.....#",
      "#######.#.#.#.#######",
      "........##..#........",
      "..##..###.#####.#....",
      ".##.##..##.###.#.####",
      "..###.#...###..##.#..",
      "###.##...#..#.#...##.",
      "#..########.##..#.#..",
      "........###....#..##.",
      "#######.#...#...#..##",
      "#.....#..#.###...#..#",
      "#.###.#..#..###..#..#",
      "#.###.#.#...###.#..#.",
      "#.###.#.#.#.##..#.#..",
      "#.....#..##..#..##...",
      "#######......#.#...#.",
    )
  }

  "numeric mode packs three digits into ten bits, so the same length of digits gives a different symbol" in {
    matrix("\\qrmatrix{12345678}") shouldBe Vector(
      "#######.####..#######",
      "#.....#.#..##.#.....#",
      "#.###.#.###.#.#.###.#",
      "#.###.#..####.#.###.#",
      "#.###.#..####.#.###.#",
      "#.....#.#####.#.....#",
      "#######.#.#.#.#######",
      "........#####........",
      "..###.#.#.##.###..###",
      "###....####.#..#.####",
      "####.##.###.##..##..#",
      "#..##...#..##.###.###",
      ".#....##.####.#.#..#.",
      "........#..#.##...#..",
      "#######....#...#.....",
      "#.....#...#.###..##.#",
      "#.###.#.##.#####.#...",
      "#.###.#.##..#.##..#..",
      "#.###.#.#.#.....#.#..",
      "#.....#...#.#..##.##.",
      "#######..#####..#.#..",
    )
  }

  "the encoder agrees with an independent one over every mode, level and version tested" - {
    "byte mode, reached by a character outside the alphanumeric set" in {
      digest(matrix("\\qrmatrix{hello world}")) shouldBe "7b5b5a41f0527139"
    }
    "a URL, whose // has to reach the encoder rather than starting a comment" in {
      digest(matrix("\\qrmatrix{https://example.com}")) shouldBe "99c1c176eca51cfd"
    }
    "a longer URL, at the version where the alignment pattern first appears" in {
      digest(matrix("\\qrmatrix{https://christianevangelism.media}")) shouldBe "6bc4167ddf1f994e"
    }
    "a multi-block symbol, whose data and check codewords interleave" in {
      digest(matrix("\\qrmatrix{The quick brown fox jumps over the lazy dog}")) shouldBe "f42179febf9c6238"
    }
    "text encoded as UTF-8, so an accented letter costs two bytes" in {
      digest(matrix("\\qrmatrix{Caf\u00e9 na\u00efve r\u00e9sum\u00e9}")) shouldBe "429a7fc4a09ef5ca"
    }
    "a hundred and twenty digits, which no Double could have carried" in {
      digest(matrix("\\qrmatrix{" + "0123456789" * 12 + "}")) shouldBe "911523f653923859"
    }
    "version 7 and up, which carries a version-information block as well" in {
      digest(matrix("\\qrmatrix{" + "A" * 120 + "}")) shouldBe "15801edb479be3b3"
    }
  }

  // ---- Options ---------------------------------------------------------------------

  "the option bracket" - {
    "a stronger level spends more of the symbol on protection, so the same text needs a bigger one" in {
      matrix("\\qrmatrix[ecc:l]{" + "A" * 60 + "}").length should be < matrix("\\qrmatrix[ecc:h]{" + "A" * 60 + "}").length
    }

    "the level is raised to the strongest the chosen version still holds" in {
      // five characters fit at every level in a version-1 symbol, so asking for L gives H anyway — free
      // protection at the same size, which is what the standard's own encoders do
      matrix("\\qrmatrix[ecc:l]{HELLO}") shouldBe matrix("\\qrmatrix[ecc:h]{HELLO}")
    }

    "an unknown level is reported, and the default used" in {
      matrix("\\qrmatrix[ecc:z]{HELLO}") shouldBe matrix("\\qrmatrix{HELLO}")
    }

    "options may be left out, and given in any order" in {
      matrix("\\qrmatrix[cell:4pt ecc:h]{HELLO}") shouldBe matrix("\\qrmatrix[ecc:h cell:4pt]{HELLO}")
    }
  }

  "a payload too long for any version draws nothing, rather than something wrong" in {
    // a truncated symbol would scan and give back the wrong data, which is worse than no symbol
    val (t, _) = run("\\qrcode{" + "A" * 5000 + "}")
    t.pictures shouldBe empty
  }

  // ---- What is drawn ---------------------------------------------------------------

  "the drawn symbol" - {
    "is square: one cell per module, plus the quiet zone on each side" in {
      val p = picture("\\qrcode[cell:3pt quiet:4]{HELLO}")
      p.width shouldBe (21 + 8) * 3.0
      p.height shouldBe (21 + 8) * 3.0
    }

    "follows the cell size, in whatever unit it is given" in {
      picture("\\qrcode[cell:4pt]{HELLO}").width shouldBe (21 + 8) * 4.0
      picture("\\qrcode[cell:0.1in]{HELLO}").width shouldBe ((21 + 8) * 7.2 +- 1e-9)
    }

    "follows the quiet zone" in {
      picture("\\qrcode[cell:3pt quiet:0]{HELLO}").width shouldBe 21 * 3.0
      picture("\\qrcode[cell:3pt quiet:2]{HELLO}").width shouldBe (21 + 4) * 3.0
    }

    "draws one rectangle per dark module, over one background rectangle" in {
      val dark = matrix("\\qrmatrix{HELLO}").map(_.count(_ == '#')).sum
      rects(picture("\\qrcode{HELLO}")).length shouldBe dark + 1
    }

    "draws no background when the light colour is none, so the page shows through" in {
      val dark = matrix("\\qrmatrix{HELLO}").map(_.count(_ == '#')).sum
      rects(picture("\\qrcode[light:none]{HELLO}")).length shouldBe dark
      rects(picture("\\qrcode[light:transparent]{HELLO}")).length shouldBe dark
    }

    "puts the symbol's first row at the top — picture coordinates run the other way" in {
      // module (0, 0) is the top-left corner of the top-left finder, and is dark in every symbol
      val side = (21 + 8) * 3.0
      rects(picture("\\qrcode[cell:3pt quiet:4]{HELLO}")) should contain((4 * 3.0, side - 5 * 3.0))
    }
  }
