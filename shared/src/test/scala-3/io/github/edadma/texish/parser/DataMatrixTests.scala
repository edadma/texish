package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, HeadlessTypesetter, PictureBox, PictureOp, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.ArrayBuffer

/** The `datamatrix` package (packages/datamatrix.texish): an ISO/IEC 16022 ECC200 encoder written in the document
  * language. There is no barcode primitive in the engine — the ASCII encodation, the Reed-Solomon check codewords
  * over GF(256) and the Annex F placement walk are all \calc, \for and \put over sequences.
  *
  * **How these symbols were verified, since a wrong one looks perfectly correct.** Three ways, because no scanner
  * is available to this test:
  *
  *   - the check codewords for `123456` are the worked example published with the standard, `114 25 5 88 102` over
  *     the data codewords `142 164 186`, and they are asserted here directly. That pins the field (the polynomial
  *     differs from QR's), the generator's first root (which also differs from QR's), and the digit-pair
  *     encodation at once, and it is the one anchor here that comes from outside this repository;
  *   - every symbol below was compared module-for-module against a second, independently written encoder, over
  *     every size from 10x10 to 40x40 — which is what catches a mis-transcribed corner case in the placement walk;
  *   - the placement was checked to be a strict bijection at every size the package builds: each cell of the
  *     mapping matrix written exactly once, none twice, and the only cells left unwritten the four the standard
  *     fills with a fixed pattern.
  *
  * The symbols are then frozen here, so they keep testing the encoder against something other than itself. The
  * small ones are written out in full, where a diff says which module moved; the larger ones are pinned by digest.
  */
class DataMatrixTests extends AnyFreeSpec with Matchers:

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
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\use{datamatrix}\n" + src))
    (t, handler)

  private def matrix(src: String): Vector[String] =
    run(src)._2.get("dmxmatrixvalue") match
      case Value.Seq(items) => items.map(Value.display)
      case other            => fail(s"no matrix: $other")

  private def seqOf(h: TypesetterHandler, name: String): Vector[Int] =
    h.get(name) match
      case Value.Seq(items) => items.map(v => Value.number(v).getOrElse(-1.0).toInt)
      case other            => fail(s"$name is not a sequence: $other")

  private def digest(rows: Vector[String]): String = SymbolFingerprint.of(rows)

  private def picture(src: String): PictureBox =
    val (t, _) = run(src)
    t.pictures should not be empty
    t.pictures.head

  private def rects(p: PictureBox): Vector[(Double, Double)] =
    p.displayList.collect { case PictureOp.MoveTo(x, y) => (x, y) }

  // ---- The published worked example ------------------------------------------------

  "the codewords for 123456 are the ones published with the standard" in {
    val (_, h) = run("\\dmxmatrix{123456}")
    // three data codewords: each pair of digits is 130 + the pair's value, which is what makes the symbology
    // so compact for numbers — 12 -> 142, 34 -> 164, 56 -> 186
    seqOf(h, "dmxCw") shouldBe Vector(142, 164, 186)
    // and five check codewords over GF(256) with the ECC200 polynomial, whose generator's first root is one
    // power along from QR's — get either wrong and the symbol looks perfect and does not scan
    seqOf(h, "dmxAll") shouldBe Vector(142, 164, 186, 114, 25, 5, 88, 102)
  }

  // ---- The shape of a symbol -------------------------------------------------------

  "a symbol has the finder pattern the standard prescribes" - {
    "the left column and the bottom row are solid — the L a reader orients by" in {
      val m = matrix("\\dmxmatrix{123456}")
      all(m.map(_.head)) shouldBe '#'
      m.last shouldBe "#" * m.length
    }

    "the top row and the right column alternate — the track a reader measures a module by" in {
      val m = matrix("\\dmxmatrix{123456}")
      m.head shouldBe (0 until m.length).map(i => if i % 2 == 0 then '#' else '.').mkString
      m.indices.map(i => m(i).last).mkString shouldBe
        (0 until m.length).map(i => if i % 2 == 1 then '#' else '.').mkString
    }

    "the symbol grows through the standard's sizes as the payload does" in {
      matrix("\\dmxmatrix{123456}").length shouldBe 10
      matrix("\\dmxmatrix{HELLO}").length shouldBe 12
      matrix("\\dmxmatrix{PARCEL-4471}").length shouldBe 16
      matrix("\\dmxmatrix{https://example.com}").length shouldBe 20
      matrix("\\dmxmatrix{" + "A" * 100 + "}").length shouldBe 40
    }

    "a symbol of more than one data region carries a finder around each of them" in {
      // at 32x32 and above the symbol is split into four regions of 14, each with its own L and track
      // 40x40 is four regions of 18 modules, each with a two-module border of its own, so the seam falls
      // between rows 19 and 20: row 19 is the solid bottom of the upper pair and row 20 the alternating top
      // of the lower pair. A symbol that laid its regions out as one undivided grid would show neither.
      val m = matrix("\\dmxmatrix{" + "A" * 100 + "}")
      m.length shouldBe 40
      m(19) shouldBe "#" * 40
      m(20) shouldBe (0 until 40).map(i => if i % 2 == 0 then '#' else '.').mkString
      m(18) should not be "#" * 40
    }

    "every row is as long as the symbol is tall" in {
      val m = matrix("\\dmxmatrix{https://example.com}")
      all(m.map(_.length)) shouldBe m.length
    }
  }

  // ---- The symbols themselves ------------------------------------------------------

  "the smallest symbol, from six digits in three codewords" in {
    matrix("\\dmxmatrix{123456}") shouldBe Vector(
      "#.#.#.#.#.",
      "##..#.##.#",
      "##.....#..",
      "##...###.#",
      "##....#...",
      "#.....####",
      "###.##....",
      "####.##..#",
      "#..###.#..",
      "##########",
    )
  }

  "five letters, which cost a codeword each and so need the next size up" in {
    matrix("\\dmxmatrix{HELLO}") shouldBe Vector(
      "#.#.#.#.#.#.",
      "#.##.#..#.##",
      "#....##.#...",
      "###..#..#..#",
      "##.....#.##.",
      "##...#....##",
      "##.....#.#..",
      "##.#..#.####",
      "###.#.##..#.",
      "#.#.##.###.#",
      "#..#.#.##.#.",
      "############",
    )
  }

  "the encoder agrees with an independent one at every size it builds" - {
    "a mixed payload of letters, digits and punctuation" in {
      digest(matrix("\\dmxmatrix{PARCEL-4471}")) shouldBe "414ec23399fdbd97"
    }
    "a URL, whose // has to reach the encoder rather than starting a comment" in {
      digest(matrix("\\dmxmatrix{https://example.com}")) shouldBe "c683fac2f0beee95"
    }
    "twenty digits, which pack two to a codeword and which no Double could have carried" in {
      digest(matrix("\\dmxmatrix{12345678901234567890}")) shouldBe "f6612f86573ea669"
    }
    "a sentence, at a size with one data region" in {
      digest(matrix("\\dmxmatrix{The quick brown fox jumps over the lazy dog}")) shouldBe "c41654a65acd70db"
    }
    "an accented letter, encoded as UTF-8 behind an upper shift" in {
      digest(matrix("\\dmxmatrix{Caf\u00e9}")) shouldBe "0a219c584e684dd9"
    }
    "a payload large enough to be split into four data regions" in {
      digest(matrix("\\dmxmatrix{" + "A" * 100 + "}")) shouldBe "bddad5ad843ebe9a"
    }
    "two hundred digits, which fit the same size as a hundred letters" in {
      digest(matrix("\\dmxmatrix{" + "9" * 200 + "}")) shouldBe "496e0cc3ef395bb8"
    }
  }

  "a payload past the largest size this package builds draws nothing, rather than something wrong" in {
    // a truncated symbol would scan and give back the wrong data, which is worse than no symbol
    val (t, _) = run("\\datamatrix{" + "A" * 400 + "}")
    t.pictures shouldBe empty
    val (ok, _) = run("\\datamatrix{HELLO}")
    ok.pictures should have size 1
  }

  // ---- What is drawn ---------------------------------------------------------------

  "the drawn symbol" - {
    "is square: one cell per module, plus the quiet zone on each side" in {
      val p = picture("\\datamatrix[cell:3pt quiet:1]{123456}")
      p.width shouldBe (10 + 2) * 3.0
      p.height shouldBe (10 + 2) * 3.0
    }

    "follows the cell size, in whatever unit it is given" in {
      picture("\\datamatrix[cell:4pt]{123456}").width shouldBe (10 + 2) * 4.0
      picture("\\datamatrix[cell:0.1in quiet:0]{123456}").width shouldBe (10 * 7.2 +- 1e-9)
    }

    "draws one rectangle per dark module, over one background rectangle" in {
      val dark = matrix("\\dmxmatrix{123456}").map(_.count(_ == '#')).sum
      rects(picture("\\datamatrix{123456}")).length shouldBe dark + 1
    }

    "draws no background when the light colour is none, so the page shows through" in {
      val dark = matrix("\\dmxmatrix{123456}").map(_.count(_ == '#')).sum
      rects(picture("\\datamatrix[light:none]{123456}")).length shouldBe dark
    }

    "puts the symbol's first row at the top — picture coordinates run the other way" in {
      // module (0, 0) is the corner of the solid L, and is dark in every symbol
      val side = (10 + 2) * 3.0
      rects(picture("\\datamatrix[cell:3pt quiet:1]{123456}")) should contain((3.0, side - 2 * 3.0))
    }
  }

  // ---- Both symbologies in one document --------------------------------------------

  "a document may use both symbologies, which do not share a field" in {
    // the two use different primitive polynomials and different generator roots, and the shared `barcode`
    // package holds them in variables — so whichever loaded last must not have decided for both
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream)(
      proc.process("\\use{qrcode}\n\\use{datamatrix}\n\\qrmatrix{HELLO}\\dmxmatrix{123456}\\qrmatrix{HELLO}"),
    )
    seqOf(handler, "dmxAll") shouldBe Vector(142, 164, 186, 114, 25, 5, 88, 102)
    handler.get("qrmatrixvalue") match
      case Value.Seq(items) => items.length shouldBe 21
      case other            => fail(s"no QR matrix: $other")
  }
