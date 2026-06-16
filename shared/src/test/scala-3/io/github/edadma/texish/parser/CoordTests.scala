package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, PictureBox, PictureOp, HeadlessTypesetter, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The parenthesised coordinate sublanguage: Cartesian `(x,y)`, polar `(a:r)`, and named `(name)` coordinates,
  * both as a pure parser and driven through `\picture` primitives — interoperating with the old bare-scalar form.
  */
class CoordTests extends AnyFreeSpec with Matchers:

  private def approx(a: Double, b: Double): Boolean = math.abs(a - b) < 1e-6

  "the Coord parser" - {
    val noVars  = (_: String) => Option.empty[Double]
    val noUnits = (_: String) => Option.empty[Double]
    val noNamed = (_: String) => Option.empty[(Double, Double)]

    "Cartesian components are full expressions with units" in {
      Coord.parse("(2in, 3in)", noVars, noUnits, noNamed) shouldBe (144.0, 216.0)
      val (x, y) = Coord.parse("(1+1, 2*3)", noVars, noUnits, noNamed)
      x shouldBe 2.0; y shouldBe 6.0
    }

    "polar (angle:radius) converts degrees and radius to a point" in {
      val (x0, y0) = Coord.parse("(0:10)", noVars, noUnits, noNamed)
      assert(approx(x0, 10.0) && approx(y0, 0.0))
      val (x9, y9) = Coord.parse("(90:10)", noVars, noUnits, noNamed)
      assert(approx(x9, 0.0) && approx(y9, 10.0))
      val (x6, y6) = Coord.parse("(60:1in)", noVars, noUnits, noNamed)
      assert(approx(x6, 36.0) && approx(y6, 72 * math.sin(math.toRadians(60))))
    }

    "a named coordinate is looked up" in {
      val named = Map("A" -> (5.0, 7.0))
      Coord.parse("(A)", noVars, noUnits, named.get) shouldBe (5.0, 7.0)
    }

    "components may reference variables" in {
      val vars = Map("R" -> 36.0)
      val (x, y) = Coord.parse("(R*cosd(0), 0)", vars.get, noUnits, noNamed)
      assert(approx(x, 36.0) && approx(y, 0.0))
    }

    "a comma inside a function call is not the coordinate separator" in {
      val (x, y) = Coord.parse("(atan2d(1,1), 2)", (_: String) => None, noUnits, noNamed)
      assert(approx(x, 45.0) && approx(y, 2.0))
    }

    "malformed coordinates and unknown names are reported" in {
      a[Coord.CoordException] should be thrownBy Coord.parse("2,3", noVars, noUnits, noNamed) // no parens
      a[Coord.CoordException] should be thrownBy Coord.parse("(Z)", noVars, noUnits, noNamed) // unknown name
    }
  }

  "through \\picture primitives" - {
    def run(src: String): Vector[PictureOp] =
      val t       = new Capture
      val handler = new TypesetterHandler(t)
      val proc    = new Processor(handler)
      registerTypesettingPrimitives(proc, handler)
      proc.process(src)
      t.pictures should have size 1
      t.pictures.head.displayList

    "paren and polar coordinates drive a shape, interoperating with bare scalars" in {
      val ops = run("\\picture width:1in height:1in { \\stroke{black} \\line{(0,0) (90:10)} }")
      ops should contain(PictureOp.MoveTo(0, 0))
      val Some(PictureOp.LineTo(x, y)) = ops.collectFirst { case l: PictureOp.LineTo => l }: @unchecked
      assert(approx(x, 0.0) && approx(y, 10.0)) // 90 degrees, radius 10
    }

    "named coordinates: \\coordinate stores a point that (name) reads back" in {
      val ops = run(
        "\\picture width:1in height:1in { \\coordinate{A}{(0,0)} \\coordinate{B}{(60:36)} \\stroke{black} \\line{(A) (B)} }",
      )
      ops should contain(PictureOp.MoveTo(0, 0))
      val Some(PictureOp.LineTo(x, y)) = ops.collectFirst { case l: PictureOp.LineTo => l }: @unchecked
      assert(approx(x, 36 * math.cos(math.toRadians(60))) && approx(y, 36 * math.sin(math.toRadians(60))))
    }

    "the old bare-scalar form still works unchanged" in {
      val ops = run("\\picture width:1in height:1in { \\stroke{black} \\line{0 0 10 20} }")
      ops should contain(PictureOp.MoveTo(0, 0))
      ops should contain(PictureOp.LineTo(10, 20))
    }
  }

  private class Capture extends HeadlessTypesetter:
    val pictures = ArrayBuffer[PictureBox]()
    override infix def add(box: Box): Typesetter =
      box match
        case pb: PictureBox => pictures += pb
        case _              =>
      super.add(box)
