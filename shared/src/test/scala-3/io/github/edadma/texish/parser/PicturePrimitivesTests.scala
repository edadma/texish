package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, Color, PictureBox, PictureOp, StubTypesetter, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Phase 3 of the picture-graphics layer: the `\picture` language primitives. These drive the full parser, so
  * they check that the surface syntax reaches the [[io.github.edadma.texish.PictureMode]] collector — coordinate
  * groups evaluate (literals, dimensions, and computed expressions alike), the active colours bake into each
  * shape's paint, and a drawing command outside a `\picture` is a clear error.
  */
class PicturePrimitivesTests extends AnyFreeSpec with Matchers:

  /** A stub typesetter that captures the [[PictureBox]]es added to the document, so a test can inspect the
    * display list the primitives built. */
  private class Capture extends StubTypesetter:
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

  private def fixture(): (Capture, Processor) =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  "\\picture builds a box of the declared size" in {
    val (t, proc) = fixture()
    proc.process("\\picture width:2in height:1in { }")
    t.pictures should have size 1
    t.pictures.head.width shouldBe 144.0  // 2in
    t.pictures.head.ascent shouldBe 72.0  // 1in
  }

  "a shape lowers to a path and a paint carrying the active colours" in {
    val ops = run("\\picture width:1in height:1in { \\fill{red} \\stroke{blue} \\rect{0 0 72 72} }")
    ops shouldBe Vector(
      PictureOp.NewPath,
      PictureOp.MoveTo(0, 0),
      PictureOp.LineTo(72, 0),
      PictureOp.LineTo(72, 72),
      PictureOp.LineTo(0, 72),
      PictureOp.Close,
      PictureOp.Paint(Some(Color("red")), Some(Color("blue"))),
    )
  }

  "coordinates may be dimensions, and parse to points" in {
    val ops = run("\\picture width:1in height:1in { \\stroke{black} \\line{0.5in 0 1in 36pt} }")
    ops should contain(PictureOp.MoveTo(36, 0))   // 0.5in
    ops should contain(PictureOp.LineTo(72, 36))  // 1in, 36pt
  }

  "an arithmetic coordinate is evaluated, not taken literally" in {
    val ops = run("\\set n {3} \\picture width:1in height:1in { \\stroke{black} \\rect{\\*{\\n}{10} 0 5 5} }")
    ops should contain(PictureOp.MoveTo(30, 0)) // \*{\n}{10} = 30
  }

  "\\circle lowers to an arc" in {
    val ops = run("\\picture width:1in height:1in { \\stroke{black} \\circle{36 36 20} }")
    ops should contain(PictureOp.Arc(36, 36, 20, 0, 2 * math.Pi, false))
  }

  "\\rotate takes degrees and records radians" in {
    val ops = run("\\picture width:1in height:1in { \\rotate{90} }")
    ops should contain(PictureOp.Rotate(math.toRadians(90)))
  }

  "\\group brackets its body with a save and restore" in {
    val ops = run("\\picture width:1in height:1in { \\group{ \\stroke{black} \\circle{0 0 1} } }")
    ops.head shouldBe PictureOp.GSave
    ops.last shouldBe PictureOp.GRestore
  }

  "a drawing command outside \\picture is an error" in {
    val (_, proc) = fixture()
    a[ParserException] should be thrownBy proc.process("\\rect{0 0 1 1}")
  }
