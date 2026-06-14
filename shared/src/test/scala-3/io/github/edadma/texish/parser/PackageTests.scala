package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, PictureBox, PictureOp, StubTypesetter, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The opt-in package system (`\use`) and the coordinate accessors (`\xof`/`\yof`) that let a package — like the
  * bundled `chem` — be written in the document language itself.
  */
class PackageTests extends AnyFreeSpec with Matchers:

  private def fixture(): (Capture, Processor) =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  "\\xof / \\yof extract a coordinate's components as numbers" in {
    val (_, proc) = fixture()
    proc.process("\\set px {\\xof{(3in, 4)}}")
    proc.process("\\set py {\\yof{(3in, 4)}}")
    proc.handler.get("px") shouldBe Value.Num(216.0) // 3in
    proc.handler.get("py") shouldBe Value.Num(4.0)
  }

  "\\use loads a bundled package, defining its macros" in {
    val (_, proc) = fixture()
    proc.handler.get("bond") shouldBe Value.Undefined // not loaded yet
    proc.process("\\use{chem}")
    proc.handler.get("bond") shouldBe a[Value.Macro]
    proc.handler.get("dbond") shouldBe a[Value.Macro]
    proc.handler.get("atom") shouldBe a[Value.Macro]
  }

  "\\use is idempotent — loading twice is harmless" in {
    val (_, proc) = fixture()
    noException should be thrownBy proc.process("\\use{chem}\\use{chem}")
    proc.markPackageLoaded("chem") shouldBe false // already recorded
  }

  "\\use of an unknown package is an error" in {
    val (_, proc) = fixture()
    a[ParserException] should be thrownBy proc.process("\\use{nope}")
  }

  "the chem \\bond macro draws a line between two coordinates, shortened at each end" in {
    val (t, proc) = fixture()
    proc.process("\\use{chem}")
    proc.process(
      "\\picture width:2in height:1in { \\stroke{black} \\coordinate{A}{(0,0)} \\coordinate{B}{(100,0)} \\bond{(A)}{(B)} }",
    )
    t.pictures should have size 1
    val ops = t.pictures.head.displayList
    // a horizontal bond from (0,0) to (100,0), shortened by bondgap (default 9) at each end
    val Some(PictureOp.MoveTo(mx, my)) = ops.collectFirst { case m: PictureOp.MoveTo => m }: @unchecked
    val Some(PictureOp.LineTo(lx, ly)) = ops.collectFirst { case l: PictureOp.LineTo => l }: @unchecked
    mx shouldBe 9.0 +- 1e-9
    lx shouldBe 91.0 +- 1e-9
    my shouldBe 0.0 +- 1e-9
    ly shouldBe 0.0 +- 1e-9
  }

  "the chem \\dbond macro draws two parallel lines" in {
    val (t, proc) = fixture()
    proc.process("\\use{chem}")
    proc.process(
      "\\picture width:2in height:1in { \\stroke{black} \\coordinate{A}{(0,0)} \\coordinate{B}{(100,0)} \\dbond{(A)}{(B)} }",
    )
    val ops    = t.pictures.head.displayList
    val moveYs = ops.collect { case PictureOp.MoveTo(_, y) => y }
    moveYs should have size 2                 // two lines
    moveYs.head should not be moveYs(1)        // offset to either side of the axis
  }

  private class Capture extends StubTypesetter:
    val pictures = ArrayBuffer[PictureBox]()
    override infix def add(box: Box): Typesetter =
      box match
        case pb: PictureBox => pictures += pb
        case _              =>
      super.add(box)
