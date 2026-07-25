package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Anchor, Box, Color, GlyphBox, PictureBox, PictureOp, HeadlessTypesetter, TexishException, Typesetter}
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

  "\\opacity scales the fill and stroke alpha baked into a paint" in {
    val ops = run("\\picture width:1in height:1in { \\fill{red} \\stroke{blue} \\opacity{0.5} \\rect{0 0 72 72} }")
    ops should contain(PictureOp.Paint(Some(Color("red", 0.5)), Some(Color("blue", 0.5))))
  }

  "\\fillopacity and \\strokeopacity set the two channels independently" in {
    val ops = run("\\picture width:1in height:1in { \\fill{red} \\stroke{blue} \\fillopacity{0.25} \\strokeopacity{0.75} \\rect{0 0 72 72} }")
    ops should contain(PictureOp.Paint(Some(Color("red", 0.25)), Some(Color("blue", 0.75))))
  }

  "a \\group restores the opacity it changed" in {
    val ops = run(
      "\\picture width:1in height:1in { \\fill{red} \\group{ \\opacity{0.3} \\rect{0 0 10 10} } \\rect{0 0 20 20} }",
    )
    // inside the group: alpha 0.3; after the group: back to opaque
    ops should contain(PictureOp.Paint(Some(Color("red", 0.3)), None))
    ops should contain(PictureOp.Paint(Some(Color("red")), None))
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

  "\\fontglyph places a glyph from a named typeface at a codepoint" in {
    // The headless glyph seam uses the codepoint itself as the glyph index, so the placed GlyphBox carries the
    // requested codepoint. gClef is U+E050 = 57424. The default anchor is the baseline.
    val ops = run("\\picture width:1in height:1in { \\fill{black} \\fontglyph{36 36}{bravura}{32}{57424} }")
    val placed = ops.collect { case p: PictureOp.Place => p }
    placed should have size 1
    placed.head match
      case PictureOp.Place(g: GlyphBox, anchor, x, y) =>
        g.glyph shouldBe 0xe050
        g.font.typeface shouldBe "bravura"
        g.font.size shouldBe 32.0
        anchor shouldBe Anchor.Baseline
        x shouldBe 36.0
        y shouldBe 36.0
      case other => fail(s"expected a placed GlyphBox, got $other")
  }

  "\\fontglyph honours an explicit anchor" in {
    val ops = run("\\picture width:1in height:1in { \\fill{black} \\fontglyph anchor:center {0 0}{bravura}{40}{57476} }")
    val placed = ops.collect { case p: PictureOp.Place => p }
    placed.head.anchor shouldBe Anchor.Center
    placed.head.box.asInstanceOf[GlyphBox].glyph shouldBe 0xe084 // timeSig4
  }

  "\\glyphwidth measures a glyph and yields a number usable in a coordinate" in {
    // the headless backend reports every glyph as 6 wide with a zero bearing, so the measured ink width is 6
    val ops = run("\\picture width:1in height:1in { \\stroke{black} \\line{\\glyphwidth{bravura}{40}{65} 0  0 0} }")
    ops should contain(PictureOp.MoveTo(6.0, 0.0))
  }

  "a drawing command outside \\picture is an error" in {
    val (_, proc) = fixture()
    a[TexishException] should be thrownBy proc.process("\\rect{0 0 1 1}")
  }

  // \arrow / \arrowhead lower to the existing path + paint ops, so they need no new backend support; these check the
  // geometry reaches the display list. The head is filled in the pen colour (fill set, stroke clear), the shaft is
  // stroked in it (stroke set, fill clear), and the shaft is shortened so it meets the back of the head.
  "\\arrow draws a filled head at the tip and a shortened stroked shaft" in {
    val ops = run("\\picture width:1in height:1in { \\stroke{black} \\arrow head:triangle size:7 {0 0 20 0} }")
    ops should contain(PictureOp.MoveTo(20, 0))               // the head's tip sits on the endpoint
    ops should contain(PictureOp.Paint(Some(Color("black")), None)) // the head, filled in the pen colour
    ops should contain(PictureOp.LineTo(13, 0))               // the shaft stops a head-length short of the tip
    ops should contain(PictureOp.Paint(None, Some(Color("black")))) // the shaft, stroked in the pen colour
  }

  "\\arrow heads:both caps both ends" in {
    val ops = run("\\picture width:1in height:1in { \\stroke{black} \\arrow head:triangle heads:both size:7 {0 0 30 0} }")
    ops.count { case PictureOp.Paint(Some(_), None) => true; case _ => false } shouldBe 2
    ops should contain(PictureOp.MoveTo(30, 0)) // tip of the end head
    ops should contain(PictureOp.MoveTo(0, 0))  // tip of the start head, pointing back from b
  }

  "\\arrowhead head:dot lowers to a filled disc on the tip" in {
    val ops = run("\\picture width:1in height:1in { \\stroke{black} \\arrowhead head:dot size:8 {0 0 10 0} }")
    ops should contain(PictureOp.Arc(10, 0, 4, 0, 2 * math.Pi, false)) // radius = size/2, centred on the tip
    ops should contain(PictureOp.Paint(Some(Color("black")), None))
  }

  "\\arrowhead defaults to the stealth head" in {
    val ops = run("\\picture width:1in height:1in { \\stroke{black} \\arrowhead size:10 {0 0 10 0} }")
    // a stealth head has a concave notch, so its outline is four points (tip, base, notch, base), not three
    ops.count { case _: PictureOp.LineTo => true; case _ => false } shouldBe 3
  }

  "an unknown arrowhead style is an error" in {
    val (_, proc) = fixture()
    a[TexishException] should be thrownBy
      proc.process("\\picture width:1in height:1in { \\stroke{black} \\arrowhead head:wedge {0 0 1 0} }")
  }

  "\\arrow outside \\picture is an error" in {
    val (_, proc) = fixture()
    a[TexishException] should be thrownBy proc.process("\\arrow{0 0 1 1}")
  }

  // \path arrow:… caps the path with a head oriented to its TRUE end tangent, not the start→end chord. The curve
  // below starts at (0,40) and ends at (72,0) with its last control point at (36,0), so its end tangent is purely
  // horizontal even though the chord is diagonal. A head pointing horizontally has both its base corners at the
  // same x (tip-x minus head-length); a head pointing along the chord would not. So the base x's distinguish them.
  "\\path arrow:end orients the head to the end tangent, not the chord" in {
    val ops = run(
      "\\picture width:2in height:1in { \\stroke{black} " +
        "\\path arrow:end head:triangle size:7 { \\moveto{0 40} \\curveto{0 0  36 0  72 0} } }",
    )
    // the head tip lies on the horizontal tangent line through the endpoint (y = 0), pushed a little past x = 72
    ops.exists { case PictureOp.MoveTo(x, 0.0) => x >= 72.0; case _ => false } shouldBe true
    // both base corners share one x ⇒ the head points horizontally (the true tangent); along the diagonal chord
    // (72,0)−(0,40) they would not
    val baseXs = ops.collect { case PictureOp.LineTo(x, _) => x }
    baseXs should have size 2
    baseXs(0) shouldBe baseXs(1)
    ops.last shouldBe PictureOp.Paint(Some(Color("black")), None) // the head is drawn last, over the path end
  }

  "\\path arrow:both caps both ends of the path" in {
    val ops = run(
      "\\picture width:2in height:1in { \\stroke{black} " +
        "\\path arrow:both head:triangle size:6 { \\moveto{0 0} \\lineto{50 0} } }",
    )
    ops.count { case PictureOp.Paint(Some(_), None) => true; case _ => false } shouldBe 2
    ops.exists { case PictureOp.MoveTo(x, 0.0) => x >= 50.0; case _ => false } shouldBe true // head off the right end
    ops.exists { case PictureOp.MoveTo(x, 0.0) => x <= 0.0; case _  => false } shouldBe true // head off the left end
  }

  "\\path with no arrow option draws no head" in {
    val ops = run("\\picture width:2in height:1in { \\stroke{black} \\path{ \\moveto{0 0} \\lineto{50 0} } }")
    ops.exists { case PictureOp.Paint(Some(_), None) => true; case _ => false } shouldBe false
  }

  "an unknown \\path arrow value is an error" in {
    val (_, proc) = fixture()
    a[TexishException] should be thrownBy
      proc.process("\\picture width:1in height:1in { \\stroke{black} \\path arrow:middle { \\moveto{0 0} \\lineto{1 0} } }")
  }
