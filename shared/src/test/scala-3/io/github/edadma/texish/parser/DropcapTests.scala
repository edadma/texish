package io.github.edadma.texish.parser

import io.github.edadma.texish.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The `document` package's \dropcap: an oversized initial spanning the opening lines of a paragraph,
  * with the lines carved out around it. The letter's size is derived by measurement (an X of the body
  * font for the cap height, the letter test-set at a reference size for the ratio), so these tests
  * compute the same expectation from rendered probe glyphs rather than hard-coding sizes.
  */
class DropcapTests extends AnyFreeSpec with Matchers:

  /** The stock HeadlessTypesetter reports the same fixed extents at every font size, which would make a
    * measured dropcap invisible to these tests. This variant carries the size in the render font and scales
    * the fixed metrics linearly (0.8/0.2 of the size above/below the baseline, 0.6 per character), the way a
    * real font's metrics scale.
    */
  private class ScalingHeadlessTypesetter extends HeadlessTypesetter:
    private def sizeOf(rf: RenderFont): Double = rf.substring(rf.lastIndexOf('@') + 1).toDouble
    override def makeFont(font: FontFace, size: Double): RenderFont = s"$font@$size"
    override def getTextExtents(text: String, font: RenderFont): TextExtents =
      val s = sizeOf(font)
      TextExtents(0, -0.8 * s, text.length * 0.6 * s, s, text.length * 0.6 * s, 0)
    override def charWidth(font: RenderFont, c: Char): Double = 0.6 * sizeOf(font)
    override def glyphExtents(font: RenderFont, glyph: Int): TextExtents =
      val s = sizeOf(font)
      TextExtents(0, -0.8 * s, 0.6 * s, s, 0.6 * s, 0)

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def render(src: String): Seq[Box] =
    val t       = new ScalingHeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\use{document}\n" + src + "\n"))
    t.end()
    doc.shipped.toSeq

  /** Every CharBox in the tree, descending through the overlap/smash/shift wrappers the dropcap is built from. */
  private def deepChars(b: Box): List[CharBox] = b match
    case c: CharBox    => List(c)
    case h: HBox       => h.boxes.toList.flatMap(deepChars)
    case v: VBox       => v.boxes.toList.flatMap(deepChars)
    case s: ShiftBox   => deepChars(s.box)
    case p: PhantomBox => deepChars(p.inner)
    case _             => Nil

  private def shiftBoxes(b: Box): List[ShiftBox] = b match
    case s: ShiftBox   => s :: shiftBoxes(s.box)
    case h: HBox       => h.boxes.toList.flatMap(shiftBoxes)
    case v: VBox       => v.boxes.toList.flatMap(shiftBoxes)
    case p: PhantomBox => shiftBoxes(p.inner)
    case _             => Nil

  /** The text lines of the shipped pages: HBoxes sitting in a vertical list that contain letters
    * (the folio line is digits only, so it filters out).
    */
  private def textLines(boxes: Seq[Box]): List[HBox] =
    boxes.toList.flatMap {
      case v: VBox =>
        v.boxes.toList.flatMap {
          case h: HBox  => if deepChars(h).exists(_.text.exists(_.isLetter)) then List(h) else Nil
          case v2: VBox => textLines(List(v2))
          case _        => Nil
        }
      case _ => Nil
    }

  /** Where a line's ink starts: the rigid margin material before the first box that carries text. */
  private def inkStart(line: HBox): Double =
    line.boxes.takeWhile(b => deepChars(b).isEmpty).map(_.width).sum

  /** The mirror measure from the line's right edge, for the right-to-left carve. */
  private def inkEnd(line: HBox): Double =
    line.boxes.reverse.takeWhile(b => deepChars(b).isEmpty).map(_.width).sum

  private val filler =
    "ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et " +
      "dolore magna aliqua ut enim ad minim veniam quis nostrud exercitation ullamco laboris nisi ut " +
      "aliquip ex ea commodo consequat duis aute irure dolor in reprehenderit in voluptate velit esse"

  /** The body-font probe metrics the package's own measurement would see: the tight cap height of X
    * and the letter's tight height and width at the 10pt body size.
    */
  private def probe(letter: String): (Double, Double, Double) =
    val cs   = render(s"X $letter").flatMap(deepChars)
    val x    = cs.find(_.text == "X").get
    val l    = cs.find(_.text == letter).get
    (x.ascent, l.ascent, l.width)

  "the initial is sized to span the requested lines and lowered onto the last spanned baseline" in {
    val (capX, ascL, _) = probe("L")
    val boxes           = render(s"\\dropcap{L}{orem} $filler")

    // top at the first line's cap height, baseline on line 2 of a 12pt-leading paragraph
    val target = 12.0 + capX
    val big    = boxes.flatMap(deepChars).maxBy(_.font.size)
    big.text shouldBe "L"
    big.font.size shouldBe (10.0 * target / ascL +- 0.01)
    big.ascent shouldBe (target +- 0.01)

    // the letter rides a shift of one baseline, placing it a line below the first baseline
    val shifted = boxes.toList.flatMap(shiftBoxes).filter(s => deepChars(s).exists(_.text == "L"))
    shifted should not be empty
    shifted.head.shift shouldBe (12.0 +- 1e-9)
  }

  "the opening lines are carved out and the rest return to the full measure" in {
    val (capX, ascL, wdL) = probe("L")
    val size              = 10.0 * (12.0 + capX) / ascL
    val indent            = wdL * size / 10.0 + 3.0 // letter width at the computed size plus the default gap

    val ls = textLines(render(s"\\dropcap{L}{orem} $filler"))
    ls.length should be >= 4
    inkStart(ls(0)) shouldBe (indent +- 0.01)
    inkStart(ls(1)) shouldBe (indent +- 0.01)
    inkStart(ls(2)) shouldBe (0.0 +- 0.01)
  }

  "lines: deepens the carve-out and enlarges the letter to match" in {
    val (capX, ascL, wdL) = probe("L")
    val size              = 10.0 * (24.0 + capX) / ascL
    val indent            = wdL * size / 10.0 + 3.0

    val boxes = render(s"\\dropcap[3]{L}{orem} $filler")
    val big   = boxes.flatMap(deepChars).maxBy(_.font.size)
    big.ascent shouldBe (24.0 + capX +- 0.01)

    val ls = textLines(boxes)
    ls.length should be >= 5
    inkStart(ls(0)) shouldBe (indent +- 0.01)
    inkStart(ls(2)) shouldBe (indent +- 0.01)
    inkStart(ls(3)) shouldBe (0.0 +- 0.01)
  }

  "gap: sets the space between the letter and the carved text" in {
    val (capX, ascL, wdL) = probe("L")
    val size              = 10.0 * (12.0 + capX) / ascL

    val ls = textLines(render(s"\\dropcap[2][12]{L}{orem} $filler"))
    inkStart(ls(0)) shouldBe (wdL * size / 10.0 + 12.0 +- 0.01)
  }

  "the rest of the opening word goes through \\dropcaptext — small capitals unless redefined" in {
    val sc = render(s"\\dropcap{L}{orem} $filler").flatMap(deepChars).find(_.text.contains("orem")).get
    sc.font.style should contain("smallcaps")

    val bold = render(s"\\def dropcaptext t {\\textbf{\\t}}\n\\dropcap{L}{orem} $filler")
      .flatMap(deepChars).find(_.text.contains("orem")).get
    bold.font.style should contain("bold")
    bold.font.style should not contain "smallcaps"
  }

  "under a right-to-left base the carve-out and the letter move to the right edge" in {
    val (capX, ascL, wdL) = probe("L")
    val size              = 10.0 * (12.0 + capX) / ascL
    val indent            = wdL * size / 10.0 + 3.0

    val ls = textLines(render(s"\\rtl\\dropcap{L}{orem} $filler"))
    ls.length should be >= 4
    inkEnd(ls(0)) shouldBe (indent +- 0.01)
    inkEnd(ls(1)) shouldBe (indent +- 0.01)
    inkEnd(ls(2)) shouldBe (0.0 +- 0.01)
  }

  "a paragraph shorter than the spanned lines still renders" in {
    val boxes = render("\\dropcap{A}{lpha} beta.")
    boxes.flatMap(deepChars).map(_.text).mkString should include("lpha")
    boxes.flatMap(deepChars).maxBy(_.font.size).text shouldBe "A"
  }
