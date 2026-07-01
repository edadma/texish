package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, CharBox, DocumentMode, Glue, HBox, HSpaceBox, HeadlessTypesetter, Typesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** End-to-end reordering of a paragraph's text into visual order. A document sets \rtl (or embeds a
  * right-to-left word in a left-to-right paragraph), is run through the engine over the metrics-only
  * HeadlessTypesetter, and the characters of the set line are read back left to right. Because every
  * backend draws a run in the order given, that visual string is what the reader would see. */
class RtlParagraphTests extends AnyFreeSpec with Matchers:

  // Hebrew letters, named so the expected strings below stay readable.
  private val A = 'א'
  private val B = 'ב'
  private val G = 'ג'
  private val D = 'ד'
  private val Sh = 'ש'
  private val L = 'ל'

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def render(src: String): Seq[Box] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\set raggedbottom {1}\n" + src + "\n"))
    t.end()
    doc.shipped.toSeq

  // The characters of one line, left to right, with a single space standing in for each interword glue —
  // the leading and trailing margin glue and \parfillskip sit at the ends and have no character beside them.
  private def lineText(h: HBox): String =
    val sb    = new StringBuilder
    val boxes = h.boxes
    for i <- boxes.indices do
      boxes(i) match
        case c: CharBox => sb.append(c.text)
        case s if s.isSpace =>
          // interword spacing has a character box on both sides; the margin glue and \parfillskip sit at
          // the ends, with nothing or a non-character box beside them
          if sb.nonEmpty && i + 1 < boxes.length && boxes(i + 1).isInstanceOf[CharBox] then sb.append(' ')
        case _ =>
    sb.toString

  // Every set line, in order: an HBox that directly holds character boxes.
  private def lines(boxes: Seq[Box]): List[String] =
    def collect(b: Box): List[HBox] = b match
      case h: HBox if h.boxes.exists(_.isInstanceOf[CharBox]) => List(h)
      case h: HBox                                            => h.boxes.toList.flatMap(collect)
      case v: VBox                                            => v.boxes.toList.flatMap(collect)
      case _                                                  => Nil
    boxes.toList.flatMap(collect).map(lineText)

  private def visual(src: String): String = lines(render(src)).mkString(" ")

  "a right-to-left word is reversed within its box" in {
    visual(s"\\rtl $A$B$G") shouldBe s"$G$B$A"
  }

  "two right-to-left words reverse both their order and their letters" in {
    visual(s"\\rtl $A$B $G$D") shouldBe s"$D$G $B$A"
  }

  "an embedded Latin word stays left-to-right while the Hebrew around it reverses" in {
    visual(s"\\rtl $A Hello $B") shouldBe s"$B Hello $A"
  }

  "digits embedded in a right-to-left line keep their left-to-right order" in {
    visual(s"\\rtl $Sh 12 $L") shouldBe s"$L 12 $Sh"
  }

  "a Hebrew word embedded in a left-to-right paragraph reverses in place" in {
    visual(s"Hello $A$B world") shouldBe s"Hello $B$A world"
  }

  "a pure left-to-right paragraph is unchanged" in {
    visual("Hello world") shouldBe "Hello world"
  }

  "parentheses around right-to-left text are mirrored so they enclose it correctly" in {
    // logical "(אב)" reverses to ")בא(" and the brackets mirror back, so the reader sees "(בא)".
    visual(s"\\rtl ($A$B)") shouldBe s"($B$A)"
  }

  "brackets in a left-to-right paragraph are not mirrored" in {
    visual("(ab)") shouldBe "(ab)"
  }

  "\\ltr restores left-to-right after \\rtl" in {
    visual(s"\\rtl\\ltr Hello $A$B world") shouldBe s"Hello $B$A world"
  }

  // The first set line, an HBox holding the line's boxes left to right.
  private def firstLine(boxes: Seq[Box]): HBox =
    def collect(b: Box): List[HBox] = b match
      case h: HBox if h.boxes.exists(_.isInstanceOf[CharBox]) => List(h)
      case h: HBox                                            => h.boxes.toList.flatMap(collect)
      case v: VBox                                            => v.boxes.toList.flatMap(collect)
      case _                                                  => Nil
    boxes.toList.flatMap(collect).head

  // Which side of the text the first-line indent box sits on, read off the assembled line.
  private def indentSide(src: String): String =
    val bs        = firstLine(render(src)).boxes
    val indentIdx = bs.indexWhere { case s: HSpaceBox => s.indent; case _ => false }
    val charIdxs  = bs.indices.filter(i => bs(i).isInstanceOf[CharBox])
    if indentIdx < 0 then "none"
    else if indentIdx < charIdxs.head then "left"
    else if indentIdx > charIdxs.last then "right"
    else "middle"

  "the first-line indent of a right-to-left paragraph sits on the right (leading) edge" in {
    indentSide(s"\\rtl\\indent $A$B $G$D") shouldBe "right"
  }

  "the first-line indent of a left-to-right paragraph with embedded Hebrew stays on the left" in {
    indentSide(s"\\indent Hello $A$B world") shouldBe "left"
  }

  // The (left, right) physical margin width of the first line, in points.
  private def margins(src: String): (Double, Double) =
    val bs = firstLine(render(src)).boxes
    (bs.head.width, bs.last.width)

  "\\leftskip indents the leading margin — the left under \\ltr, the right under \\rtl" in {
    margins(s"\\set leftskip {30}\n$A$B $G$D") shouldBe (30.0, 0.0)
    margins(s"\\rtl\\set leftskip {30}\n$A$B $G$D") shouldBe (0.0, 30.0)
  }
