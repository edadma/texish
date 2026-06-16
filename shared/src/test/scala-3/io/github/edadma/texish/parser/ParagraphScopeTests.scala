package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, DocumentMode, HBox, StubTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Scoped paragraph-shape parameters (`\leftskip`, …) set inside an environment must apply to every paragraph of
  * that environment's body — including the last one, which `\end` closes — and must NOT leak out past the
  * environment. The witness is the per-line left-margin glue: each paragraph's line opens with a glue whose natural
  * size is the `\leftskip` in force when the line was broken.
  */
class ParagraphScopeTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: StubTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def fixture(): (StubTypesetter, CapturingDocument, Processor) =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    (t, doc, proc)

  /** The lines (HBoxes) of every shipped page, in document order. */
  private def lines(box: Box): List[HBox] = box match
    case v: VBox => v.boxes.toList.flatMap(lines)
    case h: HBox => List(h)
    case _       => Nil

  /** The width of a line's leading margin box — a rigid `\leftskip` glue sets to its natural size, so this is the
    * `\leftskip` that line was broken with. */
  private def leftskipOf(line: HBox): Double = line.boxes.headOption.map(_.width).getOrElse(-1.0)

  private def leftskips(doc: CapturingDocument): List[Double] =
    doc.shipped.toList.flatMap(lines).map(leftskipOf)

  "a scoped \\leftskip applies to the last paragraph of a nested environment and does not leak out" in {
    val (t, doc, proc) = fixture()

    val src =
      """\set raggedbottom {1}
        |\newenvironment outer {\set leftskip {20}} {}
        |\newenvironment inner {\set leftskip {50}} {}
        |base
        |
        |\begin{outer}
        |oone
        |
        |\begin{inner}
        |ione
        |
        |itwo
        |\end{inner}
        |
        |otwo
        |\end{outer}
        |
        |final
        |""".stripMargin

    proc.process(src)
    t.end()

    // base 0 · outer¶1 20 · inner¶1 50 · inner¶2 50 · outer¶2 20 · final 0
    leftskips(doc) shouldBe List(0.0, 20.0, 50.0, 50.0, 20.0, 0.0)
  }

  "a single environment's scoped \\leftskip applies to every body paragraph, even with no trailing blank line" in {
    val (t, doc, proc) = fixture()

    val src =
      """\set raggedbottom {1}
        |\newenvironment quote {\set leftskip {30}} {}
        |\begin{quote}
        |first
        |
        |second
        |\end{quote}
        |after
        |""".stripMargin

    proc.process(src)
    t.end()

    leftskips(doc) shouldBe List(30.0, 30.0, 0.0)
  }

  "an environment that sets \\leftskip as the first content of the document still applies it" in {
    val (t, doc, proc) = fixture()

    val src =
      """\set raggedbottom {1}
        |\newenvironment quote {\set leftskip {40}} {}
        |\begin{quote}
        |only
        |\end{quote}
        |after
        |""".stripMargin

    proc.process(src)
    t.end()

    leftskips(doc) shouldBe List(40.0, 0.0)
  }

  "a plain { } group at a paragraph start does not break the paragraph" in {
    val (t, doc, proc) = fixture()

    // {\bold x} y must stay one line — the inline group closes without a paragraph break, unlike \end
    proc.process("\\set raggedbottom {1}\n{\\bold here} there\n")
    t.end()

    lines(doc.shipped.head) should have length 1
  }
