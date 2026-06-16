package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, CharBox, FloatBox, HBox, StubTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The `floats` package layers LaTeX-style `\figure`/`\table` floats with numbered `\caption`s over the
  * `\topinsert` primitive and the `counters` package: each float detaches to the top of its page, and a caption
  * written inside one steps that float's own counter (figure and table numbered independently) and typesets a
  * line such as `Figure 1: …`.
  */
class FigureTests extends AnyFreeSpec with Matchers:

  private def fixture(): (StubTypesetter, Processor) =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  private def texts(box: Box): List[String] = box match
    case v: VBox    => v.boxes.toList.flatMap(texts)
    case h: HBox    => h.boxes.toList.flatMap(texts)
    case c: CharBox => List(c.text)
    case _          => Nil

  /** The floated blocks contributed to the vertical list, in contribution order. */
  private def floatContents(t: StubTypesetter): List[Box] =
    t.paragraph()
    t.mode.asInstanceOf[Builder].list.collect { case f: FloatBox => f.content }.toList

  "\\figure numbers its caption as a figure" in {
    val (t, proc) = fixture()

    proc.process("\\use{floats}\\figure{\\caption{Overview}}")

    val figs = floatContents(t)
    figs should have length 1

    val words = texts(figs.head)
    words should contain("Figure")
    words should contain("Overview")
    words.exists(_.contains("1")) shouldBe true
  }

  "\\table numbers its caption as a table, on its own counter" in {
    val (t, proc) = fixture()

    proc.process("\\use{floats}\\table{\\caption{Results}}")

    val words = texts(floatContents(t).head)
    words should contain("Table")
    words should contain("Results")
    words.exists(_.contains("1")) shouldBe true
  }

  "figure and table counters advance independently" in {
    val (t, proc) = fixture()

    proc.process("\\use{floats}\\figure{\\caption{Alpha}}\\table{\\caption{Beta}}\\figure{\\caption{Gamma}}")

    val blocks = floatContents(t)
    blocks should have length 3

    val first  = texts(blocks(0))
    val second = texts(blocks(1))
    val third  = texts(blocks(2))

    first  should contain("Figure")
    first  should contain("Alpha")
    first.exists(_.contains("1")) shouldBe true

    second should contain("Table")
    second should contain("Beta")
    second.exists(_.contains("1")) shouldBe true     // table is on its own counter, also at 1

    third  should contain("Figure")
    third  should contain("Gamma")
    third.exists(_.contains("2")) shouldBe true       // second figure, so the figure counter has reached 2
  }

  "\\figure[b] sinks the figure to the bottom of its page" in {
    val (t, proc) = fixture()

    proc.process("\\use{floats}\\figure[b]{\\caption{Lower}}")
    t.paragraph()

    val floats = t.mode.asInstanceOf[Builder].list.collect { case f: FloatBox => f }
    floats should have length 1
    floats.head.edgeTop shouldBe false                 // [b] overrode the default top placement
    texts(floats.head.content) should contain("Figure")
  }

  "a caption rides at the top of the floated block, ahead of body content" in {
    val (t, proc) = fixture()

    proc.process("\\use{floats}\\figure{\\caption{Plot} body matter}")

    val words = texts(floatContents(t).head)
    words.indexOf("Figure") should be < words.indexOf("body")
  }
