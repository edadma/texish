package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, HBox, HeadlessTypesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for the greedy line-break fallback's progress guarantee.
  *
  * When Knuth-Plass finds no solution within tolerance, lines are filled greedily. If a box does not fit and cannot
  * be hyphenated, the filler backs up to the last interword space so a box that *did* fit is not stranded before the
  * one that did not. The opening `\leftskip` margin glue reports as a space, so when the first content box already
  * fills the line and the next will not fit, the run scanned back was the whole line and the only "space" before it
  * was that leading margin: backing up past it emptied the line and re-queued the same boxes, consuming none — an
  * infinite loop. The filler must instead keep the content on the line (an overfull line that makes progress).
  */
class GreedyLineBreakTests extends AnyFreeSpec with Matchers:

  private def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    t.set("hsize", 100.0)
    (t, proc)

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  "two full-width boxes in one paragraph each take their own line rather than looping" in {
    val (t, proc) = fixture()
    // Each \hbox to:\hsize is exactly as wide as the line; two of them with no breakpoint between have no
    // Knuth-Plass solution, so the greedy fallback runs. The leading \leftskip glue once tricked the back-up
    // into emptying the line and re-queuing both boxes forever.
    quietly(proc.process("\\noindent\\hbox to:\\hsize{A}\\hbox to:\\hsize{A}"))
    t.paragraph()

    val lineBoxes = t.mode.asInstanceOf[Builder].list.collect { case b: HBox => b }
    lineBoxes.length shouldBe 2
  }

  "three full-width boxes likewise yield three lines" in {
    val (t, proc) = fixture()
    quietly(proc.process("\\noindent\\hbox to:\\hsize{A}\\hbox to:\\hsize{A}\\hbox to:\\hsize{A}"))
    t.paragraph()

    val lineBoxes = t.mode.asInstanceOf[Builder].list.collect { case b: HBox => b }
    lineBoxes.length shouldBe 3
  }
