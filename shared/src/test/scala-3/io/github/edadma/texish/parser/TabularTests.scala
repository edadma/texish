package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, CharBox, HBox, Mode, RuleBox, HeadlessTypesetter, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** \tabular is the LaTeX-style ergonomic wrapper over \halign: a column spec ({lcr}, with | for vertical rules)
  * instead of a hand-rolled template, \\ between rows, and \hline for a horizontal rule. It lowers to \halign, so
  * these tests assert the same layout facts HAlignTests does — column count, equal per-column widths, padding to
  * the widest cell, vertical/horizontal rules — but driven from the friendlier surface. The HeadlessTypesetter makes
  * every character 6 wide.
  */
class TabularTests extends AnyFreeSpec with Matchers:

  private class CaptureMode(val t: Typesetter) extends Mode:
    val rows                      = ArrayBuffer[Box]()
    def init(): Unit              = ()
    infix def add(box: Box): Unit = rows += box
    def result: Box | Null        = null

  private def run(src: String): Seq[Box] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val cap = new CaptureMode(t)
    t.push(cap)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(src)) // swallow any underfull warnings
    cap.rows.toSeq

  private def hrows(rows: Seq[Box]): Seq[HBox]  = rows.collect { case h: HBox => h }
  private def cells(row: Box): Seq[HBox]        = row.asInstanceOf[HBox].boxes.map(_.asInstanceOf[HBox])
  private def colWidths(row: Box): Seq[Double]  = row.asInstanceOf[HBox].boxes.map(_.width)
  private def text(box: Box): String = box match
    case c: CharBox => c.text
    case h: HBox    => h.boxes.map(text).mkString
    case _          => ""

  "\\tabular{lcr} lays out one row with three columns" in {
    val rows = hrows(run("\\tabular{lcr}{a&b&c}"))
    rows should have size 1
    colWidths(rows(0)) should have size 3
  }

  "a column spec aligns left, centre, and right with the right glue layout" in {
    // l = #\hfil  → content then fill: the first box of the cell is the content
    // r = \hfil#  → fill then content: the first box is glue, the content is last
    // c = \hfil#\hfil → glue on both sides: three boxes
    val row = cells(hrows(run("\\tabular{lcr}{a&b&c}")).head)
    row(0).boxes.head shouldBe a[CharBox]                  // left: content first
    row(1).boxes should have size 3                        // centre: glue both sides
    row(2).boxes.head.isInstanceOf[CharBox] shouldBe false // right: leading glue
    row(2).boxes.last shouldBe a[CharBox]                  // right: content last
  }

  "every row's columns line up (equal per-column widths across rows)" in {
    val rows = hrows(run("\\tabular{ll}{a&bb\\\\ccc&d}"))
    rows should have size 2
    colWidths(rows(0)) shouldBe colWidths(rows(1))
    rows(0).width shouldBe rows(1).width
  }

  "a column is as wide as its widest cell (shorter cells pad up)" in {
    val rows = hrows(run("\\tabular{l}{aaaa\\\\b}"))
    rows should have size 2
    colWidths(rows(0)).head shouldBe colWidths(rows(1)).head
    colWidths(rows(0)).head should be >= 24.0 // at least the 4 chars of aaaa
  }

  "\\\\ terminates a row" in {
    hrows(run("\\tabular{l}{a\\\\b}")) should have size 2
  }

  "a trailing \\\\ does not add a blank row" in {
    hrows(run("\\tabular{l}{a\\\\b\\\\}")) should have size 2
  }

  "| between columns puts a vertical rule, running to the line height, in the cell" in {
    val cell = cells(hrows(run("\\tabular{|l}{a}")).head).head
    val rule = cell.boxes.collectFirst { case r: RuleBox => r }.get
    rule.ascent shouldBe 8.0  // the line's ascent in the stub font
    rule.descent shouldBe 2.0 // and its descent
  }

  "\\hline draws a rule spanning the table width" in {
    val rows    = run("\\tabular{ll}{a&bb\\\\\\hline ccc&d}")
    val rule    = rows.collectFirst { case r: RuleBox => r }.get
    val rowWide = hrows(rows).head.width
    rule.width shouldBe rowWide // matches the table width, not the full hsize
  }

  "a leading \\hline draws a rule above the first row" in {
    val rows = run("\\tabular{l}{\\hline a}")
    rows.collectFirst { case r: RuleBox => r } should be(defined)
    hrows(rows) should have size 1
  }

  "an unknown column specifier is an error" in {
    a[ParserException] should be thrownBy run("\\tabular{lxr}{a&b&c}")
  }

  "an empty column spec is an error" in {
    a[ParserException] should be thrownBy run("\\tabular{}{a}")
  }
