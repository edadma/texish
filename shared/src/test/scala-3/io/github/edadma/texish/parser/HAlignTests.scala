package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, CharBox, HBox, Mode, RuleBox, HeadlessTypesetter, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** \halign builds an aligned table: a format (template) line with one `#` per column (`&` between columns, `\cr`
  * ending each line), then content rows. Each column is set to the widest cell's natural size; the template's
  * surrounding material (e.g. \hfil) does the per-cell alignment. \noalign drops raw vertical material between
  * rows; \omit suppresses one cell's template. The HeadlessTypesetter makes every character 6 wide.
  */
class HAlignTests extends AnyFreeSpec with Matchers:

  /** A plain capturing parent mode: \halign adds its finished rows to modeStack(1), which is this. Being a bare
    * Mode (not VerticalMode) it adds rows verbatim, with no interline glue, so the rows are easy to inspect.
    */
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

  /** Typeset a body into an \hbox (where # and & are not alignment-active) and return its concatenated text, so
    * literal-character behaviour outside a table is easy to assert.
    */
  private def hboxText(body: String): String =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.process(s"\\setbox probe \\hbox{$body}")
    proc.handler.get("probe") match
      case Value.Native(b: Box) => text(b)
      case _                    => ""

  private def hrows(rows: Seq[Box]): Seq[HBox]    = rows.collect { case h: HBox => h }
  private def colWidths(row: Box): Seq[Double]    = row.asInstanceOf[HBox].boxes.map(_.width)
  private def text(box: Box): String = box match
    case c: CharBox => c.text
    case h: HBox    => h.boxes.map(text).mkString
    case _          => ""

  "a 2-column table lays out one HBox per row, each with one cell per column" in {
    val rows = hrows(run("\\halign{#\\hfil&#\\hfil\\cr A&BB\\cr CCC&D\\cr}"))
    rows should have size 2
    colWidths(rows(0)) should have size 2
  }

  "every row's columns line up (equal per-column widths across rows)" in {
    val rows = hrows(run("\\halign{#\\hfil&#\\hfil\\cr A&BB\\cr CCC&D\\cr}"))
    colWidths(rows(0)) shouldBe colWidths(rows(1))
    rows(0).width shouldBe rows(1).width
  }

  "a column is as wide as its widest cell (shorter cells pad up, not the wide one shrinking)" in {
    val rows = hrows(run("\\halign{#\\hfil\\cr AAAA\\cr B\\cr}"))
    rows should have size 2
    colWidths(rows(0)).head shouldBe colWidths(rows(1)).head // B's cell padded up to AAAA's width
    colWidths(rows(0)).head should be >= 24.0                // at least the 4 chars of AAAA
  }

  "a cell with no alignment glue in its template is still padded to the column width" in {
    // bare # template (no \hfil): the short cell must still fill its column so columns line up
    val rows = hrows(run("\\halign{#&#\\cr AAAA&B\\cr C&DDDD\\cr}"))
    rows should have size 2
    colWidths(rows(0)) shouldBe colWidths(rows(1))
  }

  "a trailing \\cr does not add a blank row" in {
    hrows(run("\\halign{#\\hfil\\cr A\\cr B\\cr}")) should have size 2
  }

  "a table whose last row has no trailing \\cr still emits that row" in {
    hrows(run("\\halign{#\\hfil\\cr A\\cr B}")) should have size 2
  }

  "\\noalign inserts raw vertical material between rows" in {
    val rows = run("\\halign{#\\hfil\\cr A\\cr\\noalign{X}B\\cr}")
    hrows(rows) should have size 2 // A and B
    rows.exists { case c: CharBox => c.text.contains("X"); case _ => false } shouldBe true
  }

  "\\omit suppresses one cell's column template" in {
    val rows = hrows(run("\\halign{Z#\\cr A\\cr\\omit B\\cr}"))
    rows should have size 2
    text(rows(0)) should include("Z")     // template "Z" prepended to the normal cell
    text(rows(1)) should not include "Z"  // omitted on the \omit cell
  }

  "a space after \\cr does not leak into the next cell" in {
    val rows = hrows(run("\\halign{#\\cr X\\cr Y\\cr}"))
    rows should have size 2
    text(rows(0)) shouldBe "X" // not " X"
    text(rows(1)) shouldBe "Y"
  }

  "\\tabskip inserts glue before, between, and after the columns" in {
    val row  = hrows(run("\\set tabskip {7}\\halign{#\\hfil&#\\hfil\\cr A&B\\cr}")).head
    val gaps = row.asInstanceOf[HBox].boxes.filter(_.width == 7.0)
    gaps should have size 3 // before col0, between, after col1
  }

  "without \\tabskip the columns abut (no gap boxes)" in {
    val row = hrows(run("\\halign{#\\hfil&#\\hfil\\cr A&B\\cr}")).head
    row.asInstanceOf[HBox].boxes should have size 2 // just the two cells
  }

  "\\noalign{\\hrule} draws a rule spanning the table width" in {
    val rows    = run("\\halign{#\\hfil&#\\hfil\\cr A&BB\\cr\\noalign{\\hrule}CCC&D\\cr}")
    val rule    = rows.collectFirst { case r: RuleBox => r }.get
    val rowWide = hrows(rows).head.width
    rule.width shouldBe rowWide // the rule matches the table width, not the full hsize
  }

  "\\noalign{\\hrule width:N} keeps its explicit width" in {
    val rows = run("\\halign{#\\hfil\\cr A\\cr\\noalign{\\hrule width:5}B\\cr}")
    rows.collectFirst { case r: RuleBox => r }.get.width shouldBe 5.0
  }

  "\\vrule in a cell runs to the line height" in {
    val cell = hrows(run("\\halign{\\vrule#\\cr A\\cr}")).head.boxes.head.asInstanceOf[HBox]
    val rule = cell.boxes.collectFirst { case r: RuleBox => r }.get
    rule.ascent shouldBe 8.0  // the line's ascent in the stub font
    rule.descent shouldBe 2.0 // and its descent
  }

  "& and # are literal characters outside a table" in {
    noException should be thrownBy hboxText("AT&T")
    hboxText("AT&T") shouldBe "AT&T"
    hboxText("C#") shouldBe "C#"
  }

  "\\& and \\# are literals even where & / # would be active" in {
    hboxText("A\\&B\\#C") shouldBe "A&B#C"
    // inside a table cell, \& is a literal & (not a column break), so this stays a single column
    val rows = hrows(run("\\halign{#\\hfil\\cr A\\&B\\cr}"))
    rows should have size 1
    text(rows(0)) should include("&")
  }
