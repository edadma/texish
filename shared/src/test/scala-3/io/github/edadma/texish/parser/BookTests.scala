package io.github.edadma.texish.parser

import io.github.edadma.texish.HeadlessTypesetter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The `book` package (packages/book.texish) drawn through the full parser. These check the behaviour that is
  * specific to the book class and most likely to break silently: sections numbered within their chapter (and reset
  * when a new chapter opens), chapters numbered only in the main matter, the running-head mark naming the current
  * chapter, and the folio style switching from lowercase-roman front matter to an arabic main matter that restarts
  * at one. A regression in the package — or in the engine pieces it leans on (the resettable pageno, \counterwithin,
  * the number formatters) — is caught here rather than only in a rendered PDF.
  */
class BookTests extends AnyFreeSpec with Matchers:

  /** Records every string drawn with its position, and which sheet it was drawn on: a sheet's strings are drawn
    * before it is ejected, so each eject closes one entry of `sheets` and opens the next.
    */
  private class RecordingTypesetter extends HeadlessTypesetter:
    val drawn  = new ArrayBuffer[(String, Double, Double)]
    val sheets = ArrayBuffer(new ArrayBuffer[String])
    override def drawString(text: String, x: Double, y: Double): Unit =
      drawn += ((text, x, y))
      sheets.last += text
    override def ejectPageTarget(): Unit = sheets += new ArrayBuffer[String]

    /** The strings of each sheet shipped, in order. */
    def sheetText: Seq[Seq[String]] = sheets.init.map(_.toSeq).toSeq

    /** The folios, with their x positions, in page order — everything drawn below the text block. */
    def folios: Seq[(String, Double)] =
      drawn.filter(_._3 > getNumber("voffset") + getNumber("vsize")).map(f => (f._1, f._2)).toSeq

  private def run(body: String): RecordingTypesetter =
    val t       = new RecordingTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(s"\\use{book}\n$body"))
    t.end()
    t

  private def render(body: String): Seq[String] = run(body).drawn.map(_._1).toSeq

  "sections are numbered within their chapter" in {
    val drawn = render("\\mainmatter\n\\chapter{Intro}\n\\section{Background}\n\\subsection{Detail}\nbody")
    drawn should contain("Chapter") // the chapter heading's label word
    drawn should contain("1.1")     // section: chapter.section
    drawn should contain("1.1.1")   // subsection: chapter.section.subsection
  }

  "a new chapter resets the section counter and renumbers from its own number" in {
    val drawn = render(
      "\\mainmatter\n\\chapter{One}\n\\section{A}\n\\chapter{Two}\n\\section{B}\nbody",
    )
    drawn should contain("2.1") // first section of the second chapter, not 1.2
  }

  "front-matter chapters carry no number; main-matter chapters do" in {
    val front = render("\\frontmatter\n\\chapter{Preface}\nbody")
    front should contain("Preface")
    front should not contain "Chapter"

    val main = render("\\mainmatter\n\\chapter{Preface}\nbody")
    main should contain("Chapter")
  }

  "the running head names the current chapter" in {
    val drawn = render(
      "\\mainmatter\n\\chapter{First}\nbody\n\n\\vfill\\eject more",
    )
    // the chapter title is drawn once as the heading and again as the running head on the next page
    drawn.count(_ == "First") should be >= 2
  }

  "front matter uses roman folios; the main matter restarts arabic at one" in {
    // a frontmatter page (roman i) then a mainmatter page (arabic 1, restarted), each forced to ship
    val drawn = render(
      "\\frontmatter\nfront\n\n\\mainmatter\nmain\n\n\\vfill\\eject",
    )
    drawn should contain("i") // first front-matter folio
    drawn should contain("1") // main matter restarts at arabic one
  }

  "the title page carries no folio but still counts as a page" in {
    val drawn = render(
      "\\geometry twoside:off\n\\title{The Title}\n\\frontmatter\n\\maketitle\n\\chapter{Preface}\nbody\n\n\\vfill\\eject",
    )
    drawn should contain("Title") // the title page itself is set
    drawn should not contain "i" // the title page's own folio is not set
    drawn should contain("ii")   // the page after it is the second front-matter page
  }

  "two-sided, a chapter after the title page opens on the next recto, past a blank verso" in {
    val t = run("\\title{The Title}\n\\frontmatter\n\\maketitle\n\\chapter{Preface}\nbody")

    t.sheetText.map(_.contains("Title")) shouldBe Seq(true, false, false) // title, its blank verso, the preface
    t.sheetText(1) shouldBe empty
    t.folios.map(_._1) shouldBe Seq("iii")
  }

  /** A main matter whose first page ends with no division, and a chapter after it. */
  private val chapterAfterOnePage = "\\mainmatter\nfirst\n\n\\chapter{Two}\nbody"

  "a book is two-sided by default: a chapter after one page opens on folio 3, behind a blank leaf" in {
    val t = run(chapterAfterOnePage)

    t.getNumber("twoside") shouldBe 1.0
    t.sheetText.length shouldBe 3
    t.sheetText(1) shouldBe empty                  // the inserted leaf carries neither head nor folio
    t.folios.map(_._1) shouldBe Seq("1", "3")
  }

  "\\geometry twoside:off makes the book one-sided where the default is two-sided" in {
    run(chapterAfterOnePage).folios.map(_._1) shouldBe Seq("1", "3")

    val one = run(s"\\geometry twoside:off\n$chapterAfterOnePage")
    one.getNumber("twoside") shouldBe 0.0
    one.sheetText.length shouldBe 2                // no leaf is wasted one-sided
    one.folios.map(_._1) shouldBe Seq("1", "2")
  }

  "a document's own \\geometry that does not mention twoside keeps the book two-sided" in {
    val t = run(s"\\geometry paper:a5 margin:20mm\n$chapterAfterOnePage")

    t.getNumber("twoside") shouldBe 1.0
    t.folios.map(_._1) shouldBe Seq("1", "3")
  }

  "by default the folio alternates to the outer corner of each page" in {
    val t = run("\\mainmatter\none\n\n\\vfill\\eject\ntwo\n\n\\vfill\\eject\nthree")
    val Seq((r1, x1), (v2, x2), (r3, x3)) = t.folios

    (r1, v2, r3) shouldBe ("1", "2", "3")
    x2 should be < x1 // the verso's folio sits at the left, the rectos' at the right
    x2 should be < x3
    x1 shouldBe x3
  }

  "\\halftitle sets the title alone on a recto with no folio, and leaves its verso blank" in {
    val t = run("\\title{Lone}\n\\author{Someone}\n\\date{Today}\n\\halftitle\nafter")

    t.sheetText.length shouldBe 3
    t.sheetText(0) shouldBe Seq("Lone")            // no author, no date, no head, no folio
    t.sheetText(1) shouldBe empty                  // its verso
    t.sheetText(2) should contain("after")
    t.folios.map(_._1) shouldBe Seq("3")
  }

  "\\copyrightpage sets its text at the foot of the title's verso, with no folio" in {
    val t = run(
      "\\title{Name}\n\\maketitle\n\\copyrightpage{Copyright 2026 Someone.\n\nAll rights reserved.}\n\\chapter{One}\nbody",
    )

    t.sheetText.length shouldBe 3                  // title, copyright page, chapter: no blank leaf between
    t.sheetText(0) should contain("Name")
    t.sheetText(1) should contain allOf ("Copyright", "reserved.")
    t.sheetText(2) should contain("One")
    t.folios.map(_._1) shouldBe Seq("3")       // only the chapter's page carries a folio

    // pushed down to the foot: below the middle of the text block
    val y = t.drawn.find(_._1 == "Copyright").get._3
    y should be > t.getNumber("voffset") + t.getNumber("vsize") / 2
  }

  "the opening pages never put two blank pages in a row" in {
    def blankRuns(t: RecordingTypesetter) = t.sheetText.sliding(2).count(_.forall(_.isEmpty))

    val full = run(
      "\\title{Name}\n\\frontmatter\n\\halftitle\n\\maketitle\n\\copyrightpage{Copyright 2026}\n\\contents\n" +
        "\\mainmatter\n\\chapter{One}\nbody",
    )
    // half-title, its blank verso, the title, the copyright page on the title's verso, the contents on the next recto
    full.sheetText(0) shouldBe Seq("Name")
    full.sheetText(1) shouldBe empty
    full.sheetText(2) should contain("Name")
    full.sheetText(3) should contain("Copyright")
    full.sheetText(4) should contain("Contents")
    blankRuns(full) shouldBe 0

    // without a copyright page the title's verso is the one blank leaf before the contents
    val bare = run("\\title{Name}\n\\frontmatter\n\\halftitle\n\\maketitle\n\\contents\n\\mainmatter\n\\chapter{One}\nbody")
    bare.sheetText(3) shouldBe empty
    bare.sheetText(4) should contain("Contents")
    blankRuns(bare) shouldBe 0
  }

  "the title page's empty foot does not outlast it" in {
    val drawn = render("\\title{The Title}\n\\frontmatter\n\\maketitle\nbody\n\n\\vfill\\eject\nmore\n\n\\vfill\\eject")
    drawn should contain("ii")
    drawn should contain("iii")
  }
