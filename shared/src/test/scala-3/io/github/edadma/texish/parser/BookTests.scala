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

  private class RecordingTypesetter extends HeadlessTypesetter:
    val drawn = new ArrayBuffer[String]
    override def drawString(text: String, x: Double, y: Double): Unit = drawn += text

  private def render(body: String): Seq[String] =
    val t       = new RecordingTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(s"\\use{book}\n$body"))
    t.end()
    t.drawn.toSeq

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
