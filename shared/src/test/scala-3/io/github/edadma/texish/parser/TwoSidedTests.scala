package io.github.edadma.texish.parser

import io.github.edadma.texish.HeadlessTypesetter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Two-sided documents, drawn through the full parser: the `\geometry` options that turn a document two-sided and
  * name its margins by their relation to the binding, the `document` package's \ifrecto and \cleardoublepage, and
  * the `book` package's use of both — chapters opening on a recto, a running head read across the opening, and a
  * folio at the outer corner. The engine's own reflection of the frame is covered by `TwoSidedPageTests`; these
  * check the layer a document actually writes against.
  */
class TwoSidedTests extends AnyFreeSpec with Matchers:

  private class RecordingTypesetter extends HeadlessTypesetter:
    val drawn                            = new ArrayBuffer[(String, Double, Double)]
    var sheets                           = 0
    override def drawString(text: String, x: Double, y: Double): Unit = drawn += ((text, x, y))
    override def ejectPageTarget(): Unit = sheets += 1

  private def run(source: String): RecordingTypesetter =
    val t       = new RecordingTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(source))
    t.end()
    t

  private def render(pkg: String, body: String): RecordingTypesetter = run(s"\\use{$pkg}\n$body")

  /** Filler that runs to three pages, so a running head is exercised on both sides of an opening. */
  private val filler = (1 to 120).map(i => s"Filler sentence number $i for the running text.").mkString("\n\n")

  /** What each page drew in its running head, in page order — everything above the text block. */
  private def heads(t: RecordingTypesetter): Seq[String] =
    t.drawn.filter(_._3 < t.getNumber("voffset")).map(_._1).toSeq

  /** The folios, with their x positions, in page order — everything below the text block. */
  private def folios(t: RecordingTypesetter): Seq[(String, Double)] =
    t.drawn.filter(_._3 > t.getNumber("voffset") + t.getNumber("vsize")).map(f => (f._1, f._2)).toSeq

  "\\geometry" - {
    "inner and outer name the recto's margins and turn two-sided printing on" in {
      val t = run("\\geometry paperwidth:600 inner:100 outer:50\ntext")

      t.getNumber("twoside") shouldBe 1.0
      t.getNumber("hoffset") shouldBe 100.0 // the recto's inner margin is its left one
      t.getNumber("hsize") shouldBe 450.0   // 600 - 100 - 50
    }

    "an explicit twoside:off still leaves the margins it was given" in {
      val t = run("\\geometry paperwidth:600 twoside:off inner:100 outer:50\ntext")

      t.getNumber("twoside") shouldBe 0.0
      t.getNumber("hoffset") shouldBe 100.0
      t.getNumber("hsize") shouldBe 450.0
    }

    "twoside:on alone makes a document two-sided without naming a margin" in {
      run("\\geometry twoside:on\ntext").getNumber("twoside") shouldBe 1.0
    }

    "a later \\geometry that mentions neither leaves the setting alone" in {
      // adjusting the margins later must not quietly make a two-sided document one-sided again
      val t = run("\\geometry twoside:on inner:100 outer:50\n\\geometry top:20 bottom:20\ntext")

      t.getNumber("twoside") shouldBe 1.0
    }
  }

  "\\ifrecto takes its first branch on an odd folio and its second on an even one" in {
    val recto = render("document", "\\set pageno {7}\n\\ifrecto {RECTO} {VERSO}")
    recto.drawn.map(_._1) should contain("RECTO")
    recto.drawn.map(_._1) should not contain "VERSO"

    val verso = render("document", "\\set pageno {8}\n\\ifrecto {RECTO} {VERSO}")
    verso.drawn.map(_._1) should contain("VERSO")
    verso.drawn.map(_._1) should not contain "RECTO"
  }

  "\\cleardoublepage" - {
    "opens the next material on a recto, inserting a blank leaf when it would fall on a verso" in {
      val t = render("document", "one\n\\cleardoublepage\nMARKER")

      // page 1 is a recto, so the next page would be a verso: a blank leaf is shipped and MARKER opens folio 3
      t.sheets shouldBe 3
      t.drawn.map(_._1) should contain("MARKER")
    }

    "inserts nothing when the next page is already a recto" in {
      val t = render("document", "one\n\\eject\ntwo\n\\cleardoublepage\nMARKER")

      t.sheets shouldBe 3 // folios 1 and 2, then MARKER on 3 — no blank needed
      folios(t).map(_._1) shouldBe Seq("1", "2", "3")
    }

    "leaves the blank leaf genuinely blank — no folio, no running head" in {
      val t = render("document", "one\n\\cleardoublepage\nMARKER")

      // three pages ship but only two print a folio: the leaf between them carries nothing at all
      folios(t).map(_._1) shouldBe Seq("1", "3")
    }
  }

  "the two mark streams are independent" in {
    // \mark and \submark are read back separately, which is what lets a head name a division on one side of an
    // opening and a subdivision on the other.
    val t = render("document", "\\mark{CHAP}\\submark{SECT}\ntext")

    Value.display(t.getVar("firstmark")) shouldBe "CHAP"
    Value.display(t.getVar("firstsubmark")) shouldBe "SECT"
  }

  "book" - {
    "a chapter opens on a recto when the book is two-sided" in {
      val t = render("book", s"\\geometry twoside:on\n\\mainmatter\n\\chapter{One}\nshort\n\\chapter{Two}\nshort")

      t.sheets shouldBe 3                             // chapter One on folio 1, a blank leaf on 2, chapter Two on 3
      folios(t).map(_._1) shouldBe Seq("1", "3")      // the leaf between them prints nothing
    }

    "a one-sided book wastes no leaf: the next chapter takes the next page" in {
      val t = render("book", "\\mainmatter\n\\chapter{One}\nshort\n\\chapter{Two}\nshort")

      t.sheets shouldBe 2
      folios(t).map(_._1) shouldBe Seq("1", "2")
    }

    "two-sided, the head names the chapter on a verso and the section on a recto" in {
      val t = render(
        "book",
        s"\\geometry twoside:on top:60 bottom:60\n\\mainmatter\n\\chapter{Alpha}\n\\section{Beta}\n$filler",
      )

      // Three pages: the chapter opens on folio 1 and clears the section stream as it does, so that page's head
      // is empty — the section it would name has not started. Folio 2 is a verso and names the chapter; folio 3
      // is a recto and names the section running through it.
      t.sheets shouldBe 3
      heads(t) shouldBe Seq("Alpha", "Beta")
    }

    "one-sided, the head names the chapter on every page" in {
      val t = render("book", s"\\geometry top:60 bottom:60\n\\mainmatter\n\\chapter{Alpha}\n\\section{Beta}\n$filler")

      heads(t).distinct shouldBe Seq("Alpha") // unchanged from before two-sided printing existed
    }

    "two-sided, the folio sits at the outer corner of each page" in {
      val t = render("book", "\\geometry paperwidth:600 twoside:on inner:100 outer:50\n\\mainmatter\none\n\\eject\ntwo")

      // the recto's text block spans 100..550 and the verso's 50..500, so the folio hugging the edge away from
      // the spine lands near 550 on the recto and at 50 on the verso — one at each outer corner of the opening
      val Seq((recto, rectoX), (verso, versoX)) = folios(t)

      recto shouldBe "1"
      rectoX should be > 500.0
      verso shouldBe "2"
      versoX shouldBe 50.0
    }
  }

  "the two-sided demo renders, and every leaf it inserts is blank" in {
    // The demo in scripts/ is what the corpus renderer draws and what a reader learns the feature from, so a
    // construct renamed out from under it is caught here rather than only by rendering the corpus.
    val bytes = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get("scripts/twosided-demo.script"))
    val t     = run(new String(bytes, "UTF-8"))

    // more sheets ship than folios are printed: the difference is the blank leaves the chapters opened on a
    // recto with, each carrying neither head nor folio
    t.sheets should be > folios(t).length
    folios(t).map(_._1) should contain("1") // the main matter restarts arabic folios at one
  }
