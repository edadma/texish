package io.github.edadma.texish.parser

import io.github.edadma.texish.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The structural apparatus of a long document, as the `document` and `book` packages provide it: the run-in
  * heading levels, the per-depth list labels, margin notes, the index, and the bibliography with the citations
  * that point into it. Each rides a piece of engine machinery — the marginal note box, the index form of a
  * contents list, the cross-reference table a `\cite` resolves through — so a regression in either the package
  * or the primitive under it is caught here.
  */
class DocumentStructureTests extends AnyFreeSpec with Matchers:

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  private def render(src: String, pkg: String = "document"): Seq[Box] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val doc = new CapturingDocument(t)
    t.document = doc
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(s"\\use{$pkg}\n" + src + "\n"))
    t.end()
    doc.shipped.toSeq

  private def chars(b: Box): List[CharBox] = b match
    case c: CharBox => List(c)
    case h: HBox    => h.boxes.toList.flatMap(chars)
    case v: VBox    => v.boxes.toList.flatMap(chars)
    case _          => Nil

  private def text(boxes: Seq[Box]): String = boxes.toList.flatMap(chars).map(_.text).mkString

  // An interword space is glue, not a character, so the char-only reading above closes every gap up. This one
  // keeps the spaces, for the assertions that are about how a run of text reads rather than what is in it.
  private def spaced(b: Box): String = b match
    case c: CharBox     => c.text
    case h: HBox        => h.boxes.map(x => if x.isSpace then " " else spaced(x)).mkString
    case v: VerticalBox => v.boxes.map(spaced).mkString
    case _              => ""

  private def spacedText(boxes: Seq[Box]): String = boxes.map(spaced).mkString

  private def fontOf(boxes: Seq[Box], mark: String): Font =
    boxes.toList.flatMap(chars).collectFirst { case c if c.text.contains(mark) => c.font }.get

  private def marginals(b: Box): List[MarginalBox] = b match
    case m: MarginalBox  => List(m)
    case v: VerticalBox  => v.boxes.toList.flatMap(marginals)
    case h: HBox         => h.boxes.toList.flatMap(marginals)
    case _               => Nil

  private def allMarginals(boxes: Seq[Box]): List[MarginalBox] = boxes.toList.flatMap(marginals)

  // ---- Heading levels ------------------------------------------------------------

  "\\subparagraph sets a bold run-in heading below \\paragraph" in {
    val boxes = render("\\subparagraph{Detail} and the text that follows it.")
    text(boxes) should include("Detail")
    fontOf(boxes, "D").style should contain("bold")
    // the body resumes at the ordinary weight on the same line
    fontOf(boxes, "f").style should not contain "bold"
  }

  // ---- List labels ---------------------------------------------------------------

  "a first-level enumerate numbers 1., 2., 3." in {
    val out = text(render("\\begin{enumerate}\\item Alpha\\item Beta\\item Gamma\\end{enumerate}"))
    out should include("1.")
    out should include("2.")
    out should include("3.")
  }

  "a nested enumerate letters its items and leaves the outer numbering alone" in {
    val out = text(
      render(
        "\\begin{enumerate}\\item Alpha\\begin{enumerate}\\item Inner\\item Other\\end{enumerate}\\item Beta\\end{enumerate}",
      ),
    )
    out should include("(a)")
    out should include("(b)")
    // the outer list resumes at 2 — a shared counter would have restarted it at 1
    out should include("2.")
  }

  "the third and fourth enumerate levels are roman then uppercase letters" in {
    val out = text(
      render(
        "\\begin{enumerate}\\item A\\begin{enumerate}\\item B\\begin{enumerate}\\item C" +
          "\\begin{enumerate}\\item D\\end{enumerate}\\end{enumerate}\\end{enumerate}\\end{enumerate}",
      ),
    )
    out should include("i.")
    out should include("A.")
  }

  "an itemize changes its bullet with depth" in {
    val out = text(render("\\begin{itemize}\\item Alpha\\begin{itemize}\\item Inner\\end{itemize}\\end{itemize}"))
    out should include("•")
    out should include("–")
  }

  "a list label is redefinable, like any other format macro" in {
    val out = text(render("\\def labelitemi {→}\\begin{itemize}\\item Alpha\\end{itemize}"))
    out should include("→")
  }

  // ---- Margin notes --------------------------------------------------------------

  "\\marginpar sets its note beside the text, in the right margin, taking no room in the line" in {
    val boxes = render("Some running text that carries a note.\\marginpar{A note.}")
    val notes = allMarginals(boxes)

    notes.length shouldBe 1
    // zero-size: the line it attaches to is set as though the note were not there
    notes.head.width shouldBe 0.0
    notes.head.height shouldBe 0.0
    // the offset clears the text block: hsize plus the gap
    notes.head.offset shouldBe (468.0 + 10.0 +- 1e-9)
    text(Seq(notes.head.content)) should include("note")
  }

  "a two-sided \\marginpar goes in the outer margin, so a verso note is a negative offset" in {
    val boxes = render("\\geometry twoside:on\nText.\\marginpar{Recto note.}\\eject\nMore.\\marginpar{Verso note.}")
    val notes = allMarginals(boxes)

    notes.length shouldBe 2
    notes.head.offset should be > 0.0
    notes(1).offset should be < 0.0
  }

  "a margin note is set ragged right, so its narrow measure does not stretch its words apart" in {
    val boxes = render("Some running text that carries a note.\\marginpar{A note long enough to wrap over three lines.}")
    val note  = allMarginals(boxes).head
    val lines = note.content.asInstanceOf[VerticalBox].boxes.collect { case h: HBox => h }

    lines.length should be > 1
    // every line but the last ends in the infinitely stretchable glue \\marginparstyle's rightskip puts there.
    // Without it the paragraph justifies: the note's body group would revert the setting before the paragraph
    // was broken, which is why the body ends with a \\par.
    // Ragged setting leaves every interword space at its natural size and lets the slack fall off the right
    // edge; justifying stretches the spaces instead. So the tell is that no space inside a line is wider than
    // any other. Without the \\par that ends the note's paragraph inside its own group, the body group would
    // revert \\marginparstyle's rightskip before the paragraph was broken and these would differ.
    val interior = lines.init.flatMap(l => l.boxes.dropRight(1).filter(_.isSpace).map(_.width)).filter(_ > 0)

    interior should not be empty
    interior.distinct.length shouldBe 1
    // and the slack really is at the right edge on at least one line
    lines.init.map(_.boxes.last.width).max should be > 0.0
  }

  "a margin note does not narrow the paragraph it sits in" in {
    val plain = render("The quick brown fox jumps over the lazy dog and keeps on running.")
    val noted = render("The quick brown fox jumps over the lazy dog and keeps on running.\\marginpar{Note.}")
    text(noted) shouldBe text(plain)
  }

  // ---- The index -----------------------------------------------------------------

  // The index is replayed straight into the current list, as \\contentslist is, so these read the lines it built
  // rather than a rendered page: one hbox per term, in the order \\indexlist chose to call the format macro.
  private def replayIndex(entries: Seq[(String, Int)]): Seq[String] =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)

    t.references = new ReferenceTable
    for (title, page) <- entries do t.references.recordList("idx", 1, "", title, page)
    t.references.commit()

    proc.process("\\def row lvl num ttl pg {\\hbox{\\ttl|\\pg}}\\indexlist{idx}{row}")
    t.mode.asInstanceOf[Builder].list.collect { case b: HBox => spaced(b) }

  "\\indexlist gathers a term's pages, in order, with repeats dropped" in {
    replayIndex(Seq("kerning" -> 7, "kerning" -> 3, "kerning" -> 7)) shouldBe Seq("kerning|3, 7")
  }

  "\\indexlist files terms alphabetically, folding case so a capital does not sort ahead of everything" in {
    val lines = replayIndex(Seq("Zebra" -> 2, "apple" -> 5, "Mongoose" -> 9))
    lines shouldBe Seq("apple|5", "Mongoose|9", "Zebra|2")
  }

  "\\indexlist emits nothing on the pass that has collected no entries" in {
    replayIndex(Seq.empty) shouldBe Seq.empty
  }

  "\\index draws nothing where it is written" in {
    val plain = text(render("A sentence about kerning in type."))
    val noted = text(render("A sentence about \\index{kerning}kerning in type."))
    noted shouldBe plain
  }

  // A whole document, run the way one really is: repeatedly over one reference table until the references stop
  // moving, so a forward reference, a contents list and an index all resolve. The last pass's pages are read.
  private def renderPasses(src: String, pkg: String = "document"): Seq[Box] =
    var pages = Seq.empty[Box]

    Console.withOut(new java.io.ByteArrayOutputStream) {
      Passes.untilStable() { () => new HeadlessTypesetter } { case t: HeadlessTypesetter =>
        val handler = new TypesetterHandler(t)
        val proc    = new Processor(handler)
        registerTypesettingPrimitives(proc, handler)
        val doc = new CapturingDocument(t)
        t.document = doc
        proc.process(s"\\use{$pkg}\n" + src + "\n")
        t.end()
        pages = doc.shipped.toSeq
      }
    }

    pages

  "\\printindex sets each term with the page it was marked on, once the passes settle" in {
    val out = spacedText(
      renderPasses("\\index{kerning}Body text about kerning and \\index{leading}leading.\\eject\\printindex"),
    )

    out should include("Index")
    out should include("kerning, 1")
    out should include("leading, 1")
    // alphabetical, not the order the two were written in
    out.indexOf("kerning, 1") should be < out.indexOf("leading, 1")
  }

  "an index merges a term marked on two pages into one line" in {
    val out = spacedText(renderPasses("\\index{kerning}First.\\eject\\index{kerning}Second.\\eject\\printindex"))

    out should include("kerning, 1, 2")
  }

  // ---- Bibliography and citations -------------------------------------------------

  "\\bibitem numbers its entries and \\cite names them" in {
    val out = text(
      render(
        "As shown before.\\cite{knuth}\n\n\\begin{thebibliography}\n" +
          "\\bibitem{knuth} Knuth, The TeXbook.\n\\bibitem{lamport} Lamport, LaTeX.\n" +
          "\\end{thebibliography}",
      ),
    )
    out should include("[1]")
    out should include("[2]")
    out should include("References")
    out should include("TeXbook")
  }

  "\\cite of several keys prints one bracketed, comma-separated group" in {
    val out = spacedText(
      render(
        "\\begin{thebibliography}\n\\bibitem{knuth} K.\n\\bibitem{lamport} L.\n\\end{thebibliography}\n" +
          "Later.\\cite{knuth lamport}",
      ),
    )
    // a backward citation resolves within the same pass
    out should include("[1, 2]")
  }

  "a \\cite before the bibliography resolves on a later pass" in {
    val out = spacedText(
      renderPasses(
        "Shown before.\\cite{lamport}\n\n\\begin{thebibliography}\n" +
          "\\bibitem{knuth} Knuth.\n\\bibitem{lamport} Lamport.\n\\end{thebibliography}",
      ),
    )

    out should include("[2]")
    out should not include "[??]"
  }


  "an unresolved \\cite shows the rerun placeholder rather than a wrong number" in {
    val out = text(render("Text.\\cite{nosuch}"))
    out should include("[??]")
  }

  // ---- The book package -----------------------------------------------------------

  "\\appendix letters a book's chapters and renames the word above them" in {
    val out = text(render("\\mainmatter\\chapter{One}\\appendix\\chapter{Data}\\section{Detail}", pkg = "book"))
    out should include("Appendix")
    out should include("Chapter")
    // the appendix chapter is A, and its sections number within it
    out should include("A.1")
  }

  "a book's float captions follow the appendix letter too" in {
    val out = spacedText(
      render(
        "\\mainmatter\\chapter{One}\\appendix\\chapter{Data}\\figure{\\caption{A chart}}",
        pkg = "book",
      ),
    )
    out should include("Figure A.1")
  }
