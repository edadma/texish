package io.github.edadma.texish.parser

import io.github.edadma.texish.{
  Box,
  Builder,
  HBox,
  HeadlessTypesetter,
  LabelBox,
  ReferenceTable,
  TocEntry,
  TocMarkBox,
}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Cross-references: `\label` binds a name to the current reference text and to the page it lands on, `\ref` prints
  * the text, `\pageref` the page. Forward references need a second pass over a shared [[ReferenceTable]]; a backward
  * reference (label before its `\ref`) already resolves within one pass, which is what these tests exercise.
  */
class ReferenceTests extends AnyFreeSpec with Matchers:

  // ---- the table on its own --------------------------------------------------

  "a declared label is readable in the same pass (backward reference)" in {
    val r = new ReferenceTable
    r.declare("sec:intro", "3.1")
    r.refText("sec:intro") shouldBe Some("3.1")
  }

  "a label carries its kind and name for \\autoref and \\nameref" in {
    val r = new ReferenceTable
    r.declare("sec:intro", "3.1", "Section", "Introduction")
    r.refKind("sec:intro") shouldBe Some("Section")
    r.refName("sec:intro") shouldBe Some("Introduction")
  }

  "kind and name are absent for a bare label" in {
    val r = new ReferenceTable
    r.declare("plain", "3.1")
    r.refKind("plain") shouldBe None
    r.refName("plain") shouldBe None
  }

  "setPage keeps a label's kind and name" in {
    val r = new ReferenceTable
    r.declare("fig:frog", "2", "Figure", "A frog")
    r.setPage("fig:frog", 5)
    r.refKind("fig:frog") shouldBe Some("Figure")
    r.refName("fig:frog") shouldBe Some("A frog")
    r.refPage("fig:frog") shouldBe Some(5)
  }

  "kind and name resolve forward across a commit" in {
    val r = new ReferenceTable
    r.refKind("fig:later") shouldBe None
    r.declare("fig:later", "2", "Figure", "A frog")
    r.commit()
    r.refKind("fig:later") shouldBe Some("Figure")
    r.refName("fig:later") shouldBe Some("A frog")
  }

  "an undeclared label has no text" in {
    val r = new ReferenceTable
    r.refText("nope") shouldBe None
  }

  "a forward reference resolves only after a pass is committed" in {
    val r = new ReferenceTable
    // pass 1: nothing resolved yet
    r.refText("eq:euler") shouldBe None
    r.declare("eq:euler", "7")
    r.commit()
    // pass 2: the previous pass's value is now visible even before this pass redeclares it
    r.refText("eq:euler") shouldBe Some("7")
  }

  "a page is reported only once it is a real folio" in {
    val r = new ReferenceTable
    r.declare("fig:frog", "2")
    r.refPage("fig:frog") shouldBe None // page still 0
    r.setPage("fig:frog", 5)
    r.refPage("fig:frog") shouldBe Some(5)
  }

  "commit reports whether the references moved" in {
    val r = new ReferenceTable
    r.declare("a", "1")
    r.commit() shouldBe true // empty -> {a}
    r.declare("a", "1")
    r.commit() shouldBe false // unchanged
    r.declare("a", "2")
    r.commit() shouldBe true // text changed
  }

  "table-of-contents entries collect in document order and survive a commit" in {
    val r = new ReferenceTable
    r.recordToc(1, "1", "Intro", 1)
    r.recordToc(2, "1.1", "Background", 2)
    r.toc shouldBe Vector.empty // not yet committed
    r.commit()
    r.toc shouldBe Vector(TocEntry(1, "1", "Intro", 1), TocEntry(2, "1.1", "Background", 2))
  }

  "named lists collect independently and survive a commit" in {
    val r = new ReferenceTable
    r.recordToc(1, "1", "Intro", 1)
    r.recordList("lof", 1, "1", "A frog", 2)
    r.recordList("lot", 1, "1", "Results", 3)
    r.recordList("lof", 1, "2", "A toad", 4)
    r.commit()
    r.list("toc") shouldBe Vector(TocEntry(1, "1", "Intro", 1))
    r.list("lof") shouldBe Vector(TocEntry(1, "1", "A frog", 2), TocEntry(1, "2", "A toad", 4))
    r.list("lot") shouldBe Vector(TocEntry(1, "1", "Results", 3))
  }

  "an unseen list is empty" in {
    val r = new ReferenceTable
    r.list("lof") shouldBe Vector.empty
  }

  "commit reports a change in a non-toc list" in {
    val r = new ReferenceTable
    r.recordList("lof", 1, "1", "A frog", 2)
    r.commit() shouldBe true
    r.recordList("lof", 1, "1", "A frog", 2)
    r.commit() shouldBe false
    r.recordList("lof", 1, "1", "A toad", 2)
    r.commit() shouldBe true
  }

  // ---- primitive wiring ------------------------------------------------------

  private def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    t.document = new io.github.edadma.texish.DocumentMode(t)
    (t, proc)

  private def quietly[A](body: => A): A =
    Console.withOut(new java.io.ByteArrayOutputStream)(body)

  private def textOf(t: HeadlessTypesetter, key: String): String = t.get(key) match
    case Some(Value.Text(s)) => s
    case Some(v)             => Value.display(v)
    case None                => "<unset>"

  "\\label captures the current label text" in {
    val (t, proc) = fixture()
    proc.process("\\set currentlabel {3.1}\\label{sec:intro}")

    t.references.refText("sec:intro") shouldBe Some("3.1")
    t.mode.asInstanceOf[Builder].list.last shouldBe a[LabelBox]
  }

  "\\ref prints the labelled text and composes in an expression" in {
    val (t, proc) = fixture()
    proc.process("\\set currentlabel {3.1}\\label{sec:intro}\\set captured {\\ref{sec:intro}}")

    textOf(t, "captured") shouldBe "3.1"
  }

  "\\ref to an unknown label prints the placeholder" in {
    val (t, proc) = fixture()
    proc.process("\\set captured {\\ref{ghost}}")

    textOf(t, "captured") shouldBe "??"
  }

  "\\eqref wraps the labelled text in parentheses" in {
    val (t, proc) = fixture()
    proc.process("\\set currentlabel {3.2}\\label{eq:euler}\\set captured {\\eqref{eq:euler}}")

    textOf(t, "captured") shouldBe "(3.2)"
  }

  "\\eqref to an unknown label prints (??)" in {
    val (t, proc) = fixture()
    proc.process("\\set captured {\\eqref{ghost}}")

    textOf(t, "captured") shouldBe "(??)"
  }

  "\\autoref prefixes the kind word captured at \\label" in {
    val (t, proc) = fixture()
    proc.process(
      "\\set currentlabel {3.2}\\set currentlabeltype {Section}\\label{sec:m}\\set captured {\\autoref{sec:m}}",
    )

    textOf(t, "captured") shouldBe "Section 3.2"
  }

  "\\autoref falls back to a bare number when no kind was set" in {
    val (t, proc) = fixture()
    proc.process("\\set currentlabel {3.2}\\label{sec:m}\\set captured {\\autoref{sec:m}}")

    textOf(t, "captured") shouldBe "3.2"
  }

  "\\nameref prints the title captured at \\label" in {
    val (t, proc) = fixture()
    proc.process(
      "\\set currentlabel {3.2}\\set currentlabelname {Methods}\\label{sec:m}\\set captured {\\nameref{sec:m}}",
    )

    textOf(t, "captured") shouldBe "Methods"
  }

  "\\nameref to an unknown label prints the placeholder" in {
    val (t, proc) = fixture()
    proc.process("\\set captured {\\nameref{ghost}}")

    textOf(t, "captured") shouldBe "??"
  }

  "a label inside a paragraph migrates out of its line, like a mark" in {
    val (t, proc) = fixture()
    t.set("hsize", 100.0)
    proc.process("\\set currentlabel {1}alpha \\label{here} beta gamma delta epsilon zeta eta theta iota")
    t.paragraph()

    val items = t.mode.asInstanceOf[Builder].list
    items.count(_.isInstanceOf[LabelBox]) shouldBe 1
    for line <- items.collect { case h: HBox => h } do
      line.boxes.exists(_.isInstanceOf[LabelBox]) shouldBe false
  }

  "\\tocentry records a contents entry as a migrating box in the toc list" in {
    val (t, proc) = fixture()
    proc.process("\\tocentry{2}{1.3}{Methods}")

    val last = t.mode.asInstanceOf[Builder].list.last
    last shouldBe a[TocMarkBox]
    val e = last.asInstanceOf[TocMarkBox]
    (e.list, e.level, e.number, e.title) shouldBe ("toc", 2, "1.3", "Methods")
  }

  "\\addcontentsline records an entry into the named list" in {
    val (t, proc) = fixture()
    proc.process("\\addcontentsline{lof}{1}{2}{A frog}")

    val last = t.mode.asInstanceOf[Builder].list.last.asInstanceOf[TocMarkBox]
    (last.list, last.level, last.number, last.title) shouldBe ("lof", 1, "2", "A frog")
  }

  "\\listoffigures replays the lof list through \\lofformat, in order" in {
    val (t, proc) = fixture()
    t.references.recordList("lof", 1, "1", "A frog", 2)
    t.references.recordList("lof", 1, "2", "A toad", 5)
    t.references.commit()

    proc.process(
      "\\def lofformat a b c d {\\set rec {\\cat{\\rec}{(\\b,\\c,\\d)}}}\\set rec {}\\listoffigures",
    )

    textOf(t, "rec") shouldBe "(1,A frog,2)(2,A toad,5)"
  }

  "\\listoftables reads its own list, independent of the toc and lof" in {
    val (t, proc) = fixture()
    t.references.recordToc(1, "1", "Intro", 1)
    t.references.recordList("lof", 1, "1", "A frog", 2)
    t.references.recordList("lot", 1, "1", "Results", 3)
    t.references.commit()

    proc.process("\\def lotformat a b c d {\\set rec {\\cat{\\rec}{(\\c,\\d)}}}\\set rec {}\\listoftables")

    textOf(t, "rec") shouldBe "(Results,3)"
  }

  "\\tableofcontents replays the resolved entries through \\tocformat, in order" in {
    val (t, proc) = fixture()
    t.references.recordToc(1, "1", "Intro", 3)
    t.references.recordToc(2, "1.1", "Background", 4)
    t.references.commit() // promote the collection so \tableofcontents reads it

    proc.process(
      "\\def tocformat a b c d {\\set rec {\\cat{\\rec}{(\\a,\\b,\\c,\\d)}}}\\set rec {}\\tableofcontents",
    )

    textOf(t, "rec") shouldBe "(1,1,Intro,3)(2,1.1,Background,4)"
  }

  "\\tableofcontents emits nothing when no entries have been collected yet" in {
    val (t, proc) = fixture()
    proc.process("\\def tocformat a b c d {x}\\set rec {clean}\\tableofcontents")

    textOf(t, "rec") shouldBe "clean"
  }

  "a label and a contents entry buried inside a float learn the float's page at shipout" in quietly {
    val (t, proc) = fixture()
    proc.process(
      "\\set currentlabel {1}\\topinsert{\\label{fig:in} \\addcontentsline{lof}{1}{1}{A frog}}\\vfill\\eject",
    )
    t.end()

    // the \label rode a \centerline-free vbox inside the FloatBox; PageMode must recurse into the float to find it
    t.references.refPage("fig:in") shouldBe Some(1)
    t.references.commit() // promote the pending lof list so list() can read it
    t.references.list("lof") shouldBe Vector(TocEntry(1, "1", "A frog", 1))
  }

  "\\pageref reports the folio of the page a label ships on" in quietly {
    val (t, proc) = fixture()
    proc.process(
      "\\set currentlabel {1}\\label{first} one\n\n\\vfill\\eject \\set currentlabel {2}\\label{second} two\n\n\\vfill\\eject three\n\n",
    )
    t.end()

    t.references.refPage("first") shouldBe Some(1)
    t.references.refPage("second") shouldBe Some(2)
  }
