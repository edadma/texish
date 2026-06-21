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

  "\\tocentry records a contents entry as a migrating box" in {
    val (t, proc) = fixture()
    proc.process("\\tocentry{2}{1.3}{Methods}")

    val last = t.mode.asInstanceOf[Builder].list.last
    last shouldBe a[TocMarkBox]
    val e = last.asInstanceOf[TocMarkBox]
    (e.level, e.number, e.title) shouldBe (2, "1.3", "Methods")
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

  "\\pageref reports the folio of the page a label ships on" in quietly {
    val (t, proc) = fixture()
    proc.process(
      "\\set currentlabel {1}\\label{first} one\n\n\\vfill\\eject \\set currentlabel {2}\\label{second} two\n\n\\vfill\\eject three\n\n",
    )
    t.end()

    t.references.refPage("first") shouldBe Some(1)
    t.references.refPage("second") shouldBe Some(2)
  }
