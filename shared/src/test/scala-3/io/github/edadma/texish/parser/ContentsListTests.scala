package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, HBox, HeadlessTypesetter, ReferenceTable}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `\contentslist{list}{format}` is the general form of `\tableofcontents`: it replays the entries collected for
  * any named list — the name passed to `\addcontentsline` — through a format macro of the caller's choosing,
  * invoked once per entry as `format{level}{number}{title}{page}`. This is what lets one document keep several
  * independent contents lists, such as a separate table of contents per language.
  */
class ContentsListTests extends AnyFreeSpec with Matchers:

  private def fixture(): (HeadlessTypesetter, Processor) =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  private def lines(t: HeadlessTypesetter): Seq[HBox] =
    t.mode.asInstanceOf[Builder].list.collect { case b: HBox => b }

  "replays each entry of a named list through the given format macro" in {
    val (t, proc) = fixture()
    t.references = new ReferenceTable
    t.references.recordList("frtoc", 1, "1", "Alpha", 4)
    t.references.recordList("frtoc", 1, "2", "Beta", 6)
    t.references.commit() // promote the pass's collection to the resolved side that \contentslist reads

    proc.process("\\def row lvl num ttl pg {\\hbox{\\num\\ttl\\pg}}\\contentslist{frtoc}{row}")

    lines(t).length shouldBe 2
  }

  "keeps two named lists independent" in {
    val (t, proc) = fixture()
    t.references = new ReferenceTable
    t.references.recordList("frtoc", 1, "1", "Alpha", 4)
    t.references.recordList("entoc", 1, "1", "Alpha", 5)
    t.references.recordList("entoc", 1, "2", "Beta", 7)
    t.references.commit()

    proc.process("\\def row lvl num ttl pg {\\hbox{\\num}}\\contentslist{frtoc}{row}")
    lines(t).length shouldBe 1
  }

  "emits nothing for a list that collected no entries (the first-pass case)" in {
    val (t, proc) = fixture()
    t.references = new ReferenceTable
    t.references.commit()

    proc.process("\\def row lvl num ttl pg {\\hbox{\\num}}\\contentslist{empty}{row}")
    lines(t).length shouldBe 0
  }
