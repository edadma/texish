package io.github.edadma.texish.parser

import io.github.edadma.texish.{Glue, StubTypesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The parser and the typesetter share one variable scope: anything either side sets, the other reads back unchanged.
  */
class ScopeUnificationTests extends AnyFreeSpec with Matchers:

  def fixture(): (StubTypesetter, TypesetterHandler, Processor) =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, handler, proc)

  "variables set by the language are visible to the typesetter" in {
    val (t, _, proc) = fixture()

    proc.process("\\set x {42}")
    t.getNumber("x") shouldBe 42.0

    proc.process("\\set name {Lazarus}")
    t.getVar("name") shouldBe Value.Text("Lazarus")
  }

  "variables set by the typesetter are visible to the language" in {
    val (t, handler, _) = fixture()

    t.set("chapter", 5)
    handler.get("chapter") shouldBe Value.Num(5)

    t.set("book", "Genesis")
    handler.get("book") shouldBe Value.Text("Genesis")
  }

  "numbers survive the round trip unchanged" in {
    val (t, handler, proc) = fixture()

    proc.process("\\set pi {3.14159}")
    t.getVar("pi") shouldBe Value.Num(3.14159)
    handler.get("pi") shouldBe Value.Num(3.14159)
  }

  "engine objects live in the same scope the parser reads" in {
    val (t, handler, _) = fixture()

    // baselineskip is set by the engine as native Glue
    val glue = t.getGlue("baselineskip")
    handler.get("baselineskip") shouldBe Value.Native(glue)
  }

  "engine parameters are readable as language values" in {
    val (t, handler, _) = fixture()

    handler.get("parindent") shouldBe Value.Num(t.getNumber("parindent"))
    handler.get("hsize") shouldBe Value.Num(t.getNumber("hsize"))
  }

  "undefined variables are Undefined to the language, errors to the engine" in {
    val (t, handler, _) = fixture()

    handler.get("nonexistent") shouldBe Value.Undefined
    an[Exception] should be thrownBy t.getVar("nonexistent")
  }

  "a local set does not survive its scope's exit, but a global one does" in {
    val (t, _, proc) = fixture()

    t.enter()
    proc.process("\\set inner {1}")
    t.exit()

    // a plain \set is local to the group it ran in, so it is gone once that scope closes
    t.get("inner") shouldBe None

    t.enter()
    proc.process("\\global\\set persists {2}")
    t.exit()

    // \global escapes the scope, so its value remains at the outermost scope
    t.getNumber("persists") shouldBe 2.0
  }
