package io.github.edadma.typesetter.texish

import io.github.edadma.typesetter.{Builder, Font, Glue, StubTypesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Glue syntax in the language layer: `<dimen> [plus <flex>] [minus <flex>]` where a flex part is a dimension or an
  * infinite amount (`1fil`, `2fill`). Stretch and shrink carry independent infinity orders, so
  * `12pt plus 2pt minus 1fil` — finite stretch, infinite shrink — round-trips exactly.
  */
class GlueSyntaxTests extends AnyFreeSpec with Matchers:

  def fixture(): (StubTypesetter, Processor) =
    val t       = new StubTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  def lastGlue(t: StubTypesetter): Glue =
    t.mode.asInstanceOf[Builder].last.asInstanceOf[Glue]

  "parseGlue" - {
    "finite glue" in {
      parseGlue("12pt plus 2pt minus 1pt") shouldBe Some(Value.Glue(12, 2, 1))
    }

    "stretch only" in {
      parseGlue("0pt plus 1fil") shouldBe Some(Value.Glue(0, 1, 0, 1, 0))
    }

    "shrink only" in {
      parseGlue("6pt minus 2fill") shouldBe Some(Value.Glue(6, 0, 2, 0, 2))
    }

    "mixed infinity orders are independent" in {
      parseGlue("12pt plus 2pt minus 1fil") shouldBe Some(Value.Glue(12, 2, 1, 0, 1))
    }

    "flex parts convert units like the natural part" in {
      parseGlue("1in plus 2pc minus 1pc") shouldBe Some(Value.Glue(72, 24, 12))
    }

    "a bare dimension is Dimen, not Glue" in {
      parseGlue("12pt") shouldBe Some(Value.Dimen(12))
    }

    "a malformed flex part does not parse" in {
      parseGlue("12pt plus bogus") shouldBe None
    }

    "plain text does not parse" in {
      parseGlue("hello world") shouldBe None
    }
  }

  "display round-trips through parseGlue" in {
    val specs = Vector(
      Value.Glue(12, 2, 1),
      Value.Glue(0, 1, 0, 1, 0),
      Value.Glue(12, 2, 1, 0, 1),
      Value.Glue(6.5, 0, 2, 0, 2),
    )
    for v <- specs do parseGlue(Value.display(v)) shouldBe Some(v)
    Value.display(Value.Glue(12, 2, 0)) shouldBe "12pt plus 2pt"
    Value.display(Value.Glue(12, 2, 1, 0, 1)) shouldBe "12pt plus 2pt minus 1fil"
  }

  "\\set with a braced glue spec stores Value.Glue and the engine reads it back" in {
    val (t, proc) = fixture()
    proc.process("\\set baselineskip {12pt plus 2pt minus 1pt}")
    t.getVar("baselineskip") shouldBe Value.Glue(12, 2, 1)
    val g = t.getGlue("baselineskip")
    g.naturalSize shouldBe 12.0
    g.stretch shouldBe 2.0
    g.shrink shouldBe 1.0
    g.stretchOrder shouldBe 0
    g.shrinkOrder shouldBe 0
  }

  "getGlue treats a plain dimension as rigid glue" in {
    val (t, proc) = fixture()
    proc.process("\\set lineskip {14pt}")
    val g = t.getGlue("lineskip")
    g.naturalSize shouldBe 14.0
    g.stretch shouldBe 0.0
    g.shrink shouldBe 0.0
  }

  "\\vskip with a braced glue spec adds the glue" in {
    val (t, proc) = fixture()
    proc.process("\\vskip {6pt plus 3pt minus 2pt}")
    val g = lastGlue(t)
    g.naturalSize shouldBe 6.0
    g.stretch shouldBe 3.0
    g.shrink shouldBe 2.0
  }

  "\\vskip with an unbraced TeX-style continuation" in {
    val (t, proc) = fixture()
    proc.process("\\vskip 12pt plus 2pt minus 1fil")
    val g = lastGlue(t)
    g.naturalSize shouldBe 12.0
    g.stretch shouldBe 2.0
    g.stretchOrder shouldBe 0
    g.shrink shouldBe 1.0
    g.shrinkOrder shouldBe 1
  }

  "\\hskip with a stretch-only continuation" in {
    val (t, proc) = fixture()
    proc.process("\\hskip 4pt plus 1fill")
    val g = lastGlue(t)
    g.naturalSize shouldBe 4.0
    g.stretch shouldBe 1.0
    g.stretchOrder shouldBe 2
    g.shrink shouldBe 0.0
  }

  "\\vskip with a bare dimension stays rigid" in {
    val (t, proc) = fixture()
    proc.process("\\vskip 12pt")
    val g = lastGlue(t)
    g.naturalSize shouldBe 12.0
    g.stretch shouldBe 0.0
    g.shrink shouldBe 0.0
  }

  "a non-keyword word after the dimension is typeset, not consumed" in {
    val (t, proc) = fixture()
    // "plush" is not the keyword "plus": the scan must leave it (and the space before it) for normal processing
    proc.process("\\vskip {12pt} plush")
    t.mode.asInstanceOf[Builder].last should not be a[Glue]
  }

  "\\vskip accepts a glue-valued variable" in {
    val (t, proc) = fixture()
    proc.process("\\set myskip {3pt plus 1fil}")
    proc.process("\\vskip \\myskip")
    val g = lastGlue(t)
    g.naturalSize shouldBe 3.0
    g.stretch shouldBe 1.0
    g.stretchOrder shouldBe 1
  }

  "font-relative units work in glue specs" in {
    val (t, proc) = fixture()
    t.currentFont = new Font("stub", 10, 6, 4.5, Set.empty, "stub", None, Set.empty)
    proc.process("\\set s {1em plus 0.5em minus 1ex}")
    t.getVar("s") shouldBe Value.Glue(10, 5, 4.5)
  }

  "font-relative units work in unbraced continuations" in {
    val (t, proc) = fixture()
    t.currentFont = new Font("stub", 10, 6, 4.5, Set.empty, "stub", None, Set.empty)
    proc.process("\\vskip 1em plus 1ex")
    val g = lastGlue(t)
    g.naturalSize shouldBe 10.0
    g.stretch shouldBe 4.5
    g.stretchOrder shouldBe 0
  }

  "\\vskip accepts an engine glue stored by the host" in {
    val (t, proc) = fixture()
    t.set("hostskip", Glue(5, 1, 0, 1, 0))
    proc.process("\\vskip \\hostskip")
    val g = lastGlue(t)
    g.naturalSize shouldBe 5.0
    g.stretchOrder shouldBe 1
  }
