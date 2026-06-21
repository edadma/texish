package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, CharBox, CodeHighlight, Color, HBox, HeadlessTypesetter, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Syntax-highlighted code listings: `\code[lang]{…}`, the `code` environment, and the CodeHighlight bridge that
  * turns a language's tokens into coloured runs. Listings are set in the JetBrains Mono face; the default theme is
  * a light one (its ink reads on a white page).
  */
class CodeTests extends AnyFreeSpec with Matchers:

  private class Capture extends HeadlessTypesetter:
    val added = ArrayBuffer[Box]()
    override infix def add(box: Box): Typesetter =
      added += box
      super.add(box)

  private def fixture(): (Capture, Processor) =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    (t, proc)

  private def lineBoxes(t: Capture): Seq[HBox] = t.added.collect { case h: HBox => h }.toSeq

  "the CodeHighlight bridge" - {
    "bundles a starter set of grammars" in {
      CodeHighlight.availableLanguages should contain allOf ("scala", "python", "json", "bash", "texish")
    }

    "tokenizes a known language into coloured runs, keyword distinct from default" in {
      val Right(hl) = CodeHighlight.forLanguage("scala"): @unchecked
      val lines     = CodeHighlight.styledLines(hl, io.github.edadma.highlighter.Theme.OneLight, "val x = 42")
      lines.length shouldBe 1
      val texts = lines.head.map(_._1).mkString
      texts shouldBe "val x = 42" // run texts reproduce the source line
      val colours = lines.head.filter(_._1.trim.nonEmpty).map(_._2).distinct
      colours.length should be > 1 // not every token is the default colour
    }

    "reports an unknown language rather than guessing" in {
      CodeHighlight.forLanguage("no-such-lang").isLeft shouldBe true
    }
  }

  "tokenizer readOptionalRawBracket" - {
    "reads a [bracketed] argument and leaves the reader at the brace body" in {
      val tok = Tokenizer("[scala]{code}")
      tok.readOptionalRawBracket() shouldBe Some("scala")
      tok.readRawGroup() shouldBe Some("code")
    }

    "returns None when there is no opening bracket" in {
      Tokenizer("{code}").readOptionalRawBracket() shouldBe None
    }
  }

  "\\code without a language" - {
    "sets one plain line box per source line in the code face" in {
      val (t, proc) = fixture()
      proc.process("\\code{plain text}")
      val line = lineBoxes(t).head
      line.boxes.length shouldBe 1
      val cb = line.boxes.head.asInstanceOf[CharBox]
      cb.text shouldBe "plain text"
      cb.font.typeface shouldBe "jetbrains"
    }

    "keeps // literal in the body (read raw)" in {
      val (t, proc) = fixture()
      proc.process("\\code{a // b}")
      lineBoxes(t).head.boxes.head.asInstanceOf[CharBox].text shouldBe "a // b"
    }
  }

  "\\code with a language" - {
    "highlights, splitting the line into several coloured runs" in {
      val (t, proc) = fixture()
      proc.process("\\code[scala]{val x = 42}")
      val line = lineBoxes(t).head
      line.boxes.length should be > 1 // highlighting split the line into runs
      line.boxes.head.asInstanceOf[CharBox].font.typeface shouldBe "jetbrains"
    }

    "colours the keyword with the theme's keyword colour" in {
      val (t, proc) = fixture()
      proc.process("\\code[scala]{val x = 42}")
      val cbs = lineBoxes(t).head.boxes.map(_.asInstanceOf[CharBox])
      val valBox = cbs.find(_.text == "val").getOrElse(fail("no 'val' run"))
      valBox.color shouldBe Color("#a626a4") // OneLight keyword
    }

    "reports an unknown language as an error" in {
      val (_, proc) = fixture()
      a[ParserException] should be thrownBy proc.process("\\code[nope]{x}")
    }
  }

  "the code environment" - {
    "highlights using the codelang variable" in {
      val (t, proc) = fixture()
      proc.process("\\set codelang {scala}\n\\begin{code}\nval n = 1\n\\end{code}")
      lineBoxes(t).head.boxes.length should be > 1
    }

    "is plain when codelang is unset" in {
      val (t, proc) = fixture()
      proc.process("\\begin{code}\njust text\n\\end{code}")
      lineBoxes(t).head.boxes.length shouldBe 1
    }
  }
