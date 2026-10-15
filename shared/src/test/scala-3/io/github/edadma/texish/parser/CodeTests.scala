package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.path.Path
import io.github.edadma.texish.{Box, CharBox, CodeHighlight, Color, HBox, HeadlessTypesetter, TexishException, Typesetter}
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
      a[TexishException] should be thrownBy proc.process("\\code[nope]{x}")
    }
  }

  "sysl" - {
    "is bundled" in {
      CodeHighlight.availableLanguages should contain("sysl")
    }

    "colours a declaration keyword with the theme's keyword colour" in {
      val (t, proc) = fixture()
      proc.process("\\code[sysl]{var width = 6}")
      val cbs = lineBoxes(t).head.boxes.map(_.asInstanceOf[CharBox])
      cbs.find(_.text == "var").getOrElse(fail("no 'var' run")).color shouldBe Color("#a626a4")
    }
  }

  /** A grammar that colours only the word `zap`, as a keyword: enough to tell it apart from any bundled grammar. */
  private val zapGrammar =
    """{"scopeName": "source.zap", "patterns": [{"match": "\\bzap\\b", "name": "keyword.control.zap"}]}"""

  /** A fixture whose document directory is a fresh temp directory holding the given (name -> content) files. */
  private def withGrammars(files: (String, String)*)(body: (Capture, Processor) => Unit): Unit =
    val dir     = Path.createTempDirectory("texish-grammar-")
    val written = files.map((name, content) => { val p = dir / name; p.writeText(content); p })
    try
      val (t, proc) = fixture()
      proc.setBaseDir(dir.toPlatformString)
      body(t, proc)
    finally
      written.foreach(p => try p.delete() catch { case _: Exception => () })
      try dir.delete() catch { case _: Exception => () }

  private def colourOf(t: Capture, word: String): Color =
    lineBoxes(t).head.boxes.map(_.asInstanceOf[CharBox]).find(_.text == word).getOrElse(fail(s"no '$word' run")).color

  "a grammar file beside the document" - {
    "highlights a language texish does not bundle" in {
      withGrammars("zap.tmLanguage.json" -> zapGrammar) { (t, proc) =>
        proc.process("\\code[zap]{zap here}")
        colourOf(t, "zap") shouldBe Color("#a626a4")
      }
    }

    "is used by the code environment too" in {
      withGrammars("zap.tmLanguage.json" -> zapGrammar) { (t, proc) =>
        proc.process("\\set codelang {zap}\n\\begin{code}\nzap here\n\\end{code}")
        colourOf(t, "zap") shouldBe Color("#a626a4")
      }
    }

    "shadows the bundled grammar of the same name" in {
      withGrammars("scala.tmLanguage.json" -> zapGrammar) { (t, proc) =>
        proc.process("\\code[scala]{val zap}")
        colourOf(t, "zap") shouldBe Color("#a626a4")
        lineBoxes(t).head.boxes.map(_.asInstanceOf[CharBox]).exists(_.text.contains("val")) shouldBe true
        lineBoxes(t).head.boxes.map(_.asInstanceOf[CharBox]).filter(_.text.contains("val")).map(_.color) should
          not contain Color("#a626a4") // the bundled scala grammar would have made `val` a keyword
      }
    }

    "that cannot be compiled is an error naming the file" in {
      withGrammars("bad.tmLanguage.json" -> "{ not json") { (_, proc) =>
        val e = the[TexishException] thrownBy proc.process("\\code[bad]{x}")
        e.getMessage should include("bad.tmLanguage.json")
      }
    }
  }

  "an unknown language's error says where a grammar file would go" in {
    val (_, proc) = fixture()
    val e         = the[TexishException] thrownBy proc.process("\\code[nope]{x}")
    e.getMessage should include("nope.tmLanguage.json")
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
