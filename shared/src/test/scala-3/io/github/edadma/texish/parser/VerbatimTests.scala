package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, CharBox, HBox, HeadlessTypesetter, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Inline `\verb` and the `verbatim` environment. The HeadlessTypesetter gives every character width 6 and metrics
  * ascent 8 / descent 2, so a verbatim line of n characters (spaces included) is 6n wide — which is how the
  * leading-space and line-width checks below are read.
  */
class VerbatimTests extends AnyFreeSpec with Matchers:

  /** Records every box handed to the typesetter's top-level `add`, so the boxes a `\verb` or a verbatim block
    * places can be inspected. Interline glue inserted inside vertical mode is not a top-level add, so it does not
    * appear here — only the line boxes and inline boxes do. */
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

  "tokenizer raw seams" - {
    "readVerb captures up to the closing delimiter and consumes it" in {
      val tok = Tokenizer("|a_b//c|rest")
      tok.readVerb() shouldBe Some("a_b//c")
      tok.next() shouldBe a[Token.Text] // the closing | was consumed; "rest" is next
    }

    "readVerb returns None at end of input with no closing delimiter" in {
      Tokenizer("|unterminated").readVerb() shouldBe None
    }

    "readVerb refuses once a token has been peeked" in {
      val tok = Tokenizer("|ab|")
      tok.peek
      tok.readVerb() shouldBe None
    }

    "readRawUntilEnd captures the literal body and consumes the sentinel" in {
      val tok = Tokenizer("  x\n  y\n\\end{foo} after")
      tok.readRawUntilEnd("foo") shouldBe Some("  x\n  y\n")
      tok.next() shouldBe a[Token.Space] // reader sits just past }, on the space before "after"
    }

    "readRawUntilEnd tolerates spaces around the sentinel braces" in {
      Tokenizer("body\\end { foo }").readRawUntilEnd("foo") shouldBe Some("body")
    }

    "readRawUntilEnd returns None when the matching end is missing" in {
      Tokenizer("body with no end").readRawUntilEnd("foo") shouldBe None
    }
  }

  "\\verb" - {
    "sets its text literally in the typewriter face" in {
      val (t, proc) = fixture()
      proc.process("\\verb|a_b//c|")
      val box = t.added.last.asInstanceOf[CharBox]
      box.text shouldBe "a_b//c"
      box.font.style should contain("mono")
      box.width shouldBe (6.0 * 6) // six characters
    }

    "does not start a comment at // inside the verbatim text" in {
      val (t, proc) = fixture()
      proc.process("\\verb#http://x#")
      t.added.last.asInstanceOf[CharBox].text shouldBe "http://x"
    }

    "leaves the surrounding font unchanged afterwards" in {
      val (t, proc) = fixture()
      val before = t.currentFont
      proc.process("\\verb|x|")
      t.currentFont shouldBe before
    }

    "keeps the text that follows the closing delimiter on the same run" in {
      // the sentence-ending punctuation of "… through \verb|\textcolor|." must survive
      val (t, proc) = fixture()
      proc.process("a \\verb|x|. b\n")
      val texts = t.added.collect { case c: CharBox => c.text }
      texts should contain inOrder ("x", ".", " ", "b")
    }

    "keeps the interword space after the verbatim text" in {
      // "\verb|x| to" is two words; the space between them is real
      val (t, proc) = fixture()
      proc.process("a \\verb|x| b\n")
      val texts = t.added.collect { case c: CharBox => c.text }
      texts.mkString shouldBe "a x b"
    }

    "leaves the surrounding glue parameters untouched" in {
      // the mono switch refreshes \baselineskip; without its own scope that write survived the \verb and
      // silently replaced a document's leading
      val (t, proc) = fixture()
      proc.process("\\set baselineskip {14}\\verb|x| after")
      Value.number(proc.handler.get("baselineskip")) shouldBe Some(14.0)
    }
  }

  "verbatim environment" - {
    "sets one line box per source line, dropping the framing newlines" in {
      val (t, proc) = fixture()
      proc.process("\\begin{verbatim}\nalpha\nbeta\n\\end{verbatim}")
      val lines = t.added.collect { case h: HBox => h }
      lines.length shouldBe 2
    }

    "preserves leading spaces (each is a fixed-width character)" in {
      val (t, proc) = fixture()
      proc.process("\\begin{verbatim}\n    indented\n\\end{verbatim}")
      val line = t.added.collect { case h: HBox => h }.head
      line.width shouldBe (6.0 * 12) // four spaces + "indented" = 12 chars
    }

    "keeps interior blank lines as their own line boxes" in {
      val (t, proc) = fixture()
      proc.process("\\begin{verbatim}\na\n\nb\n\\end{verbatim}")
      t.added.collect { case h: HBox => h }.length shouldBe 3
    }

    "sets the lines in the typewriter face" in {
      val (t, proc) = fixture()
      proc.process("\\begin{verbatim}\ncode\n\\end{verbatim}")
      val cb = t.added.collect { case h: HBox => h }.head.boxes.head.asInstanceOf[CharBox]
      cb.font.style should contain("mono")
    }

    "restores the running font after the block" in {
      val (t, proc) = fixture()
      val before = t.currentFont
      proc.process("\\begin{verbatim}\ncode\n\\end{verbatim}")
      t.currentFont shouldBe before
    }
  }
