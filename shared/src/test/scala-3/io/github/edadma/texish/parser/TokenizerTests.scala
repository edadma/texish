package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TokenizerTests extends AnyFreeSpec with Matchers:

  "Tokenizer" - {
    "should tokenize plain text" in {
      val tok = Tokenizer("hello world")
      tok.next() shouldBe a[Token.Text]
      tok.next() shouldBe a[Token.Space]
      tok.next() shouldBe a[Token.Text]
      tok.next() shouldBe a[Token.EOF]
    }

    "should tokenize control sequences" in {
      val tok = Tokenizer("\\foo\\bar")
      val t1 = tok.next()
      t1 shouldBe a[Token.ControlSeq]
      t1.asInstanceOf[Token.ControlSeq].name shouldBe "foo"

      val t2 = tok.next()
      t2 shouldBe a[Token.ControlSeq]
      t2.asInstanceOf[Token.ControlSeq].name shouldBe "bar"
    }

    "should tokenize single-char control sequences" in {
      val tok = Tokenizer("\\{\\}")
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe "{"
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe "}"
    }

    "reads the comparison operators as one symbolic control sequence" in {
      def name(s: String): String = Tokenizer(s).next().asInstanceOf[Token.ControlSeq].name
      name("\\!=") shouldBe "!="
      name("\\<=") shouldBe "<="
      name("\\>=") shouldBe ">="
      name("\\<") shouldBe "<"  // a bare comparison char, with no '=' following, stays one char
    }

    "never lets a comma join a symbolic control sequence" in {
      // `\,` is the lone thin-space control symbol; the comma must not absorb a following symbolic character
      // (so `\,(` is `\,` then `(`), and a comma after another symbolic char ends the control sequence.
      val tok = Tokenizer("\\,(")
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe ","
      tok.next().asInstanceOf[Token.Text].s shouldBe "("

      Tokenizer("\\,").next().asInstanceOf[Token.ControlSeq].name shouldBe ","

      val pair = Tokenizer("\\;,") // a comma ends a run begun by another symbolic char
      pair.next().asInstanceOf[Token.ControlSeq].name shouldBe ";"
      pair.next().asInstanceOf[Token.Text].s shouldBe ","
    }

    "a math-space control symbol does not absorb following punctuation" in {
      // symbolic control sequences are one character (bar the three comparison operators), so `\;(x)` is the
      // thick math space applied to `(x)`, not an unknown command named `;(`
      val tok = Tokenizer("\\;(x)")
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe ";"
      tok.next().asInstanceOf[Token.Text].s shouldBe "(x)"

      val neg = Tokenizer("\\!(y)") // `!` joins only a following '='
      neg.next().asInstanceOf[Token.ControlSeq].name shouldBe "!"
      neg.next().asInstanceOf[Token.Text].s shouldBe "(y)"
    }

    "a CRLF line ending is a single Newline token" in {
      val tok = Tokenizer("a\r\nb")
      tok.next().asInstanceOf[Token.Text].s shouldBe "a"
      tok.next() shouldBe a[Token.Newline]
      tok.next().asInstanceOf[Token.Text].s shouldBe "b"
      tok.next() shouldBe a[Token.EOF]
    }

    "should tokenize groups" in {
      val tok = Tokenizer("{hello}")
      tok.next() shouldBe a[Token.BeginGroup]
      tok.next() shouldBe a[Token.Text]
      tok.next() shouldBe a[Token.EndGroup]
    }

    "should treat # as normal text" in {
      val tok = Tokenizer("#1#2")
      val t = tok.next()
      t shouldBe a[Token.Text]
      t.asInstanceOf[Token.Text].s shouldBe "#1#2"
    }

    "should skip // comments to end of line" in {
      val tok = Tokenizer("hello//comment\nworld")
      tok.next().asInstanceOf[Token.Text].s shouldBe "hello"
      tok.next() shouldBe a[Token.Text] // "world"
    }

    "treats % as ordinary text and a single / as text" in {
      Tokenizer("50%").next().asInstanceOf[Token.Text].s shouldBe "50%"
      Tokenizer("a/b").next().asInstanceOf[Token.Text].s shouldBe "a/b"
    }

    "should tokenize newlines" in {
      val tok = Tokenizer("a\nb")
      tok.next() shouldBe a[Token.Text]
      tok.next() shouldBe a[Token.Newline]
      tok.next() shouldBe a[Token.Text]
    }

    "should tokenize active characters" in {
      val tok = Tokenizer("a~b")
      tok.next() shouldBe a[Token.Text]
      tok.next() shouldBe a[Token.Active]
      tok.next() shouldBe a[Token.Text]
    }

    "should tokenize multi-char symbolic control sequences" in {
      val tok = Tokenizer("\\<=\\>=\\!=")
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe "<="
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe ">="
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe "!="
    }

    "should tokenize single-char symbolic control sequences" in {
      val tok = Tokenizer("\\+\\-\\*\\/")
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe "+"
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe "-"
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe "*"
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe "/"
    }

    "should terminate symbolic sequences at braces" in {
      val tok = Tokenizer("\\<={5}")
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe "<="
      tok.next() shouldBe a[Token.BeginGroup]
      tok.next().asInstanceOf[Token.Text].s shouldBe "5"
      tok.next() shouldBe a[Token.EndGroup]
    }

    "should terminate symbolic sequences at letters" in {
      val tok = Tokenizer("\\<=abc")
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe "<="
      tok.next().asInstanceOf[Token.Text].s shouldBe "abc"
    }

    "should terminate symbolic sequences at backslash" in {
      val tok = Tokenizer("\\<=\\>")
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe "<="
      tok.next().asInstanceOf[Token.ControlSeq].name shouldBe ">"
    }
  }
