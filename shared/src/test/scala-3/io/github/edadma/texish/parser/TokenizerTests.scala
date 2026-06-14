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

    "should skip comments" in {
      val tok = Tokenizer("hello%comment\nworld")
      tok.next().asInstanceOf[Token.Text].s shouldBe "hello"
      tok.next() shouldBe a[Token.Text] // "world"
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
