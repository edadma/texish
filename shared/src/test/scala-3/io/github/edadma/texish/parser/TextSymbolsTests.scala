package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, CharBox, HBox, HeadlessTypesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream

/** The common LaTeX text symbols are engine primitives (the literal-character block in Processor), so they are
  * available to every document, not only those that load the article-class `document` package. These render each one
  * with no package at all and confirm a `\def` of the same name still overrides the primitive — the rule the math
  * package relies on for its own `\def dots {\ldots}`.
  */
class TextSymbolsTests extends AnyFreeSpec with Matchers:

  private def chars(b: Box): List[String] = b match
    case c: CharBox => List(c.text)
    case h: HBox    => h.boxes.toList.flatMap(chars)
    case v: VBox    => v.boxes.toList.flatMap(chars)
    case _          => Nil

  private def text(src: String): String =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new ByteArrayOutputStream) {
      proc.process(src)
      t.paragraph()
    }
    t.mode.asInstanceOf[Builder].list.flatMap(chars).mkString

  "every text symbol emits its Unicode character with no package loaded" in {
    text("\\dots") shouldBe "…"
    text("\\ldots") shouldBe "…"
    text("\\S") shouldBe "§"
    text("\\P") shouldBe "¶"
    text("\\dag") shouldBe "†"
    text("\\ddag") shouldBe "‡"
    text("\\copyright") shouldBe "©"
    text("\\textregistered") shouldBe "®"
    text("\\texttrademark") shouldBe "™"
    text("\\pounds") shouldBe "£"
    text("\\textbullet") shouldBe "•"
    text("\\textemdash") shouldBe "—"
    text("\\textendash") shouldBe "–"
    text("\\textdegree") shouldBe "°"
  }

  "a \\def of the same name overrides the symbol primitive" in {
    text("\\def dag {X}\\dag") shouldBe "X"
  }
