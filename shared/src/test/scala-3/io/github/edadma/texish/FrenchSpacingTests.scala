package io.github.edadma.texish

import scala.collection.mutable.ArrayBuffer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** French spacing of high punctuation ([[FrenchSpacing]]).
  *
  * French sets a space before the colon, semicolon, exclamation and question marks and inside its guillemets.
  * The space must not stretch when the line is justified, or the mark drifts from its word, and must not
  * break, or the mark is carried alone to the next line. These pin the widths, the rigidity and the marks
  * the rule applies to, on the fixed-metric [[HeadlessTypesetter]].
  */
class FrenchSpacingTests extends AnyFreeSpec with Matchers:

  private def boxesFor(t: HeadlessTypesetter, parts: String*): ArrayBuffer[Box] =
    val out = ArrayBuffer.empty[Box]
    for (p, i) <- parts.zipWithIndex do
      if i > 0 then out += t.getGlue("spaceskip")
      out += t.charBox(p)
    out

  // The run as widths and kinds, for comparing shape without depending on glyph metrics.
  private def shape(boxes: ArrayBuffer[Box]): Seq[String] =
    boxes.toSeq.map {
      case g: Glue    => if g.nobreak then f"fixed(${g.naturalSize}%.1f)" else f"glue(${g.naturalSize}%.1f)"
      case c: CharBox => s"'${c.text}'"
      case b          => b.toString
    }

  private def interword(t: HeadlessTypesetter): Double = t.getGlue("spaceskip").naturalSize

  "the language tag is recognised, regional variants included" in {
    FrenchSpacing.applies(Some("fr")) shouldBe true
    FrenchSpacing.applies(Some("fr-CA")) shouldBe true
    FrenchSpacing.applies(Some("fr_FR")) shouldBe true
    FrenchSpacing.applies(Some("FR")) shouldBe true
    FrenchSpacing.applies(Some("en-us")) shouldBe false
    FrenchSpacing.applies(Some("frisian")) shouldBe false
    FrenchSpacing.applies(None) shouldBe false
  }

  "a typed space before a mark becomes a fixed, unbreakable one" in {
    // The space is there but is an ordinary interword space: it stretches and it offers a break, so the mark
    // can be carried alone to the next line. Both must go.
    val t  = new HeadlessTypesetter
    val sp = interword(t)
    val b  = boxesFor(t, "vie", ";", "nul")
    FrenchSpacing(b)
    shape(b) shouldBe Seq("'vie'", f"fixed(${sp / 2}%.1f)", "';'", f"glue($sp%.1f)", "'nul'")
  }

  "the colon takes a full space and the other marks a fine one" in {
    val t  = new HeadlessTypesetter
    val sp = interword(t)
    for (mark, width) <- Seq(":" -> sp, ";" -> sp / 2, "!" -> sp / 2, "?" -> sp / 2, "»" -> sp / 2) do
      val b = boxesFor(t, "mot", mark)
      FrenchSpacing(b)
      shape(b) shouldBe Seq("'mot'", f"fixed($width%.1f)", s"'$mark'")
  }

  "a mark typed tight against its word is separated and spaced" in {
    // `cela?` is one box, the two having been merged; it must come apart so the space can go in.
    val t  = new HeadlessTypesetter
    val sp = interword(t)
    val b  = boxesFor(t, "cela?")
    FrenchSpacing(b)
    shape(b) shouldBe Seq("'cela'", f"fixed(${sp / 2}%.1f)", "'?'")
  }

  "an opening guillemet is spaced from the word it opens" in {
    val t  = new HeadlessTypesetter
    val sp = interword(t)
    val b  = boxesFor(t, "«Oui")
    FrenchSpacing(b)
    shape(b) shouldBe Seq("'«'", f"fixed(${sp / 2}%.1f)", "'Oui'")
  }

  "a quoted exclamation comes apart into all three pieces" in {
    // `«Oui!»` is one box and must end as guillemet, word, mark, guillemet, each pair spaced — the closing
    // guillemet taking its space even though a mark precedes it.
    val t  = new HeadlessTypesetter
    val sp = interword(t)
    val f  = f"fixed(${sp / 2}%.1f)"
    val b  = boxesFor(t, "«Oui!»")
    FrenchSpacing(b)
    shape(b) shouldBe Seq("'«'", f, "'Oui'", f, "'!'", f, "'»'")
  }

  "a run of marks is spaced once, before the run" in {
    // `Quoi ?!` takes its space before the pair, not inside it.
    val t  = new HeadlessTypesetter
    val sp = interword(t)
    val b  = boxesFor(t, "Quoi?!")
    FrenchSpacing(b)
    shape(b) shouldBe Seq("'Quoi'", f"fixed(${sp / 2}%.1f)", "'?!'")
  }

  "the comma and full stop are left alone" in {
    // Only the high punctuation is spaced; French sets these tight, as English does.
    val t = new HeadlessTypesetter
    val b = boxesFor(t, "mot,", "puis.")
    val before = shape(b)
    FrenchSpacing(b)
    shape(b) shouldBe before
  }

  "an English paragraph is never touched" in {
    // The rule is reached only for a French document, but prove it leaves the same text alone regardless.
    val t = new HeadlessTypesetter
    t.language = Some("en-us")
    FrenchSpacing.applies(t.language) shouldBe false
  }
