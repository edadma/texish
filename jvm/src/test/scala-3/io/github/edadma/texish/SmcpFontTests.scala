package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{Gsub, OtfFont}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Synthesized small capitals through the OpenType `smcp` feature, checked against the bundled EB Garamond
  * face — a text roman that carries `smcp` but ships no separate small-caps file. The engine turns its
  * lowercase letters into small capitals itself (see the small-caps path in [[CharBox]]); this test exercises
  * the parsing and feature application that path rests on, reading the real font from disk. */
class SmcpFontTests extends AnyFreeSpec with Matchers:

  private val garamond =
    new OtfFont(Files.readAllBytes(Paths.get("fonts/EB_Garamond/static/EBGaramond-Regular.ttf")))

  private def smcp(font: OtfFont): Gsub =
    Gsub.fromSmallCaps(font.tableBytes("GSUB"), font.tableBytes("GDEF")).get

  private def sub(font: OtfFont, shaper: Gsub, cp: Int): Int =
    shaper.applyFeatureByTag(Array(font.glyphIndex(cp)), "smcp")(0)

  "EB Garamond is recognised as carrying the smcp feature" in {
    Gsub.fromSmallCaps(garamond.tableBytes("GSUB"), garamond.tableBytes("GDEF")) shouldBe defined
  }

  "a lowercase letter is substituted for a different glyph" in {
    val shaper = smcp(garamond)
    for cp <- 'a'.toInt to 'z'.toInt do
      sub(garamond, shaper, cp) should not equal garamond.glyphIndex(cp)
  }

  "the small-capital glyph is not the full-capital glyph" in {
    // Small caps are their own glyphs: the small-cap 'a' is neither the lowercase 'a' nor the full-height 'A'.
    val shaper = smcp(garamond)
    val smallA = sub(garamond, shaper, 'a'.toInt)
    smallA should not equal garamond.glyphIndex('a'.toInt)
    smallA should not equal garamond.glyphIndex('A'.toInt)
  }

  "an uppercase letter passes through smcp unchanged" in {
    // smcp maps lowercase to small caps; capitals are left full height, which is exactly small-caps behaviour.
    val shaper = smcp(garamond)
    for cp <- 'A'.toInt to 'Z'.toInt do
      sub(garamond, shaper, cp) shouldBe garamond.glyphIndex(cp)
  }

  "a face without the smcp feature is not mistaken for a small-caps synthesiser" in {
    // The Naskh Arabic face carries no Latin small-caps feature, so the factory returns None and such a font
    // sets its ordinary letters through the plain path rather than trying to synthesise small caps.
    val arabic = new OtfFont(Files.readAllBytes(Paths.get("fonts/NotoNaskhArabic/NotoNaskhArabic-Regular.ttf")))
    Gsub.fromSmallCaps(arabic.tableBytes("GSUB"), arabic.tableBytes("GDEF")) shouldBe None
  }
