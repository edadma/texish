package io.github.edadma.texish.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The inline-bitmap building blocks: the self-contained base64 decoder (standard and URL-safe alphabets) and the
  * packed-alpha unpacker `\defbitmap` uses. Both are pure, so they're checked directly here.
  */
class BitmapTests extends AnyFreeSpec with Matchers:

  private def bytes(s: String): Seq[Int] = base64Decode(s).toSeq.map(_ & 0xff)

  "base64Decode handles the RFC 4648 test vectors" in {
    bytes("") shouldBe Seq()
    bytes("Zg==") shouldBe Seq('f'.toInt)
    bytes("Zm8=") shouldBe "fo".map(_.toInt)
    bytes("Zm9v") shouldBe "foo".map(_.toInt)
    bytes("Zm9vYg==") shouldBe "foob".map(_.toInt)
    bytes("Zm9vYmE=") shouldBe "fooba".map(_.toInt)
    bytes("Zm9vYmFy") shouldBe "foobar".map(_.toInt)
  }

  "base64Decode accepts both the standard and URL-safe alphabets" in {
    // 0xf8 encodes to a group starting with sextet 62 ('+' standard, '-' URL-safe); 0xff,0xff,0xff is 63,63,63,63
    bytes("+A==") shouldBe Seq(0xf8)
    bytes("-A==") shouldBe Seq(0xf8)
    bytes("////") shouldBe Seq(0xff, 0xff, 0xff)
    bytes("____") shouldBe Seq(0xff, 0xff, 0xff)
  }

  "base64Decode ignores whitespace and padding so wrapped data still decodes" in {
    bytes("Zm9v\n Ym\tFy") shouldBe "foobar".map(_.toInt)
  }

  "unpackBitmapAlpha turns packed bits into alpha levels in the high byte" in {
    // 1-bit, 8 pixels: 0xAA = 1010_1010 -> alternating opaque / transparent
    val a1 = unpackBitmapAlpha(Array(0xaa.toByte), 8, 1, 1)
    a1.map(p => (p >>> 24) & 0xff).toSeq shouldBe Seq(255, 0, 255, 0, 255, 0, 255, 0)

    // 2-bit, 4 pixels: 0x1B = 00_01_10_11 -> levels 0,1,2,3 -> alpha 0, 85, 170, 255
    val a2 = unpackBitmapAlpha(Array(0x1b.toByte), 4, 1, 2)
    a2.map(p => (p >>> 24) & 0xff).toSeq shouldBe Seq(0, 85, 170, 255)

    // colour is black, so only the alpha byte is ever set
    a2.forall(p => (p & 0x00ffffff) == 0) shouldBe true
  }
