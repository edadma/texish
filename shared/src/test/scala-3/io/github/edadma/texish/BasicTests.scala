package io.github.edadma.texish

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BasicTests extends AnyFreeSpec with Matchers with Testing:
  "characters" in {
    test("asdf") shouldBe "asdf"
  }

  "space text 1" in {
    test("asdf zxcv") shouldBe
      """
      |["asdf", " ", "zxcv"]
      """.trim.stripMargin
  }

  "space text 2" in {
    test("asdf  zxcv") shouldBe
      """
      |["asdf", " ", "zxcv"]
      """.trim.stripMargin
  }

  "space text 3" in {
    test(" asdf zxcv") shouldBe
      """
      |[" ", "asdf", " ", "zxcv"]
      """.trim.stripMargin
  }

  "space text 4" in {
    test("  asdf zxcv") shouldBe
      """
      |[" ", "asdf", " ", "zxcv"]
      """.trim.stripMargin
  }

  "space text 5" in {
    test("asdf zxcv ") shouldBe
      """
      |["asdf", " ", "zxcv", " "]
      """.trim.stripMargin
  }

  "space text 6" in {
    test("asdf zxcv  ") shouldBe
      """
      |["asdf", " ", "zxcv", " "]
      """.trim.stripMargin
  }

  "space text 7" in {
    test(" ") shouldBe " "
  }

  "space text 8" in {
    test("  ") shouldBe " "
  }

  "newline text 1" in {
    test(
      """asdf
        |zxcv""".stripMargin,
    ) shouldBe "[\"asdf\", \"\n\", \"zxcv\"]"
  }

  "newline text 2" in {
    test(
      """asdf
        |
        |zxcv""".stripMargin,
    ) shouldBe "[\"asdf\", \"\n\", \"\n\", \"zxcv\"]"
  }
