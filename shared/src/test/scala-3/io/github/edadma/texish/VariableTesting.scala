package io.github.edadma.texish

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class VariableTesting extends AnyFreeSpec with Matchers with Testing:
  "vars 1" in {
    test("""\set v 123 123""") shouldBe "123"
  }
