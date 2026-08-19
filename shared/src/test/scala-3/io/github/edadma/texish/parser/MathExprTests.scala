package io.github.edadma.texish.parser

import io.github.edadma.texish.HeadlessTypesetter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The infix math evaluator behind `\calc`: arithmetic with precedence, the function library (trig in radians
  * plus degree variants), constants, length units, and variable resolution. Pure, so it runs on every platform.
  */
class MathExprTests extends AnyFreeSpec with Matchers:

  private def ev(s: String, resolve: String => Option[Double] = _ => None): Double = MathExpr.eval(s, resolve)

  private def approx(a: Double, b: Double): Boolean = math.abs(a - b) < 1e-9

  "arithmetic respects precedence and associativity" in {
    ev("2 + 3 * 4") shouldBe 14.0
    ev("(2 + 3) * 4") shouldBe 20.0
    ev("2 - 3 - 4") shouldBe -5.0      // left associative
    ev("2 ^ 3 ^ 2") shouldBe 512.0     // right associative: 2^(3^2)
    ev("-3 ^ 2") shouldBe -9.0         // unary minus binds looser than ^, as in math
    ev("7 % 3") shouldBe 1.0
  }

  "trig functions take radians, with degree variants for graphics" in {
    assert(approx(ev("sin(0)"), 0.0))
    assert(approx(ev("cos(pi)"), -1.0))
    assert(approx(ev("sind(30)"), 0.5))
    assert(approx(ev("cosd(60)"), 0.5))
    assert(approx(ev("rad(180)"), math.Pi))
    assert(approx(ev("deg(pi)"), 180.0))
    assert(approx(ev("atan2d(1,1)"), 45.0))
  }

  "the function library covers roots, logs, rounding and min/max" in {
    ev("sqrt(16)") shouldBe 4.0
    ev("pow(2,10)") shouldBe 1024.0
    assert(approx(ev("ln(e)"), 1.0))
    ev("log(1000)") shouldBe 3.0
    ev("floor(2.9)") shouldBe 2.0
    ev("ceil(2.1)") shouldBe 3.0
    ev("round(2.5)") shouldBe 3.0
    ev("abs(0 - 5)") shouldBe 5.0
    ev("max(1, 7, 3)") shouldBe 7.0
    ev("min(1, 7, 3)") shouldBe 1.0
    ev("hypot(3, 4)") shouldBe 5.0
  }

  "constants are available" in {
    assert(approx(ev("pi"), math.Pi))
    assert(approx(ev("tau"), 2 * math.Pi))
    assert(approx(ev("e"), math.E))
  }

  "a number may carry a length unit, evaluating to points" in {
    ev("1in") shouldBe 72.0
    ev("2in + 36pt") shouldBe 180.0
    assert(approx(ev("cosd(0) * 1in"), 72.0))
    ev("10mm") shouldBe (72 / 25.4 * 10) +- 1e-9
  }

  "bare identifiers resolve to caller-supplied variables" in {
    val vars = Map("x" -> 3.0, "angle" -> 30.0)
    ev("2 * x", vars.get) shouldBe 6.0
    assert(approx(ev("sind(angle)", vars.get), 0.5))
  }

  "bit operations work over the integer part of their arguments" in {
    ev("and(12, 10)") shouldBe 8.0
    ev("or(12, 10)") shouldBe 14.0
    ev("xor(12, 10)") shouldBe 6.0
    ev("shl(1, 8)") shouldBe 256.0
    ev("shr(256, 4)") shouldBe 16.0
    ev("and(5.9, 3.2)") shouldBe 1.0 // truncated to 5 & 3
  }

  "and, or and xor fold over any number of arguments" in {
    ev("or(1, 2, 4, 8)") shouldBe 15.0
    ev("xor(1, 2, 4)") shouldBe 7.0
    ev("xor(255, 255, 255)") shouldBe 255.0
    ev("and(255)") shouldBe 255.0
    a[MathExpr.MathExprException] should be thrownBy ev("xor()")
  }

  "not complements over 64 bits, so a byte mask is and(not(x), 255)" in {
    ev("not(0)") shouldBe -1.0
    ev("and(not(12), 255)") shouldBe 243.0
  }

  "the log table of GF(256) can be built with shl, and and xor" in {
    // x <<= 1, and when it overflows a byte, reduce by the primitive polynomial — this is the loop every
    // Reed-Solomon encoder starts with, and the reason the bit operations exist
    def double(x: Double, poly: Int): Double =
      val shifted = ev(s"shl($x, 1)")
      if shifted >= 256 then ev(s"xor($shifted, $poly)") else shifted

    double(1, 0x11d) shouldBe 2.0
    double(0x80, 0x11d) shouldBe 0x1d.toDouble  // QR's polynomial
    double(0x80, 0x12d) shouldBe 0x2d.toDouble  // DataMatrix ECC200's, which differs
  }

  "malformed or unevaluable expressions are reported" in {
    a[MathExpr.MathExprException] should be thrownBy ev("2 +")
    a[MathExpr.MathExprException] should be thrownBy ev("(2 + 3")
    a[MathExpr.MathExprException] should be thrownBy ev("1 / 0")
    a[MathExpr.MathExprException] should be thrownBy ev("nope")          // unknown name
    a[MathExpr.MathExprException] should be thrownBy ev("frobnicate(2)") // unknown function
    a[MathExpr.MathExprException] should be thrownBy ev("atan2(1)")      // wrong arity
    a[MathExpr.MathExprException] should be thrownBy ev("shl(1)")        // wrong arity
  }

  "the \\calc primitive computes, reads document variables, and flows into other expressions" in {
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)

    proc.process("\\set angle {30}")
    proc.process("\\set r {\\calc{sind(angle) * 4}}") // sin(30°)=0.5, ×4 = 2
    val Value.Num(r) = handler.get("r"): @unchecked
    r shouldBe 2.0 +- 1e-9

    // the result composes with the prefix arithmetic primitives
    proc.process("\\set total {\\*{\\calc{2in + 36pt}}{2}}") // (144+36)*2 = 360
    handler.get("total") shouldBe Value.Num(360.0)
  }
