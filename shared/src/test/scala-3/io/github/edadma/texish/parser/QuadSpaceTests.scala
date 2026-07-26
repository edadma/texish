package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Box, HSpaceBox, HeadlessTypesetter, Mode, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The wide manual spaces `\quad` (one em) and `\qquad` (two ems), the TeX gaps for setting formulas or
  * examples apart. They scale with the font, so on the [[HeadlessTypesetter]]'s 10pt default one em is 10pt.
  */
class QuadSpaceTests extends AnyFreeSpec with Matchers:

  private class CaptureMode(val t: Typesetter) extends Mode:
    val items                     = ArrayBuffer[Box]()
    def init(): Unit              = ()
    infix def add(box: Box): Unit = items += box
    def result: Box | Null        = null

  private def spaceWidth(src: String): Double =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    val cap = new CaptureMode(t)
    t.push(cap)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(src))
    cap.items.collectFirst { case s: HSpaceBox => s.width }.getOrElse(fail(s"no space box from '$src'"))

  "\\quad is a one-em space and \\qquad is two ems" in {
    val em = 10.0 // the default font size
    spaceWidth("a\\quad b") shouldBe (em +- 0.001)
    spaceWidth("a\\qquad b") shouldBe (2 * em +- 0.001)
  }
