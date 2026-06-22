package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 6d: math arrays. Geometry is checked on the fixed-metric stub (each glyph 6 wide, ascent 8,
  * descent 2), with the row/column gaps passed in explicitly so the positions are exact.
  */
class MathMatrixTests extends AnyFreeSpec with Matchers:

  private def glyph(t: Typesetter, c: Char): Box = new GlyphBox(t, c.toInt, t.currentFont, t.currentColor)

  "a matrix aligns its columns and stacks its rows, centred on the math axis" in {
    val rec  = new RecordingGlyphTypesetter
    val rows = Vector(
      Vector(glyph(rec, 'a'), glyph(rec, 'b')),
      Vector(glyph(rec, 'c'), glyph(rec, 'd')),
    )
    // axis 3.5, rowSep 2, colSep 4: rows are 10 tall, total height 22, width 6+6+4 = 16
    val m = new MatrixBox(rows, axisHeight = 3.5, rowSep = 2.0, Vector.fill(2)(ColumnAlign.Center), Vector(4.0))

    m.width shouldBe (16.0 +- 1e-9)
    (m.ascent - m.descent) shouldBe (7.0 +- 1e-9) // centred on the axis: ascent − descent = 2·axisHeight

    rec.draw(m)
    def at(c: Char): (Double, Double) = rec.drawn.collectFirst { case (g, x, y) if g == c.toInt => (x, y) }.get

    at('a')._1 shouldBe at('c')._1 // column 0 aligned across rows
    at('b')._1 shouldBe at('d')._1 // column 1 aligned across rows
    at('b')._1 should be > at('a')._1 // column 1 is to the right of column 0
    at('a')._2 shouldBe at('b')._2 // row 0 shares a baseline
    at('a')._2 should be < at('c')._2 // row 0 sits above row 1
  }

  "cells centre in their column for a matrix but sit flush left for cases" in {
    def narrowCellX(leftAlign: Boolean): Double =
      val rec  = new RecordingGlyphTypesetter
      val rows = Vector(
        Vector(HBox(Vector(glyph(rec, 'a'), glyph(rec, 'b')))), // a wide (12) cell sets the column width
        Vector(glyph(rec, 'c')),                                // a narrow (6) cell under it
      )
      val align = if leftAlign then ColumnAlign.Left else ColumnAlign.Center
      val m     = new MatrixBox(rows, axisHeight = 3.5, rowSep = 0.0, Vector(align), Vector.empty)
      rec.draw(m)
      rec.drawn.collectFirst { case (g, x, _) if g == 'c'.toInt => x }.get

    narrowCellX(leftAlign = false) shouldBe (3.0 +- 1e-9) // centred under the 12-wide column: (12 − 6)/2
    narrowCellX(leftAlign = true) shouldBe (0.0 +- 1e-9)  // flush left
  }

  "a ragged row is padded so the array still has full width" in {
    val rec  = new RecordingGlyphTypesetter
    val rows = Vector(
      Vector(glyph(rec, 'a'), glyph(rec, 'b')), // two columns
      Vector(glyph(rec, 'c')),                  // only one — the second is an empty pad
    )
    val m = new MatrixBox(rows, axisHeight = 3.5, rowSep = 0.0, Vector.fill(2)(ColumnAlign.Center), Vector(4.0))

    m.width shouldBe (16.0 +- 1e-9) // 6 + 6 + 4, the column count is the widest row's
  }
