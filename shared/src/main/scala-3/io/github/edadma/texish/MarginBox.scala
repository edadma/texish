package io.github.edadma.texish

class MarginBox(box: Box, top: Double, right: Double, bottom: Double, left: Double) extends ContentBox:
  val width: Double = box.width + left + right
  val ascent: Double = box.ascent + top
  val descent: Double = box.descent + bottom
  val xAdvance: Double = width

  // the left margin shifts the content right; the top margin needs no shift because ascent grows upward
  // from the unchanged baseline
  def draw(t: Typesetter, x: Double, y: Double): Unit = box.draw(t, x + left, y)
