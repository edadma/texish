package io.github.edadma.texish

class RuleBox(t: Typesetter, val width: Double, val ascent: Double, val descent: Double, color: Color)
    extends NoGlueBox:
  def this(t: Typesetter, width: Double, ascent: Double, descent: Double) =
    this(t, width, ascent, descent, t.currentColor)
  require(width >= 0, "rule width is non-negative")
  require(ascent >= 0, "rule ascent is non-negative")
  require(descent >= 0, "rule descent is non-negative")

//  val (ascent, descent) =
//    if shift >= 0 then (thickness + shift, 0d)
//    else if thickness + shift < 0 then (0d, thickness - shift)
//    else (thickness + shift, -shift)

  val xAdvance: Double = width

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    t.setColor(color)
    t.fillRect(x, y - ascent, width, height)
