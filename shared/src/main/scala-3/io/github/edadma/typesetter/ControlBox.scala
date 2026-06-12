package io.github.edadma.typesetter

/** A zero-size bookkeeping item (penalty, mark) that lives in box lists but is invisible to layout: it draws
  * nothing, occupies no space, and interline-glue insertion looks straight through it.
  */
trait ControlBox extends Box:
  val ascent: Double   = 0
  val descent: Double  = 0
  val width: Double    = 0
  val xAdvance: Double = 0

  def draw(t: Typesetter, x: Double, y: Double): Unit = ()
