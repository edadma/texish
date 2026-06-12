package io.github.edadma.typesetter

/** A breakpoint marker, as in TeX. A penalty occupies no space and draws nothing; it only tells the page builder how
  * desirable a break is at this position. A value at or above [[Penalty.Inhibit]] forbids a break here; a value at or
  * below [[Penalty.Force]] forces one. Like glue, a penalty is discardable — it is dropped at the top of a new page.
  */
class Penalty(val penalty: Int) extends Box:
  val ascent: Double   = 0
  val descent: Double  = 0
  val width: Double    = 0
  val xAdvance: Double = 0
  val isSpace: Boolean = true

  def draw(t: Typesetter, x: Double, y: Double): Unit = ()

  override def toString: String = s"Penalty($penalty)"

object Penalty:
  /** Penalties at or above this value forbid a break. */
  val Inhibit = 10000

  /** Penalties at or below this value force a break. */
  val Force = -10000
