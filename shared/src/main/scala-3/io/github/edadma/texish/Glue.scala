package io.github.edadma.texish

/** Flexible space. Stretch and shrink each carry their own infinity order (0 = finite, 1 = fil, 2 = fill), as in TeX
  * — glue like `12pt plus 2pt minus 1fil` has finite stretch but infinitely shrinkable shrink.
  */
class Glue(
    val naturalSize: Double,
    val stretch: Double = 0,
    val shrink: Double = 0,
    val stretchOrder: Int = 0,
    val shrinkOrder: Int = 0,
    val nobreak: Boolean = false,
) extends SpaceBox:

  val descent: Double = naturalSize
  val width: Double = naturalSize // Initially, it's the natural width
  val xAdvance: Double = naturalSize // Same as width initially

  def -(amount: Double): Glue = Glue(naturalSize - amount, stretch, shrink, stretchOrder, shrinkOrder, nobreak)
  def +(amount: Double): Glue = Glue(naturalSize + amount, stretch, shrink, stretchOrder, shrinkOrder, nobreak)
  def *(amount: Double): Glue = Glue(naturalSize * amount, stretch, shrink, stretchOrder, shrinkOrder, nobreak)
  def noBreak: Glue = Glue(naturalSize, stretch, shrink, stretchOrder, shrinkOrder, true)

  override def toString: String =
    s"$Glue(naturalSize=$naturalSize, stretch=$stretch, shrink=$shrink, stretchOrder=$stretchOrder, shrinkOrder=$shrinkOrder, nobreak=$nobreak)"
end Glue

val FilGlue = Glue(0, 1, 0, 1)

val FillGlue = Glue(0, 1, 0, 2)

val ZeroGlue = Glue(0, 0, 0, 0)

val InfGlue = Glue(0, 1, 1, 1, 1)

/** Interword glue for a font whose space is `space`. Following TeX (cmr10's `3.33pt plus 1.67pt minus 1.11pt`),
  * the space stretches by half its width and shrinks by a third. A finite shrink lets an over-long line tighten
  * instead of going overfull. The font-changing primitives reset the `spaceskip`/`xspaceskip` parameters through
  * these so every font's interword glue is flexed the same way. */
def interwordGlue(space: Double): Glue  = Glue(space, space / 2, space / 3)
def xinterwordGlue(space: Double): Glue = Glue(space * 1.5, space / 2, space / 3)
