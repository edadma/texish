package io.github.edadma.typesetter

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
