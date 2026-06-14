package io.github.edadma.texish

import scala.language.postfixOps

//import pprint.pprintln

abstract class ListBoxBuilder extends Builder:
  protected val measure: Box => Double
  protected val skip: Double => Box
  protected var toSize: Double | Null
  protected val wrap: Seq[Box] => Box

  def size: Double = boxes map measure sum

  def result: Box =
    toSize match
      case null      => wrap(build)
      case s: Double => wrap(buildTo(s))

  protected def build: Seq[Box] =
    boxes map {
      case g: Glue => skip(g.naturalSize)
      case b       => b
    } toSeq

  /** Set the glue so the box reaches the target size — TeX's glue-setting rules.
    *
    * All of the excess (or deficit) goes to the glue at the highest infinity order that can absorb it; glue at other
    * orders is set to its natural size. Stretching is unbounded (an underfull box is merely ugly), but finite shrink
    * is a hard limit: glue never shrinks past it, and the box is reported overfull by the amount that didn't fit.
    */
  protected def buildTo(size: Double): Seq[Box] =
    val naturalSize = boxes map measure sum
    val delta       = size - naturalSize

    val glue = boxes.zipWithIndex.collect { case (g: Glue, idx) => (g, idx) }

    // replace every Glue with a fixed skip of the given size
    def setGlue(f: Glue => Double): Unit =
      glue.foreach((g, idx) => boxes(idx) = skip(f(g)))

    var reported = false

    def warn(msg: String): Unit =
      println(s"Warning: $msg in ${getClass.getName}")
      reported = true

    if glue.isEmpty then
      if math.abs(delta) > 1e-6 then warn(f"box is ${-delta}%.2f off its target size and contains no glue")
    else if math.abs(delta) <= 1e-6 then setGlue(_.naturalSize)
    else if delta > 0 then
      glue.collect { case (g, _) if g.stretch > 0 => g.stretchOrder }.maxOption match
        case None =>
          warn(f"underfull box: $delta%.2f of stretch needed but no glue can stretch")
          setGlue(_.naturalSize)
        case Some(order) =>
          val total = glue.collect { case (g, _) if g.stretchOrder == order => g.stretch }.sum
          val r     = delta / total
          setGlue(g => if g.stretchOrder == order then g.naturalSize + r * g.stretch else g.naturalSize)
    else
      glue.collect { case (g, _) if g.shrink > 0 => g.shrinkOrder }.maxOption match
        case None =>
          warn(f"overfull box: content exceeds the target size by ${-delta}%.2f and no glue can shrink")
          setGlue(_.naturalSize)
        case Some(order) =>
          val total = glue.collect { case (g, _) if g.shrinkOrder == order => g.shrink }.sum
          if order == 0 && -delta > total then
            // finite shrink is a hard limit: set glue at maximum shrink and report the overflow
            warn(f"overfull box: content exceeds the target size by ${-delta - total}%.2f")
            setGlue(g => g.naturalSize - (if g.shrinkOrder == 0 then g.shrink else 0))
          else
            val r = -delta / total
            setGlue(g => if g.shrinkOrder == order then g.naturalSize - r * g.shrink else g.naturalSize)

    // safety net: if the glue was set correctly the box is exactly on target unless already reported
    val finalSize = boxes map measure sum

    if !reported && math.abs(finalSize - size) > 1e-3 then
      println(s"Warning: Final size ($finalSize) of '${getClass.getName}' does not match target size ($size).")

    boxes.toList

  override def toString: String = s"${getClass.getName}(${boxes.mkString(", ")})"
