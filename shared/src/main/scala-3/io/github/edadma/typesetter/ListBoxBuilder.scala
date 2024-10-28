package io.github.edadma.typesetter

import scala.language.postfixOps

import pprint.pprintln

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

  protected def buildTo(size: Double): Seq[Box] =
    // Step 1: Calculate the natural size of all boxes
    val naturalSize = boxes map measure sum
    val delta       = size - naturalSize

    // Step 2: Collect all Glue with their indices
    val glueBoxesWithIndices = boxes.zipWithIndex.collect { case (g: Glue, idx) =>
      (g, idx)
    }

    // If there are no Glue, return the boxes as-is (or handle accordingly)
    if (glueBoxesWithIndices.isEmpty) {
      if (delta != 0) {
        println(s"Warning: No glue available to adjust the size [${boxes mkString ", "}]")
      }

      return boxes.toList
    }

    // Step 3: Determine the maximum glue order present
    val maxOrder = glueBoxesWithIndices.map(_._1.order).max

    // Function to distribute space (stretch or shrink)
    def distributeSpace(
        remaining: Double,
        glueBoxes: scala.collection.Seq[(Glue, Int)],
        totalFlex: Double,
        adjust: (Glue, Double) => Double,
    ): Double = {
      if (totalFlex == 0) remaining
      else {
        val flexPerUnit = remaining / totalFlex
        glueBoxes.foldLeft(remaining) { case (rem, (g, idx)) =>
          val adjustment = adjust(g, flexPerUnit)
          val newSize    = measure(g) + adjustment
          boxes(idx) = skip(newSize)
          rem - math.abs(adjustment)
        }
      }
    }

    // Step 4: Distribute the delta
    var remaining    = delta
    var currentOrder = maxOrder

    while ((remaining > 1e-6 || remaining < -1e-6) && currentOrder >= 0) {
      val currentGlueBoxes = glueBoxesWithIndices.filter(_._1.order == currentOrder)

      if (delta > 0) {
        // Stretching
        val totalStretch = currentGlueBoxes.map(_._1.stretch).sum
        if (totalStretch > 0) {
          val allocated = distributeSpace(
            remaining,
            currentGlueBoxes,
            totalStretch,
            (g, flexPerUnit) => g.stretch * flexPerUnit,
          )
          remaining = delta - (delta - allocated)
        }
      } else {
        // Shrinking
        val totalShrink = currentGlueBoxes.map(_._1.shrink).sum
        if (totalShrink > 0) {
          val allocated = distributeSpace(
            -remaining,
            currentGlueBoxes,
            totalShrink,
            (g, flexPerUnit) => -g.shrink * flexPerUnit,
          )
          remaining = delta + (delta + allocated)
        }
      }

      currentOrder -= 1
    }

    // Step 5: Replace remaining Glue with their natural sizes if any
    glueBoxesWithIndices.foreach { case (g, idx) =>
      if (boxes(idx).isInstanceOf[Glue])
        boxes(idx) = skip(measure(g))
    }

    // Step 6: Verify the final size (optional)
    val finalSize = boxes map measure sum

    if (math.abs(finalSize - size) > 1e-3)
      println(s"Warning: Final size ($finalSize) of '${getClass.getName}' does not match target size ($size).")

    // Return the adjusted boxes as a List
    boxes.toList
