package io.github.edadma.typesetter

class Font(
    val typeface: String,
    val size: Double,
    //                 extents: FontExtents,
    val space: Double,
    val xHeight: Double,
    val style: Set[String],
    val renderFont: Any, // the creating typesetter's RenderFont; Any because Font travels through the engine-agnostic scope
    val baseline: Option[Double],
    val ligatures: Set[String],
):
  override def equals(obj: Any): Boolean =
    obj match
      case that: Font => this.typeface == that.typeface && this.size == that.size && this.style == this.style
      case _          => false

  override def toString: String = s"Font(typeface=$typeface)"
