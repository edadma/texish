package io.github.edadma.texish

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
    // True when small caps was asked for but no dedicated small-caps cut of this typeface was loaded, so the
    // resolved face is the ordinary (upright/roman) cut standing in for it. A face that carries the OpenType
    // `smcp` feature can then synthesize small caps from its lowercase letters (see CharBox); one that does not
    // simply sets the ordinary letters, as it did before. False whenever a real small-caps cut was selected.
    val syntheticSmallcaps: Boolean = false,
):
  override def equals(obj: Any): Boolean =
    obj match
      case that: Font => this.typeface == that.typeface && this.size == that.size && this.style == that.style
      case _          => false

  override def hashCode: Int = (typeface, size, style).hashCode

  override def toString: String = s"Font(typeface=$typeface)"
