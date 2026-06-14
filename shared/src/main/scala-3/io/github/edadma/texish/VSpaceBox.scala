package io.github.edadma.texish

class VSpaceBox(val descent: Double) extends SpaceBox:

  val width: Double = 0
  val xAdvance: Double = 0

  override def toString: String = s"VSpaceBox(height=$height)"
