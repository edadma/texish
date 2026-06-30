package io.github.edadma.texish

class HSpaceBox(val width: Double, val indent: Boolean = false) extends SpaceBox:

  val descent: Double = 0
  val xAdvance: Double = width

  override def toString: String = s"HSpaceBox(width=$width)"
