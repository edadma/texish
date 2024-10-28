package io.github.edadma.typesetter

abstract class IoperativeBox extends Box:
  val ascent: Double = ???

  val descent: Double = ???

  val width: Double = ???

  val isSpace: Boolean = ???

  val xAdvance: Double = ???

  def draw(t: Typesetter, x: Double, y: Double): Unit = ???
