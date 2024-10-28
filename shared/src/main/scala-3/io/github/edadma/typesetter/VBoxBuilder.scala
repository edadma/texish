package io.github.edadma.typesetter

class VBoxBuilder(val t: Typesetter, protected var toSize: Double | Null = null) extends VerticalMode:

  protected val wrap: Seq[Box] => Box = VBox(_)
