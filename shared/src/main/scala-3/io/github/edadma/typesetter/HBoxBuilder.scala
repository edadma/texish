package io.github.edadma.typesetter

class HBoxBuilder(val t: Typesetter, var toSize: Double | Null = null) extends ListBoxBuilder with HorizontalMode:

  protected val measure: Box => Double = _.width
  protected val skip: Double => Box    = HSpaceBox(_)
  protected val wrap: Seq[Box] => Box  = HBox(_)
