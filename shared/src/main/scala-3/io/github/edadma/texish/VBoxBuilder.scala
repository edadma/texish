package io.github.edadma.texish

class VBoxBuilder(
    val t: Typesetter,
    protected var toSize: Double | Null = null,
    spreadAmt: Double | Null = null,
    top: Boolean = false,
) extends VerticalMode:

  spread = spreadAmt

  protected val wrap: Seq[Box] => Box = if top then VTop(_) else VBox(_)
