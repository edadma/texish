package io.github.edadma.typesetter

import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

class DocumentMode extends Mode:
  private[typesetter] var ts: Typesetter = uninitialized
  val printedPages                       = new ArrayBuffer[Any]
  var page: Int                          = 0
  var eject: Boolean                     = false

  def t: Typesetter = ts

  def init(): Unit = ()

  def layout(b: Box): Box = b

  infix def add(box: Box): Unit =
    printedPages += t.createPageTarget
    t.draw(
      layout(box),
      t.getNumber("hoffset"),
      t.getNumber("voffset"),
    )
    t.ejectPageTarget()
    page += 1

  override def done(): Unit =
    pop

    if eject then t.ejectPageTarget()

  def result: Box = ???
