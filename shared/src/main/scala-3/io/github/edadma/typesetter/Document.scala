package io.github.edadma.typesetter

import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

abstract class Document extends Mode:
  private[typesetter] var ts: Typesetter = uninitialized
  val printedPages                       = new ArrayBuffer[Any]
  var page: Int                          = 0
  var eject: Boolean                     = false

  def t: Typesetter = ts

  def layout(b: Box): Box

  infix def add(box: Box): Unit

  override def done(): Unit =
    pop

    if eject then t.ejectPageTarget()

  def result: Box = ???
