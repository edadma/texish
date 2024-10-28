package io.github.edadma.typesetter

import scala.collection.mutable.ArrayBuffer

class HAlignMode(val t: Typesetter) extends Mode:
  private var state: "FORMAT" | "CONTENT" = "FORMAT"

  val format  = new ArrayBuffer[HBoxBuilder]
  val content = new ArrayBuffer[ArrayBuffer[HBoxBuilder]]

  private def newColumn(): Unit =
    state match
      case "FORMAT"  => format += new HBoxBuilder(t)
      case "CONTENT" => content.last += new HBoxBuilder(t)

  private def newLine(): Unit =
    state match
      case "FORMAT"  => state = "CONTENT"
      case "CONTENT" =>

    content += new ArrayBuffer[HBoxBuilder]

  def init(): Unit = ()

  def add(box: Box): Unit =
    state match
      case "FORMAT"  => format.last add box
      case "CONTENT" => content.last.last add box

  override def done(): Unit =
    
    super.done()

  def result: Box = ???
