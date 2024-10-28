package io.github.edadma.typesetter

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class HAlignMode(val t: Typesetter) extends Mode:
  private var state: "FORMAT" | "CONTENT" = "FORMAT"

  val format  = new ArrayBuffer[ListBuffer[Box]]
  val content = new ArrayBuffer[ArrayBuffer[ListBuffer[Box]]]

  private def newColumn(): Unit =
    state match
      case "FORMAT"  => format += new ListBuffer
      case "CONTENT" => content.last += new ListBuffer

  private def newLine(): Unit =
    state match
      case "FORMAT"  => state = "CONTENT"
      case "CONTENT" =>

    content += new ArrayBuffer

  def init(): Unit = ()

  def add(box: Box): Unit =
    state match
      case "FORMAT"  => format.last += box
      case "CONTENT" => content.last.last += box

  override def done(): Unit =
    super.done()

  def result: Box = ???
