package io.github.edadma.typesetter

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class HAlignMode(val t: Typesetter) extends Mode:
  private var state: "FORMAT" | "CONTENT" = "FORMAT"

  val format             = new ArrayBuffer[ListBuffer[Box]]
  var formatColumns: Int = 0
  val content            = new ArrayBuffer[ArrayBuffer[ListBuffer[Box]]]
  var lineColumns: Int   = 0

  def newColumn(): Unit =
    state match
      case "FORMAT" =>
        format += new ListBuffer
        formatColumns += 1
      case "CONTENT" =>
        content.last += new ListBuffer
        lineColumns += 1
        if lineColumns > formatColumns then sys.error("too many columns")

  def newLine(): Unit =
    state match
      case "FORMAT" =>
        if formatColumns == 0 then sys.error("need at least one column")
        state = "CONTENT"
      case "CONTENT" =>
        if lineColumns < formatColumns then sys.error("too few columns")
        lineColumns = 0

    content += new ArrayBuffer
    newColumn()

  def init(): Unit = newColumn()

  def add(box: Box): Unit =
    state match
      case "FORMAT"  => format.last += box
      case "CONTENT" => content.last.last += box

  override def done(): Unit =
    val hboxes = new ArrayBuffer[ArrayBuffer[HBoxBuilder]]

    super.done()

  def result: Box = ???
