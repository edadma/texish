package io.github.edadma.typesetter

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class HAlignMode(val t: Typesetter) extends Mode:
  private var state: "START" | "FORMAT_LEFT" | "FORMAT_RIGHT" | "CONTENT" = "START"

  case class Cell(var format: Boolean, material: HBoxBuilder)
  case class Format(left: ListBuffer[Box], right: ListBuffer[Box])

  val format       = new ArrayBuffer[Format]
  val content      = new ArrayBuffer[ArrayBuffer[Cell]]
  var columns: Int = 0

  newColumn()

  override def op(operation: String): Unit =
    operation match
      case "omit"        => omit()
      case "newColumn"   => newColumn()
      case "newLine"     => newLine()
      case "placeholder" => placeholder()
      case _             => sys.error(s"illegal operation '$operation'")

  def newColumn(): Unit =
    state match
      case "FORMAT_LEFT" => sys.error("missing # in column format")
      case "START" | "FORMAT_RIGHT" =>
        format += Format(new ListBuffer, new ListBuffer)
        state = "FORMAT_LEFT"
      case "CONTENT" =>
        if columns > 0 && content.last.last.format then content.last.last.material addSeq format(columns - 1).right
        content.last += Cell(false, new HBoxBuilder(t))
        content.last.last.material addSeq format(columns).left
        columns += 1
        if columns > format.length then sys.error("too many columns")

  def newLine(): Unit =
    state match
      case "START"       => sys.error("empty format line")
      case "FORMAT_LEFT" => sys.error("missing # in column format")
      case "FORMAT_RIGHT" =>
        if format.isEmpty then sys.error("need at least one column")
        state = "CONTENT"
      case "CONTENT" =>
        if columns < format.length then sys.error("too few columns")
        columns = 0

    content += new ArrayBuffer
    newColumn()

  def omit(): Unit =
    state match
      case "CONTENT" =>
        content.last.last.material.clear()
        content.last.last.format = false
      case _ => sys.error("\\omit cannot be used in the format line")

  def placeholder(): Unit =
    state match
      case "START" | "FORMAT_LEFT" => state = "FORMAT_RIGHT"
      case "FORMAT_RIGHT"          => sys.error("only one # in column format")
      case "CONTENT"               => sys.error("no # in content cell")

  def init(): Unit = ()

  def add(box: Box): Unit =
    state match
      case "START"        => sys.error("can't add a box in the START state")
      case "FORMAT_LEFT"  => format.last.left += box
      case "FORMAT_RIGHT" => format.last.right += box
      case "CONTENT"      => content.last.last.material add box

  override def done(): Unit =
    val hboxes = ArrayBuffer.fill[ArrayBuffer[HBox]](content.length)(new ArrayBuffer[HBox])

    for column <- format.indices do
      val width = content.map(_(column).material.size).max

      for line <- content.indices do
        val builder = content(line)(column).material

        builder.toSize = width
        hboxes(line) += builder.result.asInstanceOf[HBox]

    for line <- hboxes.indices do
      val hbox = new HBoxBuilder(t)

      hbox addSeq hboxes(line)
      t.modeStack(1) add hbox.result

    super.done()

  def result: Box = null
