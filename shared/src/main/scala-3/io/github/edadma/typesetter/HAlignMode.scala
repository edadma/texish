package io.github.edadma.typesetter

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

//import pprint.pprintln

class HAlignMode(val t: Typesetter) extends Mode:
  private var state: "START" | "FORMAT_LEFT" | "FORMAT_RIGHT" | "ROW" | "NOALIGN" = "START"

  case class Cell(var format: Boolean, material: HBoxBuilder)
  case class Format(left: ListBuffer[Box], right: ListBuffer[Box])
  case class Line(var noalign: ListBuffer[Box], row: ArrayBuffer[Cell])

  val format      = new ArrayBuffer[Format]
  val content     = new ArrayBuffer[Line]
  var column: Int = 0

  newColumn()

  override def op(operation: String): Unit =
    operation match
      case "noalign" =>
        state = "NOALIGN"
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
      case "ROW" =>
        if column == format.length then sys.error("too many columns")
        if content.last.row.nonEmpty && content.last.row.last.format then
          content.last.row.last.material addSeq format(column - 1).right
        content.last.row += Cell(true, new HBoxBuilder(t))
        content.last.row.last.material addSeq format(column).left
        column += 1
      case "NOALIGN" => sys.error("can't add a new column in 'noalign'")

  def newLine(): Unit =
    state match
      case "START"       => sys.error("empty format line")
      case "FORMAT_LEFT" => sys.error("missing # in column format")
      case "FORMAT_RIGHT" =>
        if format.isEmpty then sys.error("need at least one column")
        state = "ROW"
      case "ROW" =>
        if column < format.length then sys.error("too few columns")
        if content.last.row.last.format then content.last.row.last.material addSeq format(column - 1).right
        column = 0
      case "NOALIGN" =>

    content += Line(null, new ArrayBuffer)
    newColumn()

  def omit(): Unit =
    state match
      case "ROW" =>
        content.last.row.last.material.clear()
        content.last.row.last.format = false
      case "NOALIGN" => sys.error("\\omit cannot be used in 'noalign'")
      case _         => sys.error("\\omit cannot be used in the format line")

  def placeholder(): Unit =
    state match
      case "START" | "FORMAT_LEFT" => state = "FORMAT_RIGHT"
      case "FORMAT_RIGHT"          => sys.error("only one # in column format")
      case "ROW"                   => sys.error("no # in content cell")
      case "NOALIGN"               => sys.error("no # in 'noalign'")

  def init(): Unit = ()

  def add(box: Box): Unit =
    state match
      case "START"        => sys.error("can't add a box in the START state")
      case "FORMAT_LEFT"  => format.last.left += box
      case "FORMAT_RIGHT" => format.last.right += box
      case "ROW"          => content.last.row.last.material add box
      case "NOALIGN" =>
        if content.last.noalign eq null then content.last.noalign = new ListBuffer

        content.last.noalign += box

  override def done(): Unit =
    if content.last.row.last.format then content.last.row.last.material addSeq format.last.right

    val hboxes = ArrayBuffer.fill[ArrayBuffer[HBox]](content.length)(new ArrayBuffer[HBox])

    for column <- format.indices do
      val width = content.map(_.row(column).material.size).max

      for line <- content.indices do
        if content(line).noalign eq null then
          val builder = content(line).row(column).material

          builder.toSize = width
          hboxes(line) += builder.result.asInstanceOf[HBox]

    for line <- content.indices do
      if content(line).noalign eq null then
        val hbox = new HBoxBuilder(t)

        hbox addSeq hboxes(line)
        t.modeStack(1) add hbox.result
      else
        content(line).noalign foreach t.modeStack(1).add

    super.done()

  def result: Box = null
