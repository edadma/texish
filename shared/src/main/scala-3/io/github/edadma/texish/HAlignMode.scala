package io.github.edadma.texish

import io.github.edadma.texish.parser.Value

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

//import pprint.pprintln

class HAlignMode(val t: Typesetter) extends Mode:

  /** The inter-column glue (TeX's \tabskip), placed before the first column, between columns, and after the last.
    * Read from the `tabskip` variable; absent or zero means columns simply abut, as before.
    */
  private def tabskip: Glue =
    t.get("tabskip") match
      case Some(Value.Glue(n, st, sh, sto, sho)) => Glue(n, st, sh, sto, sho)
      case Some(Value.Native(g: Glue))           => g
      case Some(Value.Dimen(p))                  => Glue(p)
      case Some(Value.Num(n))                    => Glue(n)
      case _                                     => Glue(0)
  private var state: "START" | "FORMAT_LEFT" | "FORMAT_RIGHT" | "ROW" | "NOALIGN" = "START"

  // `started` flips once the cell receives its first real (non-space) content, so a space at the very start of a
  // cell entry — e.g. the space after the \cr that ends the previous row — can be dropped, as in TeX.
  case class Cell(var format: Boolean, material: HBoxBuilder, var started: Boolean = false)
  case class Format(left: ListBuffer[Box], right: ListBuffer[Box])
  // `filled` records whether the row received any typeset content; a row opened by a trailing \cr but never
  // filled is dropped at done() rather than emitted as a blank line (as in TeX).
  case class Line(var noalign: ListBuffer[Box], row: ArrayBuffer[Cell], var filled: Boolean = false)

  val format      = new ArrayBuffer[Format]
  val content     = new ArrayBuffer[Line]
  var column: Int = 0

  newColumn()

  override def op(operation: String): Unit =
    operation match
      case "noalign-begin" =>
        state = "NOALIGN"
      case "noalign-end" =>
        newLine()
        state = "ROW"
      case "omit" => omit()
      case "newColumn" =>
        if state == "NOALIGN" then sys.error("can't add a new column in 'noalign'")
        newColumn()
      case "newLine"     => newLine()
      case "placeholder" => placeholder()
      case _             => sys.error(s"illegal operation '$operation'")

  def newColumn(): Unit =
    state match
      case "FORMAT_LEFT" => sys.error("missing # in column format")
      case "START" | "FORMAT_RIGHT" =>
        format += Format(new ListBuffer, new ListBuffer)
        state = "FORMAT_LEFT"
      case "ROW" | "NOALIGN" =>
        if column == format.length then sys.error("too many columns")
        if content.last.row.nonEmpty && content.last.row.last.format then
          content.last.row.last.material addSeq format(column - 1).right

        content.last.row += Cell(true, new HBoxBuilder(t))
        content.last.row.last.material addSeq format(column).left
        column += 1

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
        column = 0

    content += Line(null, new ArrayBuffer)
    newColumn()

  def omit(): Unit =
    state match
      case "ROW" =>
        content.last.row.last.material.clear()
        content.last.row.last.format = false
        content.last.row.last.started = false // a fresh cell entry: its leading space is dropped too
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
      case "ROW" =>
        val cell = content.last.row.last
        box match
          case c: CharBox if !cell.started =>
            // drop leading whitespace at the start of the cell entry (a pure-space box vanishes entirely)
            val trimmed = c.text.dropWhile(_.isWhitespace)
            if trimmed.nonEmpty then
              cell.material add (if trimmed.length == c.text.length then c else c.newCharBox(trimmed))
              cell.started = true
              content.last.filled = true
          case _ =>
            cell.material add box
            cell.started = true
            content.last.filled = true
      case "NOALIGN" =>
        if content.last.noalign eq null then content.last.noalign = new ListBuffer

        content.last.noalign += box

  override def done(): Unit =
    // A trailing \cr opens a row that never receives content. Drop that empty final row (TeX does the same);
    // it also has too few cells to take part in the column-width pass, so leaving it would crash.
    val droppedTrailing =
      if content.nonEmpty && content.last.noalign == null && !content.last.filled then
        content.remove(content.length - 1)
        true
      else false

    if content.isEmpty then
      super.done()
      return

    // If the table did not end with \cr, its last row was never closed by newLine; close its final cell's right
    // template now. (When a trailing \cr was dropped, the real last row is already closed — don't double-add.)
    if !droppedTrailing && content.last.noalign == null then
      if content.last.row.length < format.length then sys.error("too few columns in the last row (missing \\cr?)")
      if content.last.row.last.format then content.last.row.last.material addSeq format.last.right

    val hboxes = ArrayBuffer.fill[ArrayBuffer[HBox]](content.length)(new ArrayBuffer[HBox])

    for column <- format.indices do
      val width = content.map(line => if line.noalign ne null then 0 else line.row(column).material.size).max

      for line <- content.indices do
        if content(line).noalign eq null then
          val builder = content(line).row(column).material

          builder.toSize = width
          hboxes(line) += builder.result.asInstanceOf[HBox]

    val skip = tabskip
    val gap  = skip.naturalSize != 0 || skip.stretch != 0 || skip.shrink != 0

    for line <- content.indices do
      if content(line).noalign eq null then
        val hbox = new HBoxBuilder(t)

        if gap then
          // \tabskip glue surrounds and separates the columns; a fresh Glue per slot since glue-setting mutates
          // the builder's box list in place
          def skipBox = Glue(skip.naturalSize, skip.stretch, skip.shrink, skip.stretchOrder, skip.shrinkOrder)
          hbox add skipBox
          for cell <- hboxes(line) do
            hbox add cell
            hbox add skipBox
        else hbox addSeq hboxes(line)

        t.modeStack(1) add hbox.result
      else
        content(line).noalign foreach t.modeStack(1).add

    super.done()

  def result: Box = null
