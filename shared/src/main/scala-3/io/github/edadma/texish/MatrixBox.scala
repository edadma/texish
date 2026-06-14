package io.github.edadma.texish

/** A math array: a grid of cells laid out in aligned columns and baseline-spaced rows, centred vertically on
  * the math axis so a pair of fences set around it (`\pmatrix`, `\bmatrix`, `\cases`) brackets it
  * symmetrically. Each column is as wide as its widest cell; each row is as tall and deep as its tallest and
  * deepest cell. Cells are centred in their column for a matrix, or set flush left for `\cases`. Columns are
  * separated by `colSep` and successive rows' baselines by the row heights plus `rowSep`.
  *
  * Ragged rows (a row with fewer cells than the widest) are padded with empty cells on the right, so a
  * `\cases` row that gives only an expression still aligns its (absent) condition column.
  */
class MatrixBox(
    rows: Vector[Vector[Box]],
    axisHeight: Double,
    rowSep: Double,
    colSep: Double,
    leftAlign: Boolean,
) extends ContentBox:

  private val nCols: Int = if rows.isEmpty then 0 else rows.map(_.size).max
  private val nRows: Int = rows.size

  private def cell(i: Int, j: Int): Box =
    val row = rows(i)
    if j < row.size then row(j) else MatrixBox.empty

  private val colWidth: Vector[Double] =
    Vector.tabulate(nCols)(j => (0 until nRows).map(i => cell(i, j).width).maxOption.getOrElse(0.0))
  private val rowAscent: Vector[Double] =
    Vector.tabulate(nRows)(i => (0 until nCols).map(j => cell(i, j).ascent).maxOption.getOrElse(0.0))
  private val rowDescent: Vector[Double] =
    Vector.tabulate(nRows)(i => (0 until nCols).map(j => cell(i, j).descent).maxOption.getOrElse(0.0))

  private val totalHeight: Double =
    (0 until nRows).map(i => rowAscent(i) + rowDescent(i)).sum + math.max(0, nRows - 1) * rowSep
  private val half = totalHeight / 2

  val width: Double    = colWidth.sum + math.max(0, nCols - 1) * colSep
  val xAdvance: Double = width
  val ascent: Double   = half + axisHeight
  val descent: Double  = half - axisHeight

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    var rowTop = y - ascent // the top edge of the whole array

    for i <- 0 until nRows do
      val baseline = rowTop + rowAscent(i)
      var colStart = x

      for j <- 0 until nCols do
        val c  = cell(i, j)
        val cx = if leftAlign then colStart else colStart + (colWidth(j) - c.width) / 2
        c.draw(t, cx, baseline)
        colStart += colWidth(j) + colSep

      rowTop += rowAscent(i) + rowDescent(i) + rowSep

  override def toString: String = s"MatrixBox(rows=$nRows, cols=$nCols, width=$width, height=$height)"

object MatrixBox:
  /** An empty cell — zero size — used to pad a ragged row out to the column count. */
  private val empty: Box = HBox(Vector.empty)
