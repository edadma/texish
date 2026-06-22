package io.github.edadma.texish

/** How a column's cells sit within the column's width: flush left, centred, or flush right. A matrix centres
  * every column; `\cases` sets them flush left; an aligned-equation block alternates right then left so the
  * relation symbols line up down the seam. */
enum ColumnAlign:
  case Left, Center, Right

/** The alignment pattern an array environment lays its columns out in. `Center` centres every column (the
  * matrix family, `gathered`); `Left` sets them flush left (`\cases`); `Aligned` alternates right, left, right,
  * … with no gap at the right→left seam and a wide gap between successive pairs, so a block of equations lines
  * up on its relation symbols (`aligned`, `split`). */
enum MathArrayAlign:
  case Center, Left, Aligned

/** A math array: a grid of cells laid out in aligned columns and baseline-spaced rows, centred vertically on
  * the math axis so a pair of fences set around it (`\pmatrix`, `\bmatrix`, `\cases`) brackets it
  * symmetrically. Each column is as wide as its widest cell; each row is as tall and deep as its tallest and
  * deepest cell. Each column's cells sit per its [[ColumnAlign]], and the gap before each column is given
  * individually by `colSeps` (so an aligned block can butt its relation column hard against the term to its
  * left while keeping a wide gap between equation pairs).
  *
  * Ragged rows (a row with fewer cells than the widest) are padded with empty cells on the right, so a
  * `\cases` row that gives only an expression still aligns its (absent) condition column.
  */
class MatrixBox(
    rows: Vector[Vector[Box]],
    axisHeight: Double,
    rowSep: Double,
    colAligns: Vector[ColumnAlign],
    colSeps: Vector[Double],
) extends ContentBox:

  private val nCols: Int = if rows.isEmpty then 0 else rows.map(_.size).max
  private val nRows: Int = rows.size

  private def cell(i: Int, j: Int): Box =
    val row = rows(i)
    if j < row.size then row(j) else MatrixBox.empty

  /** The alignment of column `j`, defaulting to centred for any column the caller did not specify. */
  private def alignOf(j: Int): ColumnAlign = if j < colAligns.length then colAligns(j) else ColumnAlign.Center

  /** The gap to the left of column `j` (`j >= 1`); the first column has no leading gap. */
  private def sepBefore(j: Int): Double = if j >= 1 && j - 1 < colSeps.length then colSeps(j - 1) else 0.0

  private val colWidth: Vector[Double] =
    Vector.tabulate(nCols)(j => (0 until nRows).map(i => cell(i, j).width).maxOption.getOrElse(0.0))
  private val rowAscent: Vector[Double] =
    Vector.tabulate(nRows)(i => (0 until nCols).map(j => cell(i, j).ascent).maxOption.getOrElse(0.0))
  private val rowDescent: Vector[Double] =
    Vector.tabulate(nRows)(i => (0 until nCols).map(j => cell(i, j).descent).maxOption.getOrElse(0.0))

  private val totalHeight: Double =
    (0 until nRows).map(i => rowAscent(i) + rowDescent(i)).sum + math.max(0, nRows - 1) * rowSep
  private val half = totalHeight / 2

  val width: Double    = colWidth.sum + (1 until nCols).map(sepBefore).sum
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
        val c = cell(i, j)
        val cx = alignOf(j) match
          case ColumnAlign.Left   => colStart
          case ColumnAlign.Right  => colStart + (colWidth(j) - c.width)
          case ColumnAlign.Center => colStart + (colWidth(j) - c.width) / 2
        c.draw(t, cx, baseline)
        colStart += colWidth(j)
        if j < nCols - 1 then colStart += sepBefore(j + 1)

      rowTop += rowAscent(i) + rowDescent(i) + rowSep

  override def toString: String = s"MatrixBox(rows=$nRows, cols=$nCols, width=$width, height=$height)"

object MatrixBox:
  /** An empty cell — zero size — used to pad a ragged row out to the column count. */
  private val empty: Box = HBox(Vector.empty)
