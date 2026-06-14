package io.github.edadma.texish

import io.github.edadma.texish.opentype.{GlyphAssembly, GlyphPart}

/** A glyph too tall for any single precomposed size variant, built by stacking the font's assembly parts
  * vertically: end caps top and bottom, and one or more repeatable extender pieces in between. The parts are
  * listed bottom to top in the font; an extender is repeated as many times as it takes to reach the target
  * height. Consecutive parts overlap by the font's minimum connector overlap so the joints read as one
  * continuous stroke. The box sits on its baseline (zero descent) and rises by the assembled height; callers
  * centre it on the math axis with [[AxisCenteredBox]].
  */
class GlyphAssemblyBox(
    t: Typesetter,
    font: Font,
    color: Color,
    assembly: GlyphAssembly,
    target: Double,
    overlap: Double,
) extends ContentBox:

  private val partBoxes: Vector[GlyphBox] =
    assembly.parts.map(p => new GlyphBox(t, p.glyph, font, color))

  private val hasExtender: Boolean = assembly.parts.exists(_.isExtender)

  // The sequence of part boxes placed bottom to top, each extender repeated `r` times in place.
  private def sequence(r: Int): Vector[GlyphBox] =
    assembly.parts.zip(partBoxes).flatMap { (p, b) => if p.isExtender then Vector.fill(r)(b) else Vector(b) }

  private def heightOf(boxes: Vector[GlyphBox]): Double =
    if boxes.isEmpty then 0.0 else boxes.map(_.height).sum - (boxes.size - 1) * overlap

  // Repeat the extenders just enough to span the target. With no extender the assembly is fixed-size and is
  // used as-is — the best the font can do for a target past its largest precomposed variant.
  private val repeats: Int =
    if !hasExtender then 1
    else
      var r = 1
      while r < 256 && heightOf(sequence(r)) < target do r += 1
      r

  private val placed: Vector[GlyphBox] = sequence(repeats)

  val width: Double    = if placed.isEmpty then 0.0 else placed.map(_.width).max
  val xAdvance: Double = width
  val ascent: Double   = heightOf(placed)
  val descent: Double  = 0.0

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    var bottom = y // the assembly's baseline is its bottom edge (descent is zero)
    for g <- placed do
      // place this part so its ink bottom sits at `bottom`, then advance up by its height less the overlap
      g.draw(t, x + (width - g.width) / 2, bottom - g.descent)
      bottom = bottom - g.height + overlap

  override def toString: String = s"GlyphAssemblyBox(parts=${placed.size}, ascent=$ascent, width=$width)"

/** A box vertically centred on the math axis: its content's geometric centre is placed `axisHeight` above
  * the baseline, where relations, fraction bars and fences align. Stretchy delimiters are centred this way so
  * a tall fence brackets the formula symmetrically about the axis rather than sitting on the baseline.
  */
class AxisCenteredBox(inner: Box, axisHeight: Double) extends ContentBox:
  val width: Double    = inner.width
  val xAdvance: Double = inner.xAdvance

  private val half = inner.height / 2

  val ascent: Double  = half + axisHeight
  val descent: Double = half - axisHeight

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    // place the inner box so its own centre lands on the axis (axisHeight above the line's baseline)
    inner.draw(t, x, y - axisHeight + (inner.ascent - inner.descent) / 2)

  override def toString: String = s"AxisCenteredBox(inner=$inner, axisHeight=$axisHeight)"
