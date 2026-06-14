package io.github.edadma.texish

/** A fixed-size box holding a vector-graphics drawing. It is a leaf box like [[RuleBox]] or [[ImageBox]]: it
  * flows in the surrounding text, reserving `width` horizontally and `height` above the baseline (its descent
  * is zero, so its bottom edge sits on the baseline). A [[PictureMode]] collects the drawing as an ordered
  * display list of [[PictureOp]]s while the picture body parses; this box replays that list at render time.
  *
  * The display list is written in picture-local coordinates: y-up, origin at the box's bottom-left corner.
  * [[draw]] installs that frame on the backend by translating to the box origin and flipping the y axis, so a
  * picture point `(px, py)` lands at device `(x + px, y - py)` — `py = 0` on the baseline, `py = height` at the
  * top. Stroke and fill colours travel inside each [[PictureOp.Paint]]; stroke width, dash and joins are
  * backend state set by their own ops and unwound by the [[PictureOp.GSave]]/[[PictureOp.GRestore]] a `\group`
  * brackets the body with.
  *
  * Placed content — a label or a raw glyph from `\at`/`\glyph` — must not inherit the y flip, or it would
  * render upside down. A [[PictureOp.Place]] therefore undoes the flip for the duration of the box's own draw,
  * so only the *position* rides the picture's coordinate system while the glyphs stay upright.
  */
class PictureBox(t: Typesetter, val width: Double, height: Double, ops: Vector[PictureOp]) extends NoGlueBox:
  require(width >= 0, "picture width is non-negative")
  require(height >= 0, "picture height is non-negative")

  val ascent: Double   = height
  val descent: Double  = 0
  val xAdvance: Double = width

  /** The display list this box replays, for introspection and testing. */
  def displayList: Vector[PictureOp] = ops

  /** Where the anchored handle of a placed `box` sits relative to the box's own origin — the offsets
    * `(bx, by)` to pass to `box.draw` so the chosen handle lands on the placement point. Computed in the
    * upright (un-flipped) frame, where larger `y` is lower on the page: the box spans `[bx, bx+width]`
    * horizontally and `[by-ascent, by+descent]` vertically, with its baseline at `by`.
    */
  private def anchorOffset(anchor: Anchor, box: Box): (Double, Double) =
    val left   = 0.0
    val center = -box.width / 2
    val right  = -box.width
    val top    = box.ascent                  // pull the box down so its top edge meets the point
    val middle = (box.ascent - box.descent) / 2
    val bottom = -box.descent                // push the box up so its bottom edge meets the point

    anchor match
      case Anchor.Center    => (center, middle)
      case Anchor.North     => (center, top)
      case Anchor.South     => (center, bottom)
      case Anchor.East      => (right, middle)
      case Anchor.West      => (left, middle)
      case Anchor.NorthEast => (right, top)
      case Anchor.NorthWest => (left, top)
      case Anchor.SouthEast => (right, bottom)
      case Anchor.SouthWest => (left, bottom)
      case Anchor.Baseline  => (left, 0.0)

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y)
    t.gsave()
    t.translate(x, y) // the baseline = the box's bottom edge, since descent is zero
    t.scale(1, -1)    // flip into picture-local y-up space

    ops.foreach {
      case PictureOp.NewPath               => t.newPath()
      case PictureOp.MoveTo(px, py)        => t.moveTo(px, py)
      case PictureOp.LineTo(px, py)        => t.lineTo(px, py)
      case PictureOp.CurveTo(a, b, c, d, e, f) => t.curveTo(a, b, c, d, e, f)
      case PictureOp.Arc(cx, cy, r, a0, a1, neg) => t.arc(cx, cy, r, a0, a1, neg)
      case PictureOp.Close                 => t.closePath()
      case PictureOp.Clip                  => t.clipPath()
      case PictureOp.GSave                 => t.gsave()
      case PictureOp.GRestore              => t.grestore()
      case PictureOp.SetLineWidth(w)       => t.setLineWidth(w)
      case PictureOp.SetDash(pattern, off) => t.setDash(pattern, off)
      case PictureOp.SetLineCap(cap)       => t.setLineCap(cap)
      case PictureOp.SetLineJoin(join)     => t.setLineJoin(join)
      case PictureOp.Translate(dx, dy)     => t.translate(dx, dy)
      case PictureOp.Scale(sx, sy)         => t.scale(sx, sy)
      case PictureOp.Rotate(rad)           => t.rotate(rad)

      case PictureOp.Paint(fill, stroke) =>
        (fill, stroke) match
          case (Some(f), Some(s)) =>
            t.setColor(f); t.fillPath(preserve = true)
            t.setColor(s); t.strokePath()
          case (Some(f), None) =>
            t.setColor(f); t.fillPath()
          case (None, Some(s)) =>
            t.setColor(s); t.strokePath()
          case (None, None) =>
            t.newPath() // nothing to paint; discard the built path

      case PictureOp.Place(placed, anchor, px, py) =>
        t.gsave()
        t.translate(px, py)
        t.scale(1, -1) // undo the picture's flip so the placed box draws upright
        val (bx, by) = anchorOffset(anchor, placed)
        placed.draw(t, bx, by)
        t.grestore()
    }

    t.grestore()
