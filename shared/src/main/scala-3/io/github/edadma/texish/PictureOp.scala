package io.github.edadma.texish

/** How the ends of a stroked open path are drawn. */
enum LineCap:
  case Butt, Round, Square

object LineCap:
  def fromString(s: String): Option[LineCap] = s.toLowerCase match
    case "butt"   => Some(Butt)
    case "round"  => Some(Round)
    case "square" => Some(Square)
    case _        => None

/** How the corners of a stroked path are joined. */
enum LineJoin:
  case Miter, Round, Bevel

object LineJoin:
  def fromString(s: String): Option[LineJoin] = s.toLowerCase match
    case "miter" => Some(Miter)
    case "round" => Some(Round)
    case "bevel" => Some(Bevel)
    case _       => None

/** One entry in a picture's display list. A [[PictureMode]] collects these as the picture body parses, and a
  * [[PictureBox]] replays them in order through the drawing seam at render time. Coordinates are already
  * resolved to picture-local logical units (relative coordinates were folded against the running current point
  * during collection); the y-up→device flip and any document position are applied by [[PictureBox]] when it
  * sets up the backend transform, so the ops themselves are position-independent.
  *
  * The model is immediate-mode-with-state: a shape is lowered to a fresh path (`NewPath` + the segment ops)
  * followed by a single `Paint` carrying the fill and/or stroke colour active where the shape was issued. The
  * stroke *parameters* (width, dash, caps, joins) and the coordinate transform are backend state, set by their
  * own ops and bracketed by `GSave`/`GRestore` so a `\group` reverts them.
  */
enum PictureOp:
  /** Begin a fresh path, discarding any partly-built one. Every shape and clip region starts with this. */
  case NewPath

  case MoveTo(x: Double, y: Double)
  case LineTo(x: Double, y: Double)

  /** A cubic Bézier from the current point through control points `(c1x,c1y)` and `(c2x,c2y)` to `(x,y)`. */
  case CurveTo(c1x: Double, c1y: Double, c2x: Double, c2y: Double, x: Double, y: Double)

  /** A circular arc on the circle of radius `r` about `(cx,cy)`, from angle `a0` to `a1` (radians). `negative`
    * sweeps clockwise (decreasing angle) instead of counter-clockwise. */
  case Arc(cx: Double, cy: Double, r: Double, a0: Double, a1: Double, negative: Boolean)

  /** Close the current subpath back to its starting point. */
  case Close

  /** Paint the current path: fill it if `fill` is set, stroke it if `stroke` is set, both if both (filled
    * first, then stroked over). Each colour is the one active in the picture's graphics state at the point the
    * owning shape was issued. */
  case Paint(fill: Option[Color], stroke: Option[Color])

  /** Intersect the clip region with the current path, for the rest of the enclosing `GSave`/`GRestore`. */
  case Clip

  case GSave
  case GRestore

  case SetLineWidth(width: Double)

  /** A dash pattern as alternating on/off lengths with a starting phase offset; an empty pattern is solid. */
  case SetDash(pattern: Vector[Double], offset: Double)

  case SetLineCap(cap: LineCap)
  case SetLineJoin(join: LineJoin)

  case Translate(dx: Double, dy: Double)
  case Scale(sx: Double, sy: Double)

  /** Rotate the coordinate system by `radians` (counter-clockwise in the picture's y-up space). */
  case Rotate(radians: Double)

  /** Place an already-laid-out box at `(x,y)` with the given anchor. The box draws upright (the picture's y-up
    * flip is undone for it), so embedded text, math, and raw glyphs render the right way up. A raw `\glyph` is
    * just a [[GlyphBox]] placed this way. */
  case Place(box: Box, anchor: Anchor, x: Double, y: Double)
