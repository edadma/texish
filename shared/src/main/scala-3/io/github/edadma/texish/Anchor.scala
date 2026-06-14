package io.github.edadma.texish

/** Which point of a placed box or glyph is pinned to a coordinate inside a picture. With a box spanning
  * `[x, x+width]` horizontally and `[baseline-descent, baseline+ascent]` vertically, the anchor names the
  * handle that lands on the given point — so `North` hangs the box below the point, `West` puts the point at
  * the box's left edge (vertically centred), and so on. `Baseline` is the special text handle: the left end
  * of the box's baseline, the natural attach point for a label or a raw glyph.
  */
enum Anchor:
  case Center
  case North, South, East, West
  case NorthEast, NorthWest, SouthEast, SouthWest
  case Baseline

object Anchor:
  /** Parse an anchor name as written in a `\at`/`\glyph` option, e.g. `north`, `south-east`, `baseline`.
    * Returns `None` for an unrecognized name so the caller can report it at the right source position. */
  def fromString(s: String): Option[Anchor] = s.toLowerCase match
    case "center" | "centre"          => Some(Center)
    case "north"                      => Some(North)
    case "south"                      => Some(South)
    case "east"                       => Some(East)
    case "west"                       => Some(West)
    case "northeast" | "north-east"   => Some(NorthEast)
    case "northwest" | "north-west"   => Some(NorthWest)
    case "southeast" | "south-east"   => Some(SouthEast)
    case "southwest" | "south-west"   => Some(SouthWest)
    case "baseline"                   => Some(Baseline)
    case _                            => None
