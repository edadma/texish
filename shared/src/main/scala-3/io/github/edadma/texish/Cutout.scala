package io.github.edadma.texish

/** Which margin a cutout hugs: text is pushed away from that side. */
enum Side:
  case Left, Right

/** A rectangular region of the galley that running text must flow around — the geometry behind a wrapped figure.
  * `yTop`/`yBottom` are vertical positions in the galley (the enclosing vertical list) measured from its top, and
  * `inset` is how far text is pushed in from `side` for any line whose vertical band falls within the region.
  *
  * The per-line measure consults the active cutouts by vertical band rather than by line number, so a single
  * cutout narrows exactly the lines it physically overlaps and a tall one keeps narrowing into the paragraphs
  * that follow it — the wrap composes across paragraph boundaries because the cutout lives in the galley, not in
  * any one paragraph.
  *
  * The region is treated as a solid rectangle here; the richer shaped-wrap case generalises `inset` to a function
  * of the band's position, of which this constant is the degenerate case.
  */
case class Cutout(yTop: Double, yBottom: Double, side: Side, inset: Double):

  /** The (left, right) horizontal inset this cutout imposes on a line occupying the vertical band `[y0, y1]`. A
    * line contributes the full inset if its band overlaps the region at all (conservative band sampling, so text
    * never intrudes on the figure), and nothing once it has cleared the region.
    */
  def insetsOver(y0: Double, y1: Double): (Double, Double) =
    if y1 > yTop && y0 < yBottom then
      side match
        case Side.Left  => (inset, 0.0)
        case Side.Right => (0.0, inset)
    else (0.0, 0.0)
