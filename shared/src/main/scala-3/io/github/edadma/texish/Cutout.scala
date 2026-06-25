package io.github.edadma.texish

/** Which margin a cutout hugs: text is pushed away from that side. */
enum Side:
  case Left, Right

/** A zero-size, invisible marker placed in the galley to anchor a bare `\cutout`. A wrapped figure anchors its
  * cutout to the [[OverlayBox]] it already contributes; a bare cutout has no such box, so it drops one of these in
  * at the cutout point to mark the same position. Being a control box, it occupies no space and interline-glue
  * insertion looks straight through it.
  */
class CutoutMarker extends ControlBox:
  val isSpace: Boolean = false

/** A region of the galley that running text flows around — the geometry behind a wrapped figure. The region is
  * anchored to a box in the galley (the figure's [[OverlayBox]], or a [[CutoutMarker]] for a bare `\cutout`) rather
  * than to a fixed coordinate: its top is wherever that anchor currently sits, so the band stays aligned with the
  * figure as glue is inserted around it and as a page break carries the figure to the next page. `height` is how
  * far the region reaches below the anchor and `profile` how far text is pushed in from `side`.
  *
  * Resolving the top from the live galley each time, rather than freezing it when the cutout is registered, is what
  * keeps the wrap correct across a page boundary: a frozen coordinate drifts from the figure the moment a fresh
  * topskip is padded above it on the new page, and the wrong lines get narrowed.
  *
  * The per-line measure consults the active cutouts by vertical band rather than by line number, so a single cutout
  * narrows exactly the lines it physically overlaps and a tall one keeps narrowing into the paragraphs that follow
  * it — the wrap composes across paragraph boundaries because the cutout lives in the galley, not in any one
  * paragraph.
  *
  * `profile` maps a depth below the resolved top — a value in `[0, height]` — to the inset text keeps from `side`
  * at that depth, so text can hug a non-rectangular silhouette rather than the figure's bounding box. A rectangle
  * is the constant case ([[Cutout.rect]]); a circle, wedge or diamond varies the inset with depth (see
  * [[CutoutShape]]). The top of the region is not stored here — it is resolved from `anchor`'s live position in the
  * galley each time the band is needed (see `VerticalMode.cutoutBand`), so the band tracks the figure across a page
  * break.
  */
case class Cutout(anchor: Box, height: Double, side: Side, profile: Double => Double)

object Cutout:
  /** A rectangular cutout: the same inset at every depth, the degenerate case of a shaped profile. */
  def rect(inset: Double): Double => Double = _ => inset

/** The silhouettes a wrapped figure's text can follow, beyond the default rectangle. `profile` turns a shape and the
  * figure's measure into a depth → inset function: `extent` is the figure's width on the wrapped side and `height`
  * its full height, `gutter` the `\wrapsep` whitespace kept between the silhouette and the text. The result is the
  * inset at each depth below the figure's top, so text follows the outline — narrowing most where the shape is
  * widest and relaxing toward the margin where it tapers — rather than reserving the whole bounding box.
  *
  * Each shape is taken to be inscribed in the `extent` × `height` box and tangent to the wrapped margin, and the
  * gutter is added uniformly so the text column keeps a clean edge a fixed distance off the silhouette.
  */
enum CutoutShape:
  case Rectangle, Ellipse, TriangleUp, TriangleDown, Diamond

object CutoutShape:
  /** Parse a shape name; anything unrecognised (including an empty argument) is a plain rectangle. */
  def named(name: String): CutoutShape = name.trim.toLowerCase match
    case "ellipse" | "circle" | "oval"      => Ellipse
    case "triangleup" | "up"                => TriangleUp
    case "triangledown" | "down" | "wedge"  => TriangleDown
    case "diamond"                          => Diamond
    case _                                  => Rectangle

  /** The depth → inset profile for `shape` inscribed in an `extent` × `height` box, with `gutter` added at every
    * depth. Depth runs from 0 at the figure's top to `height` at its foot. A zero height degenerates to the
    * rectangle so the profile never divides by zero.
    */
  def profile(shape: CutoutShape, extent: Double, height: Double, gutter: Double): Double => Double =
    if height <= 0.0 then return _ => extent + gutter
    shape match
      case Rectangle => _ => extent + gutter
      case Ellipse =>
        // half-width times the unit ellipse's far edge at this depth: widest (full extent) at the middle, tapering
        // to the half-width at top and bottom, so text curves in around a disc and back out again
        val a = extent / 2
        val b = height / 2
        d =>
          val t = (d - b) / b
          a * (1.0 + math.sqrt(math.max(0.0, 1.0 - t * t))) + gutter
      case TriangleDown =>
        // wide at the top, tapering to a point at the foot: text wraps a downward wedge
        d => extent * math.max(0.0, 1.0 - d / height) + gutter
      case TriangleUp =>
        // a point at the top, widening to the foot
        d => extent * math.max(0.0, d / height) + gutter
      case Diamond =>
        // widest at the middle, a point at top and foot
        val b = height / 2
        d => extent * math.max(0.0, 1.0 - math.abs(d - b) / b) + gutter
