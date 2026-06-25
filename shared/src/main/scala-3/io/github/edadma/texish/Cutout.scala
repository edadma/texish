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
  * far the region reaches below the anchor and `inset` how far text is pushed in from `side`.
  *
  * Resolving the top from the live galley each time, rather than freezing it when the cutout is registered, is what
  * keeps the wrap correct across a page boundary: a frozen coordinate drifts from the figure the moment a fresh
  * topskip is padded above it on the new page, and the wrong lines get narrowed.
  *
  * The per-line measure consults the active cutouts by vertical band rather than by line number, so a single cutout
  * narrows exactly the lines it physically overlaps and a tall one keeps narrowing into the paragraphs that follow
  * it — the wrap composes across paragraph boundaries because the cutout lives in the galley, not in any one
  * paragraph. The region is treated as a solid rectangle here; the richer shaped-wrap case generalises `inset` to a
  * function of the band's position, of which this constant is the degenerate case.
  *
  * The top of the region is not stored here — it is resolved from `anchor`'s live position in the galley each time
  * the band is needed (see `VerticalMode.cutoutBand`), so the band tracks the figure across a page break. `height`
  * is how far the region reaches below that resolved top, and a line is narrowed when its band overlaps the region.
  */
case class Cutout(anchor: Box, height: Double, side: Side, inset: Double)
