package io.github.edadma.texish.opentype

/** One segment of a glyph outline, in em (design units divided by the font's unitsPerEm) and y-up, the
  * orientation the font stores its outlines in. Both outline readers — TrueType (`glyf`) and PostScript
  * (`CFF `) — produce this one ADT, so the SVG backend has a single path representation to fill. Curves are
  * cubic Béziers; TrueType's quadratics are elevated to cubic on the way out so there is only one curve case.
  */
enum PathSeg:
  case MoveTo(x: Double, y: Double)
  case LineTo(x: Double, y: Double)
  case CurveTo(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double)
  case Close
