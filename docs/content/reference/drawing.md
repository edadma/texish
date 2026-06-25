---
title: "Drawing Primitives"
weight: 4
---

`\picture width:<dim> height:<dim> {…}` opens a vector-graphics drawing of the given size and
adds it to the current list. Inside it the primitives below draw shapes, paths, and text in a
coordinate system whose origin is the bottom-left corner, in points unless a unit is given. State
(colour, line style, transform) is scoped — `\group` and the drawing's own braces save and restore
it. See the [graphics guide](/guide/graphics/) for a tutorial.

## Coordinates and points

| Command | Effect |
|---------|--------|
| `\point{coord}` | evaluate a coordinate to a first-class point value |
| `\coordinate{name}{coord}` | name a point for later reference |
| `\xof{coord}` `\yof{coord}` | the x / y component of a coordinate, as a number |
| `\padd{a}{b}` `\psub{a}{b}` | vector sum `a + b` / vector from `b` to `a` |
| `\pmid{a}{b}` `\pdist{a}{b}` | midpoint of / distance between two points |
| `\pscale{p}{factor}` | scale a point by a scalar |
| `\pperp{p}` `\pnormalize{p}` | quarter-turn counter-clockwise / unit vector |

## Strokes, fills, and opacity

| Command | Effect |
|---------|--------|
| `\stroke{color}` `\fill{color}` | set the stroke / fill colour |
| `\nostroke` `\nofill` | disable the stroke / fill |
| `\opacity{v}` | set fill and stroke opacity together (0–1) |
| `\fillopacity{v}` `\strokeopacity{v}` | set fill / stroke opacity individually |

## Line style

| Command | Effect |
|---------|--------|
| `\linewidth{d}` | set the line width |
| `\linecap{butt\|round\|square}` | set the line-end cap |
| `\linejoin{miter\|round\|bevel}` | set the corner join |
| `\dash{pattern}` | set an explicit dash pattern |
| `\linetype{solid\|dashed\|dotted\|dashdot}` | set a named line style |

## Shapes

| Command | Effect |
|---------|--------|
| `\line{x1 y1 x2 y2}` | a straight line between two points |
| `\rect{x y width height}` | a rectangle |
| `\circle{x y radius}` | a circle |
| `\ellipse{x y rx ry}` | an axis-aligned ellipse |
| `\polygon{x1 y1 x2 y2 …}` | a closed polygon |
| `\polyline{x1 y1 x2 y2 …}` | an open polyline |
| `\arc{x y radius start end}` | an arc, counter-clockwise (degrees) |
| `\arcn{x y radius start end}` | an arc, clockwise |

## Paths

`\path` builds an arbitrary outline from move/line/curve segments; the segment commands are valid
only inside its body.

| Command | Effect |
|---------|--------|
| `\path[arrow:…]{body}` | build a path from the segment commands in `body` |
| `\moveto{x y}` | start a new subpath at a point |
| `\lineto{x y}` | straight segment to a point |
| `\curveto{x1 y1 x2 y2 x3 y3}` | cubic Bézier through two controls to an end point |
| `\close` | close the current subpath |

## Transforms, grouping, and clipping

| Command | Effect |
|---------|--------|
| `\translate{x y}` `\scale{x y}` `\rotate{deg}` | transform the coordinate system |
| `\group{body}` | save the graphics state, draw the body, restore it |
| `\clip{body}` | intersect the clip region with the body's path |

## Text and glyphs

| Command | Effect |
|---------|--------|
| `\at[anchor:center]{x y}{content}` | place typeset content at a coordinate |
| `\glyph[anchor:baseline]{x y}{codepoint}` | place one glyph of the current font by codepoint |
| `\fontglyph[anchor:baseline]{x y}{face}{size}{cp}` | place a glyph from a named typeface at a size |
| `\glyphwidth{face}{size}{cp}` | the inked width of a glyph, in points, as a number for `\set` / `\calc` |

## Arrows

| Command | Effect |
|---------|--------|
| `\arrow[head:stealth size:7 heads:end]{x1 y1 x2 y2}` | a shaft with an arrowhead, shortened at the head end |
| `\arrowhead[head:stealth size:7]{x1 y1 x2 y2}` | just an arrowhead at the second point, pointing away from the first |
