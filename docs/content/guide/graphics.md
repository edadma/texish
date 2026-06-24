---
title: "Vector Graphics"
weight: 5
---

`\picture` opens a fixed-size drawing that flows in the text like any other box, so a
diagram sits beside prose and prints through the same backend as the page. Inside it,
coordinates are **y-up with the origin at the bottom-left** (the PostScript/TikZ
convention); a bare number is a point, and unit suffixes (`in`, `mm`, `pt`, `em`) are
honoured. Shapes draw immediately in the current graphics state, which `\group` saves and
restores.

```texish
\picture width:3in height:2in {
  \fill{lightsteelblue} \stroke{steelblue} \linewidth{1.5pt}
  \rect{0.2in 0.2in 1in 1in}
  \circle{2in 0.9in 0.5in}
  \nofill \stroke{firebrick} \linetype{dashed}
  \line{0.2in 0.1in 2.6in 0.1in}
  \at anchor:south {1.4in 1.6in}{$y = x^2$}
}
```

## The vocabulary

- **State** (saved by `\group`): `\stroke{color}` / `\nostroke`, `\fill{color}` /
  `\nofill`, `\opacity{v}` (or `\fillopacity{v}` / `\strokeopacity{v}`, a `0`–`1`
  multiplier on the paint's alpha, so translucent shapes show through one another),
  `\linewidth{d}`, `\linecap{butt|round|square}`,
  `\linejoin{miter|round|bevel}`, `\dash{on off …}`, and
  `\linetype{solid|dashed|dotted|dashdot}`.
- **Transforms**: `\translate{dx dy}`, `\scale{sx sy}`, `\rotate{degrees}`.
- **Shapes**: `\line`, `\rect`, `\circle`, `\ellipse`, `\polygon`, `\polyline`, `\arc`,
  `\arcn`.
- **Freeform paths**: `\path{ \moveto{x y} \lineto{x y} \curveto{c1x c1y c2x c2y x y}
  \close }`.
- **Grouping and clipping**: `\group{ … }`, `\clip{ <path body> }`.
- **Placement**: `\at[anchor:…]{x y}{content}` drops fully typeset text or math at a
  coordinate (it stays upright over the y-flip), and `\glyph[anchor:…]{x y}{codepoint}`
  places a single marker glyph. Anchors are `center`, `north`/`south`/`east`/`west`, the
  four corners, and `baseline`.

## Computed coordinates

Coordinates may be computed, not just literal — a bare variable `\x` is its value and
arithmetic like `\*{\x}{14}` works — so a plot, a chart, or a chemical diagram is just a
path built in a `\for` loop. See the picture section of `scripts/texish-demo.script` in the
repository for a worked demo: shapes, a Bézier wave, a *y = x²* line graph, a bar chart, and a
benzene ring.
