---
title: "Plotting"
weight: 7
---

The `plot` package draws 2-D data plots — line graphs, scatter plots, bar charts, and
function curves — with labelled axes, tick marks, a grid, a legend, and reference lines.
Load it with:

```
\use{plot}
```

A plot is set up by declaring the data ranges (or letting `\autorange` derive them), then
drawn by `\plot`, whose body holds one or more data series:

```
\xrange{0}{6}
\yrange{0}{130}
\xlabel{time (s)}
\ylabel{height (m)}
\plottitle{Projectile}
\plot{
  \lineplot[royalblue][trajectory]{0 0  1 50  2 88  3 112  4 124  5 126  6 118}
  \scatter[crimson][samples]{1 50  3 112  5 126}
  \legend[se]
}
```

`\plot` opens a `\picture` sized to hold the data area plus margins for the axes and
labels. It draws the frame — grid, axes, tick marks with numeric labels, axis labels and
the title — then runs its body to draw the series on top. The result is an ordinary box
that flows in the text like any figure, so a plot can sit inline with prose, in a figure
float, or beside a table.

Like the rest of texish, the package is written entirely in the document language over
the [vector-graphics layer](/guide/graphics/): `\calc` does the data-to-device
arithmetic, `\seq`/`\head`/`\tail` walk the data points, `\for` steps the ticks, and the
series are `\picture` paths and shapes. There is no plot-specific engine primitive.

## Ranges and labels

| Command | Effect |
|---|---|
| `\xrange{min}{max}` | the x data range |
| `\yrange{min}{max}` | the y data range |
| `\autorange{x y x y …}` | derive both ranges from the data |
| `\xlabel{text}` / `\ylabel{text}` | axis labels (the y label is set vertically) |
| `\plottitle{text}` | a title centred over the plot |
| `\xstep{s}` / `\ystep{s}` | force a tick step (otherwise a *nice* step is chosen) |

The ranges fix the mapping from data coordinates to the page; points outside them simply
fall outside the data area. Tick steps default to a *nice* round value (a 1, 2, or 5
times a power of ten) chosen to give about five intervals; set `\xstep`/`\ystep` to pin a
specific step. Tick labels are rounded so an accumulated tick value never shows
floating-point noise. **Each `\plot` is self-contained**: the labels, title, and any
forced steps are cleared afterwards, so set what you want fresh before each plot.

`\autorange` scans a flat data list and chooses the ranges for you — x spans the data
exactly, and y is padded slightly and rested on a zero baseline when the data is
non-negative:

```
\autorange{1 12  2 19  3 15  4 27  5 22  6 31  7 28}
\plot{ \lineplot{1 12  2 19  3 15  4 27  5 22  6 31  7 28} }
```

When a range straddles zero, the package draws a light axis line at zero (set
`plotzeroaxis` to `0` to omit it).

## Data series

Every series takes two optional bracket arguments before its data — a **colour** and a
**legend label** — then the data, a flat `x1 y1 x2 y2 …` list:

| Series | Draws |
|---|---|
| `\lineplot[colour][label]{data}` | a polyline through the data points |
| `\scatter[colour][label]{data}` | a marker at each data point |
| `\bars[colour][label]{data}` | a vertical bar from the axis up to each point |
| `\fnplot[colour][label]{expression}` | a sampled curve of a function of `x` |

Both brackets are optional. With no colour (or an empty `[]`), a series takes its colour
from a palette in turn, so several series are automatically distinct:

```
\plot{
  \lineplot{0 0  1 1  2 3  3 6}      % palette colour 1
  \lineplot{0 1  1 2  2 2  3 4}      % palette colour 2
}
```

Give a label to add a legend entry (see below). `\fnplot` takes an expression instead of
a data list: it samples the expression across the domain, written in terms of the
variable `x`, so any function the [expression evaluator](/guide/mathematics/) knows can be
drawn:

```
\xrange{-3}{3}  \yrange{0}{9}  \plottitle{$y = x^2$}
\plot{ \fnplot[seagreen]{x*x} }
```

Scatter markers are circles by default; set `plotmarkshape` to `square`, `triangle`, or
`diamond` (it can change between series).

## The legend

A series given a label records a legend entry. `\legend[pos]` draws the key — a colour
swatch (a line, the series' marker, or a filled square according to the series kind)
beside each label, on a light background — in a corner of the data area: `ne` (default),
`nw`, `se`, or `sw`.

```
\plot{
  \bars[teal][actual]{1 95  2 110  3 80  4 140}
  \lineplot[crimson][target]{1 100  4 100}
  \legend[ne]
}
```

## Reference lines

`\hline[colour]{y}` and `\vline[colour]{x}` draw a dashed line across the plot at a data
value — a threshold, a target, a mean. With no colour they use `plotreflcolor`:

```
\plot{
  \bars[teal]{1 95  2 110  3 80  4 140  5 120  6 150}
  \hline[crimson]{115.8}
}
```

## Appearance

The look is controlled by variables you can `\set` after `\use{plot}` and before `\plot`:

| Variable | Default | Meaning |
|---|---|---|
| `plotareaw` / `plotareah` | `234` / `162` | the data area's width and height, in points |
| `plotgrid` | `1` | draw a light grid behind the data (`0` to omit) |
| `plotzeroaxis` | `1` | draw an axis line at zero when a range straddles it |
| `plotmarkshape` | `circle` | scatter marker: `circle`, `square`, `triangle`, `diamond` |
| `plotmarkr` | `2.6` | marker radius for `\scatter` |
| `plotsamples` | `80` | samples across the domain for `\fnplot` |
| `plotbarfrac` | `0.6` | bar width as a fraction of the x step |
| `plotaxiscolor` / `plotgridcolor` | `dimgray` / `gainsboro` | axis and grid colours |
| `plottickdec` | `3` | most decimals shown in a tick label |

The auto colour cycle is the index-keyed map `plotPalette` (eight colours by default);
`\set` its entries or change `plotPaletteLen` to recolour. Colours — for series, axes,
grid, and palette — are the same [named colours or `#rrggbb` codes](/guide/graphics/) the
picture layer uses.

A complete, rendered demonstration of every series type, the legend, reference lines,
marker shapes, auto colour, and auto-ranging lives at `scripts/plot.script` in the
repository.
