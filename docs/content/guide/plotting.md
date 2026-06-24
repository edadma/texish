---
title: "Plotting"
weight: 7
---

The `plot` package draws 2-D data plots — line graphs, scatter plots, bar charts, and
function curves — with labelled axes, tick marks, a grid, a legend, and reference lines.
Load it with:

```texish
\use{plot}
```

A plot is set up by declaring the data ranges (or letting `\autorange` derive them), then
drawn by `\plot`, whose body holds one or more data series:

```texish
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
| `\xcategories{A B C …}` | name the x ticks instead of numbering them |
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

```texish
\autorange{1 12  2 19  3 15  4 27  5 22  6 31  7 28}
\plot{ \lineplot{1 12  2 19  3 15  4 27  5 22  6 31  7 28} }
```

When a range straddles zero, the package draws a light axis line at zero (set
`plotzeroaxis` to `0` to omit it).

For non-numeric categories, `\xcategories` names the ticks instead of numbering them. The
categories sit at `x = 1, 2, 3, …`, so a series (bars especially) plotted at those integer
positions lines up under the labels:

```texish
\xcategories{Q1 Q2 Q3 Q4}
\plot{ \bars[teal]{1 95  2 130  3 80  4 150} }
```

## Data series

Every series takes two optional bracket arguments before its data — a **colour** and a
**legend label** — then the data, a flat `x1 y1 x2 y2 …` list:

| Series | Draws |
|---|---|
| `\lineplot[colour][label]{data}` | a polyline through the data points |
| `\scatter[colour][label]{data}` | a marker at each data point |
| `\bars[colour][label]{data}` | a vertical bar from the axis up to each point |
| `\areaplot[colour][label]{data}` | the band under the curve, filled to the baseline |
| `\stepplot[colour][label]{data}` | a staircase that holds each value to the next x |
| `\fnplot[colour][label]{expression}` | a sampled curve of a function of `x` |
| `\bubble[colour][label]{x y size …}` | a translucent disc at each point, sized by a third value |
| `\errorbars[colour][label]{x y err …}` | a capped whisker of y ± err at each point |
| `\trendline[colour][label]{data}` | the least-squares line of best fit, drawn dashed |

Both brackets are optional. With no colour (or an empty `[]`), a series takes its colour
from a palette in turn, so several series are automatically distinct:

```texish
\plot{
  \lineplot{0 0  1 1  2 3  3 6}      % palette colour 1
  \lineplot{0 1  1 2  2 2  3 4}      % palette colour 2
}
```

Give a label to add a legend entry (see below). `\fnplot` takes an expression instead of
a data list: it samples the expression across the domain, written in terms of the
variable `x`, so any function the [expression evaluator](/guide/mathematics/) knows can be
drawn:

```texish
\xrange{-3}{3}  \yrange{0}{9}  \plottitle{$y = x^2$}
\plot{ \fnplot[seagreen]{x*x} }
```

Scatter markers are circles by default; set `plotmarkshape` to `square`, `triangle`, or
`diamond` (it can change between series). Set `plotvalues` to `1` to print each bar's
value above it.

By default the series body is clipped to the data area, so a curve or marker that runs
past the range does not spill into the margins; set `plotclip` to `0` to allow overflow.

## Error bars and trend lines

`\errorbars` reads `x y err` triples and draws a capped vertical whisker spanning `y ± err`
at each point — usually over a `\scatter` of the same points. `\trendline` fits the
least-squares line through `x y` data and draws it dashed across the range:

```texish
\plot{
  \scatter[steelblue][mean]{1 8  3 17  5 24  7 39  9 47}
  \errorbars[steelblue]{1 8 3  3 17 5  5 24 4  7 39 6  9 47 5}
  \trendline[firebrick][fit]{1 8  3 17  5 24  7 39  9 47}
  \legend[se]
}
```

## Bubble charts

`\bubble` is a scatter with a third dimension: each point is an `x y size` triple, and the
*area* of the disc drawn at the point encodes the size (area, not radius, so the eye reads
the value honestly). The discs are translucent, so overlapping points and a second bubble
series stay legible:

```texish
\plot{
  \bubble[royalblue][premium]{2 8 12  4 7 30  6 9 8  8 6 22}
  \bubble[darkorange][budget]{3 4 18  5 3 9  7 5 25  9 2 14}
  \legend[sw]
}
```

The size values are mapped onto a radius between `plotbubblemin` and `plotbubblemax`, so
the largest point in the series gets the largest disc; set `plotbubbleopacity` to control
how much overlapping bubbles show through. Translucency comes from the picture layer's new
`\opacity` (and `\fillopacity` / `\strokeopacity`) state, available to any drawing.

## The legend

A series given a label records a legend entry. `\legend[pos]` draws the key — a colour
swatch (a line, the series' marker, or a filled square according to the series kind)
beside each label, on a light background sized to the labels — in a corner of the data
area: `ne` (default), `nw`, `se`, or `sw`.

```texish
\plot{
  \bars[teal][actual]{1 95  2 110  3 80  4 140}
  \lineplot[crimson][target]{1 100  4 100}
  \legend[ne]
}
```

## Reference lines

`\hline[colour]{y}` and `\vline[colour]{x}` draw a dashed line across the plot at a data
value — a threshold, a target, a mean. With no colour they use `plotreflcolor`:

```texish
\plot{
  \bars[teal]{1 95  2 110  3 80  4 140  5 120  6 150}
  \hline[crimson]{115.8}
}
```

## Minor ticks and tick formatting

`plotxminor` / `plotyminor` set the number of subdivisions per major interval; when
positive, minor tick marks (and, with `plotminorgrid`, lighter grid lines) appear between
the majors. `\xtickformat{prefix}{suffix}` and `\ytickformat{prefix}{suffix}` wrap each
numeric tick label, so currency or units read naturally (a literal `$` is `\$`, a literal
`%` is `\%`):

```texish
\set plotyminor {5}
\xtickformat{}{\%}
\ytickformat{\$}{}
\plot{ \lineplot[seagreen]{0 5  20 9  40 16  60 25  80 38  100 47} }
```

Like the labels, the tick format resets after each `\plot`.

## Appearance

The look is controlled by variables you can `\set` after `\use{plot}` and before `\plot`:

| Variable | Default | Meaning |
|---|---|---|
| `plotareaw` / `plotareah` | `234` / `162` | the data area's width and height, in points |
| `plotgrid` | `1` | draw a light grid behind the data (`0` to omit) |
| `plotclip` | `1` | clip the series to the data area (`0` to allow overflow) |
| `plotzeroaxis` | `1` | draw an axis line at zero when a range straddles it |
| `plotvalues` | `0` | print each bar's value above it (`\bars`) |
| `plotmarkshape` | `circle` | scatter marker: `circle`, `square`, `triangle`, `diamond` |
| `plotmarkr` | `2.6` | marker radius for `\scatter` |
| `plotbubblemin` / `plotbubblemax` | `3` / `18` | smallest / largest bubble radius, in points (`\bubble`) |
| `plotbubbleopacity` | `0.55` | bubble fill opacity, so overlaps show through |
| `plotsamples` | `80` | samples across the domain for `\fnplot` |
| `plotbarfrac` | `0.6` | bar width as a fraction of the x step |
| `plotxminor` / `plotyminor` | `0` | minor subdivisions per major interval (0 = none) |
| `ploterrcap` | `3` | half-width of an error-bar cap, in points |
| `plotaxiscolor` / `plotgridcolor` | `dimgray` / `gainsboro` | axis and grid colours |
| `plottickdec` | `3` | most decimals shown in a tick label |

The auto colour cycle is the index-keyed map `plotPalette` (eight colours by default);
`\set` its entries or change `plotPaletteLen` to recolour. Colours — for series, axes,
grid, and palette — are the same [named colours or `#rrggbb` codes](/guide/graphics/) the
picture layer uses.

A complete, rendered demonstration of every series type, the legend, reference lines,
marker shapes, auto colour, and auto-ranging lives at `scripts/plot-demo.script` in the
repository.
