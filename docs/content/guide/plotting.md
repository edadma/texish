---
title: "Plotting"
weight: 7
---

The `plot` package draws 2-D data plots — line graphs, scatter plots, bar charts, and
function curves — with labelled axes, tick marks, and a grid. Load it with:

```
\use{plot}
```

A plot is set up by declaring the data ranges, then drawn by `\plot`, whose body holds
one or more data series:

```
\xrange{0}{6}
\yrange{0}{130}
\xlabel{time (s)}
\ylabel{height (m)}
\plottitle{Projectile}
\plot{
  \lineplot{royalblue}{0 0  1 50  2 88  3 112  4 124  5 126  6 118}
  \scatter{crimson}{1 50  3 112  5 126}
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
| `\xrange{min}{max}` | the x data range (required) |
| `\yrange{min}{max}` | the y data range (required) |
| `\xlabel{text}` / `\ylabel{text}` | axis labels (the y label is set vertically) |
| `\plottitle{text}` | a title centred over the plot |
| `\xstep{s}` / `\ystep{s}` | force a tick step (otherwise a *nice* step is chosen) |

The ranges fix the mapping from data coordinates to the page; points outside them simply
fall outside the data area. Tick steps default to a *nice* round value (a 1, 2, or 5
times a power of ten) chosen to give about five intervals; set `\xstep`/`\ystep` to pin a
specific step. Tick labels are rounded so an accumulated tick value never shows
floating-point noise. Each `\plot` is self-contained — set its labels and ranges just
before it.

## Data series

Every series reads a flat `x1 y1 x2 y2 …` list of numbers.

| Series | Draws |
|---|---|
| `\lineplot{colour}{data}` | a polyline through the data points |
| `\scatter{colour}{data}` | a filled marker at each data point |
| `\bars{colour}{data}` | a vertical bar from the axis up to each point |
| `\fnplot{colour}{expression}` | a sampled curve of a function of `x` |

A `\plot` body may hold several series, drawn in order on top of the frame:

```
\xrange{0}{7}
\yrange{0}{160}
\plottitle{Quarterly revenue}
\plot{
  \bars{teal}{1 95  2 110  3 80  4 140  5 120  6 150}
}
```

`\fnplot` takes an expression instead of a data list. It samples the expression across
the domain and strokes the result; the expression is ordinary `\calc` arithmetic written
in terms of the variable `x`, so any function the [expression evaluator](/guide/mathematics/)
knows can be drawn:

```
\xrange{-3}{3}
\yrange{0}{9}
\plottitle{$y = x^2$}
\plot{
  \fnplot{seagreen}{x*x}
}
```

Two curves on one plot, with the tick steps pinned:

```
\xrange{0}{6.283}
\yrange{-1.2}{1.2}
\xstep{1}
\ystep{0.5}
\plot{
  \fnplot{darkorange}{sin(x)}
  \fnplot{mediumvioletred}{cos(x)}
}
```

## Appearance

The look is controlled by variables you can `\set` after `\use{plot}` and before `\plot`:

| Variable | Default | Meaning |
|---|---|---|
| `plotareaw` / `plotareah` | `234` / `162` | the data area's width and height, in points |
| `plotgrid` | `1` | draw a light grid behind the data (`0` to omit) |
| `plotsamples` | `80` | samples across the domain for `\fnplot` |
| `plotmarkr` | `2.4` | marker radius for `\scatter` |
| `plotbarfrac` | `0.6` | bar width as a fraction of the x step |
| `plotaxiscolor` / `plotgridcolor` | `dimgray` / `gainsboro` | axis and grid colours |
| `plottickdec` | `3` | most decimals shown in a tick label |

Colours — for the series, the axes, and the grid — are the same
[named colours or `#rrggbb` codes](/guide/graphics/) the picture layer uses.

A complete, rendered demonstration of all four series types lives at
`scripts/plot.script` in the repository.
