---
title: "Figures and Images"
weight: 5
---

## Placing an image

`\includegraphics` places a raster image — PNG or JPEG — at its natural size, or sized by
an optional `key=value` list:

```texish
\includegraphics{frog.jpg}
\includegraphics[width=200pt]{frog.jpg}
\includegraphics[width=0.4\linewidth]{diagram.png}
\includegraphics[scale=0.5]{photo.jpg}
\includegraphics[width=120pt,height=80pt]{logo.png}
```

- `width` and `height` are lengths — a dimension like `200pt`, `5cm`, `0.5in`, or a factor
  times `\linewidth` / `\textwidth` (the current line width).
- Giving only one of `width` or `height` scales the other to preserve the aspect ratio.
- `scale` multiplies the natural size; with no options the image keeps its pixel size.

A relative path is resolved against the directory of the document being rendered, so
`frog.jpg` finds the image beside the source file.

## Centering

`\centerline` centers a single line — the simplest way to center an image:

```texish
\centerline{\includegraphics[width=0.4\linewidth]{frog.jpg}}
```

`\centering` switches the rest of the current group to centered lines (so a wrapped
paragraph or a multi-line block centers); use it inside a group or a figure body so it
reverts at the close. `\leftline` and `\rightline` are the flush-left and flush-right
counterparts of `\centerline`.

## Figures and tables with captions

`\figure` and `\table` detach their body to a page edge and number it on its own counter; a
`\caption` written inside reads "Figure 1: …" or "Table 1: …".

```texish
\figure[h]{
  \centerline{\includegraphics[width=0.35\linewidth]{frog.jpg}}
  \vskip 6pt
  \centerline{\caption{A frog, scaled to a third of the text width.}}
}
```

An optional placement specifier — `[h]` here, `[t]` top, `[b]` bottom — follows the
command. `[h]` keeps the float in place if it fits; the default is top.

## Tabular material

`\tabular` sets a table from a column spec (`l`/`c`/`r` for alignment, `|` for a vertical
rule); rows are separated by `\\` and a horizontal rule by `\hline`:

```texish
\tabular{|l|c|r|}{
  \hline
  Item & Qty & Price \\
  \hline
  Widget & 3 & 1.50 \\
  Gadget & 1 & 9.99 \\
  \hline
}
```
