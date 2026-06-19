# texish

A TeX-style document layout and rendering engine for Scala, cross-built for the JVM, Scala Native,
and Scala.js. Body text is set in Latin Modern Roman, with TeX-style math mode set in the matching
Latin Modern Math through an OpenType `MATH` table. Math covers inline `$…$` and centered display
`$$…$$` (with `\eqno` equation numbers): atoms and spacing, super/subscripts, fractions (`\frac` and
the infix `\over`/`\atop`), radicals (including higher roots), stretchy delimiters, accents, big
operators whose limits stack in display style, and matrices (`\matrix`, `\pmatrix`, `\bmatrix`,
`\cases`).

It breaks paragraphs into lines and lines into pages the way TeX does — Knuth-Plass line breaking,
Liang hyphenation, legal page breaks with widow/orphan control, footnotes, and glue/kern spacing in
a point-space coordinate system. Documents are written in a small TeX-like language (a `parser`
layer over the engine's primitives, with macros, a standard prelude/"format", `\hbox`/`\vbox`,
`\kern`, `\lower`/`\raise`, the `\TeX` and `\TeXish` logos, units like `pt`/`in`/`em`/`ex`, and
more). Pages render through pluggable backends — a Graphics2D raster (image) backend on the JVM and
a Cairo image-and-PDF backend on Native.

It also has a vector-graphics mode (see below) for figures drawn inline in the document — shapes,
freeform paths, transforms, and placed type — built on the same rendering pipeline as the text.

A document format (`\use{document}`) supplies the article furniture — title blocks, numbered
sections, lists, quotations, figures and tables with captions, and footnotes — and bundled packages
add clickable links and images (`\includegraphics`, `\href`/`\url` as real PDF annotations), text
sub/superscripts, chemistry (`\ce` reaction equations and skeletal structures), and data
plots (`\use{plot}` — line, scatter, bar, and function plots with labelled axes).

**Documentation: [texish.edadma.dev](https://texish.edadma.dev/).**

## Vector graphics

`\picture` opens a fixed-size drawing that flows in the text like any other box, so a diagram sits
beside prose and prints through the same backend as the page. Inside it, coordinates are **y-up with
the origin at the bottom-left** (the PostScript/TikZ convention); a bare number is a point, and unit
suffixes (`in`, `mm`, `pt`, `em`) are honoured. Shapes draw immediately in the current graphics
state, which `\group` saves and restores.

```
\picture width:3in height:2in {
  \fill{lightsteelblue} \stroke{steelblue} \linewidth{1.5pt}
  \rect{0.2in 0.2in 1in 1in}
  \circle{2in 0.9in 0.5in}
  \nofill \stroke{firebrick} \linetype{dashed}
  \line{0.2in 0.1in 2.6in 0.1in}
  \at anchor:south {1.4in 1.6in}{$y = x^2$}
}
```

The vocabulary:

- **State** (saved by `\group`): `\stroke{color}` / `\nostroke`, `\fill{color}` / `\nofill`,
  `\linewidth{d}`, `\linecap{butt|round|square}`, `\linejoin{miter|round|bevel}`, `\dash{on off …}`,
  and `\linetype{solid|dashed|dotted|dashdot}`.
- **Transforms**: `\translate{dx dy}`, `\scale{sx sy}`, `\rotate{degrees}`.
- **Shapes**: `\line`, `\rect`, `\circle`, `\ellipse`, `\polygon`, `\polyline`, `\arc`, `\arcn`.
- **Freeform paths**: `\path{ \moveto{x y} \lineto{x y} \curveto{c1x c1y c2x c2y x y} \close }`.
- **Grouping & clipping**: `\group{ … }`, `\clip{ <path body> }`.
- **Placement**: `\at[anchor:…]{x y}{content}` drops fully typeset text or math at a coordinate (it
  stays upright over the y-flip), and `\glyph[anchor:…]{x y}{codepoint}` places a single marker glyph.
  Anchors are `center`, `north`/`south`/`east`/`west`, the four corners, and `baseline`.

Coordinates can be parenthesised as well as bare: `(x, y)` is Cartesian, `(angle:radius)` is polar
(degrees), and `(name)` is a point named earlier with `\coordinate{name}{(x, y)}`. Either form may be
made **relative to the current point**: `++(dx, dy)` offsets from it and then *advances* it, so steps
chain into a shape or path — `\polygon{(10,10) ++(40,0) ++(0,40) ++(-40,0)}` walks a square — while
`+(dx, dy)` offsets without moving it, for several spokes from one hub. `\point{(x, y)}` makes a
first-class point value (for `\set`, printed by `\the` as `(x, y)`), `\xof{coord}` / `\yof{coord}`
read a point's components back as numbers, and the point operators `\padd` / `\psub` / `\pscale` /
`\pnormalize` / `\pperp` / `\pmid` / `\pdist` do vector arithmetic on points — so geometry like a bond
shortened along its own axis is written directly, e.g. `\padd{(A)}{\pscale{\pnormalize{\psub{(B)}{(A)}}}{9}}`.

Coordinates may also be computed, not just literal — a bare variable `\x` is its value and arithmetic
like `\*{\x}{14}` or `\forloop.index` works — so a plot, a chart, or a chemical diagram is just a path
built in a `\for` loop. See [`scripts/picture.script`](scripts/picture.script) for a worked demo
(shapes, a Bézier wave, a rotate-fan, a y=x² line graph, a bar chart, and a benzene ring).

## Command-line tool

The Scala Native build links a standalone `texish` executable that turns a source document into a PDF
(or one PNG per page) using the Cairo backend:

```
texish [options] [input-file]

  input-file                    texish source to typeset; reads standard input if omitted
  -o, --output <file>           output path (default: beside the input file, or out)
  -t, --type <pdf | png>        output type (default: pdf)
  -p, --paper <a4 | letter>     paper size (default: letter)
  -r, --resolution <sd|hd|fhd>  PNG device resolution (default: hd)
```

```sh
texish doc.texish -o doc -p a4          # writes doc.pdf
texish doc.texish -t png -r fhd         # writes doc.png (or doc_1.png, doc_2.png, … for multiple pages)
cat doc.texish | texish -o doc          # read the source from standard input
```

Build the binary with `sbt texishNative/nativeLink`; it is produced at
`native/target/scala-3.8.4/texish`.

## Installation

texish is cross-published for the JVM, Scala Native, and Scala.js. Add it to an sbt build with the
`%%%` operator so the right platform artifact is selected:

```scala
libraryDependencies += "io.github.edadma" %%% "texish" % "0.4.0"
```

## License

ISC. See [LICENSE](LICENSE).
