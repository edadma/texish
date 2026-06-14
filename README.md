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

Full documentation is coming — a dedicated docs site will be linked here once it's published.

## Command-line tool

The Scala Native build links a standalone `texish` executable that turns a source document into a PDF
(or one PNG per page) using the Cairo backend:

```
texish [options] [input-file]

  input-file                    texish source to typeset; reads standard input if omitted
  -o, --output <file>           output path (default: out, or the input file's base name)
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
libraryDependencies += "io.github.edadma" %%% "texish" % "0.3.0"
```

## License

ISC. See [LICENSE](LICENSE).
