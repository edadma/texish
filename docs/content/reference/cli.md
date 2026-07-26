---
title: "Command-Line Tool"
weight: 1
---

The Scala Native build links a standalone `texish` executable that turns a source document
into a PDF (or one PNG per page) using the Cairo backend.

```
texish [options] [input-file]

  input-file                    texish source to typeset; reads standard input if omitted
  -o, --output <file>           output path (default: beside the input file, or out)
  -t, --type <pdf | png>        output type (default: pdf)
  -p, --paper <a4 | letter>     paper size (default: letter)
  -r, --resolution <sd|hd|fhd|dpi>  PNG device resolution (default: hd)
```

The resolution is either a named size — `sd` (96 dpi), `hd` (150 dpi) or `fhd` (300 dpi) —
or a positive DPI number. Because the engine measures in points (72 per inch), **`-r 72`
makes one point render as exactly one pixel**, so a document laid out in points reads
directly in pixels — convenient for fixed-size raster targets such as video frames.

## Examples

```sh
texish doc.texish                       # writes doc.pdf
texish doc.texish -o paper -p a4        # writes paper.pdf on A4
texish doc.texish -t png -r fhd         # writes doc.png (or doc_1.png, doc_2.png, …)
texish frame.texish -t png -r 72        # 1 point = 1 pixel: a 1280x720pt page is a 1280x720 PNG
cat doc.texish | texish -o doc          # read the source from standard input
```

PDF is the default output. A single-page document writes `name.png`; a multi-page document
writes `name_1.png`, `name_2.png`, and so on.

## Fonts and packages

The core faces are compiled into the binary: Latin Modern (roman body with its bold, italic, slanted
and small-caps cuts, plus the sans and typewriter roles), Latin Modern Math, New Computer Modern as
the glyph-fallback face, and JetBrains Mono for `\code` listings. The `base` and `document` packages
are compiled in too. So texish renders an ordinary document — Latin, Greek, Cyrillic, mathematics,
highlighted source code — from any directory with nothing installed and nothing configured.

The rest is too large for that, and lives in a `fonts/` and a `packages/` folder: the complex-script
faces, the CJK cuts, the alternative text families, the SMuFL music faces, and every package beyond
those two.

**An installed texish finds its own.** On startup it locates its own executable and looks upward for a
`share/texish/` directory, or a `fonts/`/`packages/` beside it. A package that installs the binary and
the tree therefore needs **no wrapper script and no environment variable**, and it works the same
invoked by absolute path, through `$PATH`, or through a package manager's link farm.

Every release attaches that tree as `texish-<version>-share.tar.gz`, which unpacks to
`share/texish/fonts` and `share/texish/packages`. Unpack it at the prefix holding `bin/texish` and the
binary finds it — see [Installation](/getting-started/installation/#from-a-release).

Otherwise a font path is looked for beside the document and then in the current working directory (so
running from the texish source tree just works), and a module beside the document, in the current
directory, and in `./packages/`. `$TEXISHHOME` remains as a fallback for a tree kept somewhere none of
that finds:

```sh
TEXISHHOME=/opt/texish texish doc.texish   # reads /opt/texish/fonts/… and /opt/texish/packages/…
```

A document that asks for a family this installation does not have is told which font file was
missing, at the point it asks, rather than rendering silently wrong:

```
typeface 'hebrew' is one texish bundles, but its font files were not found —
no font source has 'fonts/NotoSerifHebrew/NotoSerifHebrew-Regular.ttf'
```

## Building the binary

```sh
sbt texishCli/nativeLink
```

The executable is produced at `cli/target/scala-3.8.4/texish-cli`.

## Links and images in PDF output

Clickable links (`\href` / `\url`) become real PDF link annotations only in PDF output —
the PNG backend draws the link text but carries no annotation. `\includegraphics` reads
PNG everywhere and JPEG through the libjpeg-turbo binding on the native backend.
