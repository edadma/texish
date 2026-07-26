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

## Fonts

The core faces are compiled into the binary: Latin Modern (roman body with its bold, italic, slanted
and small-caps cuts, plus the sans and typewriter roles), Latin Modern Math, and New Computer Modern
as the glyph-fallback face. So texish renders Latin, Greek, Cyrillic and mathematics from any
directory with nothing installed and nothing configured.

The wider bundled set — the complex-script faces, the CJK cuts, the alternative text families, the
SMuFL music faces — is too large for that and lives in a `fonts/` directory. **An installed texish
finds its own**: on
startup it locates its executable and looks upward for `share/texish/fonts` or a `fonts/` beside it,
so a package that installs the binary and the tree needs no wrapper script and no environment
variable. Symlinks are resolved, so invoking it through `$PATH` or through a package manager's link
farm works the same.

Failing that, a font path is looked for beside the document, then in the current working directory,
then under `$TEXISHHOME` — so running from the texish source tree just works, and `TEXISHHOME` still
covers a tree kept somewhere unusual:

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
sbt texishNative/nativeLink
```

The executable is produced at `native/target/scala-3.8.4/texish`.

## Links and images in PDF output

Clickable links (`\href` / `\url`) become real PDF link annotations only in PDF output —
the PNG backend draws the link text but carries no annotation. `\includegraphics` reads
PNG everywhere and JPEG through the libjpeg-turbo binding on the native backend.
