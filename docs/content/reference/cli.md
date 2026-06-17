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
  -r, --resolution <sd|hd|fhd>  PNG device resolution (default: hd)
```

## Examples

```sh
texish doc.texish                       # writes doc.pdf
texish doc.texish -o paper -p a4        # writes paper.pdf on A4
texish doc.texish -t png -r fhd         # writes doc.png (or doc_1.png, doc_2.png, …)
cat doc.texish | texish -o doc          # read the source from standard input
```

PDF is the default output. A single-page document writes `name.png`; a multi-page document
writes `name_1.png`, `name_2.png`, and so on.

## Building the binary

```sh
sbt texishNative/nativeLink
```

The executable is produced at `native/target/scala-3.8.4/texish`.

## Links and images in PDF output

Clickable links (`\href` / `\url`) become real PDF link annotations only in PDF output —
the PNG backend draws the link text but carries no annotation. `\includegraphics` reads
PNG everywhere and JPEG through the libjpeg-turbo binding on the native backend.
