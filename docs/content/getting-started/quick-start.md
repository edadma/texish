---
title: "Quick Start"
weight: 2
---

A texish document is a plain text file. The document features — titles, sections, lists,
figures — live in a format you load with `\use{document}` at the top.

## A first document

Save this as `hello.texish`:

```texish
\use{document}

\title{A First Document}
\author{Your Name}
\date{June 2026}
\maketitle

\section{Introduction}
Welcome to texish. This paragraph is set in Latin Modern Roman and broken into
lines the way \TeX does. Input conventions give you proper typography for free:
``curly quotes'', an en--dash, an em---dash, and an ellipsis\dots

\section{A little mathematics}
Inline math sits in the text, like $a^2 + b^2 = c^2$, and a displayed equation is
centered on its own line:
$$ \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}. $$
```

## Render it

With the [command-line tool](/reference/cli/):

```sh
texish hello.texish            # writes hello.pdf
texish hello.texish -t png     # writes hello.png (one per page)
```

The output lands beside the source file. Pass `-o` to choose a different path, `-p a4` for
A4 paper, and `-r fhd` for a higher-resolution PNG.

## Next steps

- The [document format](/guide/document-format/) — sections, lists, quotations, and the
  title block.
- [Text and markup](/guide/text-and-markup/) — emphasis, code, colour, and clickable links.
- [Mathematics](/guide/mathematics/) — the full math-mode vocabulary.
