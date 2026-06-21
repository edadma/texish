---
title: texish
splash: true
heroTitle: A TeX-style document engine for
heroHighlight: Scala
summary: Write documents in a small TeX-like language and render them to PDF or images. Latin Modern type, real Knuth-Plass line breaking, TeX math mode, footnotes and floats, an inline vector-graphics mode — cross-built for the JVM, Scala Native, and Scala.js.
getStarted: true
---

## Why texish

texish lays out and renders documents the way TeX does. You write in a small TeX-like
language — macros, math mode, boxes and glue — and the engine breaks paragraphs into lines
and lines into pages, then draws them through a pluggable backend.

```texish
\use{document}
\title{On the Motion of Small Particles}
\author{A. Einstein}
\maketitle

\section{Introduction}
It is well known that $E = mc^2$. The displayed form,
$$ \int_0^\infty e^{-x^2}\,dx = \frac{\sqrt\pi}{2}, $$
follows from the Gaussian integral.
```

Body text is set in **Latin Modern Roman**, math in the matching **Latin Modern Math**
through an OpenType `MATH` table, and monospaced text in **Latin Modern Mono** — one
coherent family across prose, equations, and code.

## What's here

- **[Getting Started](/getting-started/)** — install texish and render your first document.
- **[Guide](/guide/)** — the document format, text and markup, mathematics, figures and
  images, the inline vector-graphics mode, chemistry, and data plotting.
- **[Reference](/reference/)** — the command-line tool and a command cheat sheet.

## What it does

- **Knuth-Plass line breaking** with Liang hyphenation, cost-based page breaks with
  widow/orphan control, and balanced multi-column layout, all in a point-space coordinate system.
- **TeX math mode** — inline `$…$` and centered display `$$…$$`: fractions, radicals,
  scripts, stretchy delimiters, accents, big operators, matrices, the math alphabets
  (`\mathbf`, `\mathbb`, `\mathfrak`, …), and phantom/`\smash` spacing.
- **A document format** (`\use{document}`) — title blocks, numbered sections,
  cross-references and a table of contents, lists, quotations, figures and tables with
  captions, and footnotes.
- **Clickable links and images** — `\href`/`\url` become real PDF link annotations;
  `\includegraphics` places PNG and JPEG images.
- **Vector graphics** — `\picture` draws shapes, paths, and placed type inline, through the
  same pipeline as the text.
- **Chemistry and plotting** — `\use{chem}` sets reaction equations and structures;
  `\use{plot}` draws line, scatter, bar, and function plots with labelled axes.
- **Three platforms** — the JVM (Graphics2D raster), Scala Native (Cairo PDF + image), and
  Scala.js.
