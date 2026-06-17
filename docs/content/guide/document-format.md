---
title: "The Document Format"
weight: 1
---

The engine itself is small — boxes, glue, macros, math. The familiar article furniture
(titles, sections, lists, figures) comes from a *format* you load at the top of a document:

```
\use{document}
```

This is the analogue of plain TeX's format or a LaTeX document class, kept deliberately
small. `\use` resolves the format from texish's bundled packages, so nothing else is
needed. Everything below assumes it is loaded.

## Title block

Declare the title, author, and date, then typeset the block with `\maketitle`:

```
\title{On the Electrodynamics of Moving Bodies}
\author{A. Einstein}
\date{June 1905}
\maketitle
```

Each is optional; `\maketitle` centers whichever were given.

## Abstract

```
\begin{abstract}
A short summary of the document, indented on both sides under a centered heading.
\end{abstract}
```

## Sections

`\section` and `\subsection` are auto-numbered; a subsection's number resets with each new
section. The first paragraph after a heading is set flush left with no indent, as in a
LaTeX article; following paragraphs indent.

```
\section{Introduction}
The opening paragraph sits flush against the left margin.

A second paragraph opens with the usual first-line indent.

\subsection{Background}
Numbered 1.1, 1.2, and so on.
```

## Lists

Lists nest, and the markers hang to the left of the text so wrapped lines align under the
content rather than the marker.

```
\begin{enumerate}
\item First point.
\item Second point.
\end{enumerate}

\begin{itemize}
\item A bullet.
\item Another bullet, with a nested list:
  \begin{itemize}
  \item a deeper bullet.
  \end{itemize}
\end{itemize}
```

## Quotations

`quote` indents a block on both sides; `quotation` indents it further. Both revert at the
matching `\end`.

```
\begin{quote}
The art of writing is the art of discovering what you believe.
\end{quote}
```

## Footnotes

`\footnote` raises a numbered marker in the text and sets its body at the foot of whatever
page the marker lands on.

```
The result was first noted by Gauss\footnote{In a letter of 1809.} and later refined.
```

## Spacing and page breaks

```
\smallskip   \medskip   \bigskip       % named vertical gaps
\vskip 12pt plus 2pt minus 1pt         % explicit vertical glue
\vfill                                  % stretchable fill to the page bottom
\eject                                  % force a page break (\vfill\eject to flush)
```

## Running heads and feet

The format ships a page number in the footer. Override `\footline`, or define a
`\headline`, to change the running head or foot; `\the\pageno` is the current page.

```
\def headline {\hfil\italic{Draft}\hfil}
```

See [figures and images](/guide/figures-and-images/) for `\figure`, `\caption`, and
centering, and [text and markup](/guide/text-and-markup/) for emphasis, code, colour, and
links.
