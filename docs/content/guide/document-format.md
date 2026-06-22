---
title: "The Document Format"
weight: 1
---

The engine itself is small — boxes, glue, macros, math. The familiar article furniture
(titles, sections, lists, figures) comes from a *format* you load at the top of a document:

```texish
\use{document}
```

This is the analogue of plain TeX's format or a LaTeX document class, kept deliberately
small. `\use` resolves the format from texish's bundled packages, so nothing else is
needed. Everything below assumes it is loaded.

## Title block

Declare the title, author, and date, then typeset the block with `\maketitle`:

```texish
\title{On the Electrodynamics of Moving Bodies}
\author{A. Einstein}
\date{June 1905}
\maketitle
```

Each is optional; `\maketitle` centers whichever were given.

## Abstract

```texish
\begin{abstract}
A short summary of the document, indented on both sides under a centered heading.
\end{abstract}
```

## Sections

`\section` and `\subsection` are auto-numbered; a subsection's number resets with each new
section. The first paragraph after a heading is set flush left with no indent, as in a
LaTeX article; following paragraphs indent.

```texish
\section{Introduction}
The opening paragraph sits flush against the left margin.

A second paragraph opens with the usual first-line indent.

\subsection{Background}
Numbered 1.1, 1.2, and so on.
```

## Cross-references and a table of contents

`\label` binds a name to the section (or figure or table) it follows — its number, its kind, its
title and its page. `\ref` prints that number later and `\pageref` prints the page it landed on.
Forward references work because the document is typeset twice over a shared label table — the first
pass learns every number and page, the second fills them in.

```texish
\section{Results}
\label{sec:results}
...

As shown in section~\ref{sec:results} on page~\pageref{sec:results}, ...
```

Three more references read the other things `\label` recorded: `\autoref` prints the number with
its kind word in front (`Section 3`, `Figure 1`), `\nameref` prints the title, and `\eqref` prints
the number in parentheses for an equation.

```texish
\autoref{sec:results} confirms the bound; see also ``\nameref{sec:results}''.
```

`\tableofcontents` (or `\contents`, which adds an unnumbered heading) lists the sections with their
page numbers and a dotted leader between, built from the headings that have been recorded.
`\listoffigures` and `\listoftables` do the same for captioned floats — each `\caption` files itself
into the matching list — and `\listoffigs` / `\listoftabs` add an unnumbered heading above them:

```texish
\maketitle
\contents
\listoffigs
```

## Lists

Lists nest, and the markers hang to the left of the text so wrapped lines align under the
content rather than the marker.

```texish
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

```texish
\begin{quote}
The art of writing is the art of discovering what you believe.
\end{quote}
```

## Footnotes

`\footnote` raises a numbered marker in the text and sets its body at the foot of whatever
page the marker lands on.

```texish
The result was first noted by Gauss\footnote{In a letter of 1809.} and later refined.
```

## Spacing and page breaks

```texish
\smallskip   \medskip   \bigskip       % named vertical gaps
\vskip 12pt plus 2pt minus 1pt         % explicit vertical glue
\vfill                                  % stretchable fill to the page bottom
\eject                                  % force a page break (\vfill\eject to flush)
```

Page breaks are chosen by cost, as in TeX: the break of least badness-plus-penalty wins, so a
positive `\penalty` steers a break away from a spot that would strand material at the foot of a
page. On an ordinary page (rigid interline glue) this comes to the same thing as filling each page
as far as it will go.

## Multiple columns

`\columns{n}{…}` sets its body as *n* balanced columns — typeset once at the column measure, then
divided into pieces of nearly equal height and stood side by side. An optional `gap:` sets the
gutter (one em by default).

```texish
\columns{2}{
A long passage that the engine pours into two columns of equal height, the way a newspaper or a
two-column paper reads: down the first column, then back up to the top of the second.
}

\columns gap:24pt {3}{ ... three columns with a wider gutter ... }
```

A balanced block is set as a single unit, so it sits within one page rather than flowing across a
page break.

## Boxes, minipages, and lengths

`\mbox` keeps a phrase on one line; `\makebox` gives a box an explicit width, with its content
flush left, centred, flush right, or stretched (`l` / `c` / `r` / `s`); and `\parbox` sets a whole
paragraph into a box of a chosen width, so two columns of text can sit side by side. The width may
be a dimension or a fraction of `\linewidth`.

```texish
\mbox{Newton--Raphson}                       % never breaks across a line
\makebox[0.5\linewidth][r]{flush right}
\parbox{0.3\linewidth}{A paragraph set in a narrow box.}
```

`minipage` is the environment form of `\parbox`, which reads better for more than a few words. The
optional `[t]` / `[c]` / `[b]` aligns the box on the surrounding baseline by its first line, centre,
or last line:

```texish
\begin{minipage}[t]{0.45\linewidth}
The left column of a two-column row.
\end{minipage}\hskip 24pt \begin{minipage}[t]{0.45\linewidth}
The right column, top-aligned with the left.
\end{minipage}
```

A *length* is a dimension variable. `\newlength` declares one, `\setlength` assigns it, and
`\addtolength` adjusts it; the result is read back as `\name` and drives real spacing:

```texish
\newlength{gutter}
\setlength{gutter}{20pt}
\addtolength{gutter}{1em}
\hskip\gutter
```

`\ifthenelse{test}{then}{else}` chooses a branch on a boolean test, and `\equal{a}{b}` is true when
the two expand to the same string; numeric tests use the comparison operators (`\>`, `\<`, `\=`).

```texish
\ifthenelse{\> {\the\pageno} {1}}{not the first page}{the first page}
```

## Running heads and feet

The format ships a page number in the footer. Override `\footline`, or define a
`\headline`, to change the running head or foot; `\the\pageno` is the current page.

```texish
\def headline {\hfil\italic{Draft}\hfil}
```

See [figures and images](/guide/figures-and-images/) for `\figure`, `\caption`, and
centering, and [text and markup](/guide/text-and-markup/) for emphasis, code, colour, and
links.
