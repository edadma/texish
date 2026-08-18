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

The everyday conveniences — the size ladder (`\large`, `\Large`, …), the vertical skips
(`\smallskip`/`\medskip`/`\bigskip`), single-line alignment (`\centerline` and friends),
the TeX logos and the `center` / `flushleft` / `quote` environments — actually live in a
lighter package, **`base`**, that carries none of the article furniture. `document`
builds on it (`\use{base}`), so they are all available here; but a page that wants only
those — a caption, a flyer, a title card — can load `\use{base}` alone and skip titles,
sections and page numbering. (The inline shape commands `\italic`, `\bold`, `\smallcaps`,
`\mono` and friends are engine primitives — available with no package at all.)

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

`\if {test} … \else … \fi` chooses a branch on a boolean test. The test is an ordinary expression, so
`\=` compares two strings and the comparison operators (`\>`, `\<`) compare numbers.

```texish
\if {\> {\the\pageno} {1}} not the first page \else the first page \fi
```

## Running heads and feet

The format ships a page number in the footer. Override `\footline`, or define a
`\headline`, to change the running head or foot; `\the\pageno` is the current page.

```texish
\def headline {\hfil\italic{Draft}\hfil}
```

## Two-sided documents

A document printed on both sides of the sheet is read as openings, not as a stack of pages,
and the two halves of an opening are mirror images: the binding margin is on the right of a
verso and the left of a recto. `\geometry twoside:on` says so, and the engine reflects the
text block — body, running head and running foot together — on every even folio. Name the
horizontal margins `inner` and `outer` and they read the way the reader sees them:

```texish
\geometry paper:a5 inner:22mm outer:14mm top:18mm bottom:20mm
```

That gives every page 22mm against the spine and 14mm at the outer edge, whichever side it
falls on. Naming either margin turns two-sided printing on by itself, since neither means
anything otherwise; `twoside:off` alongside them switches it off again.

Two macros go with it. `\ifrecto{…}{…}` takes its first branch on a recto (an odd folio) and
its second on a verso, so a head or foot can put a folio at the outer corner of each page:

```texish
\def footline {\ifrecto{\hss\the\pageno}{\the\pageno\hss}}
```

`\cleardoublepage` ends the page and, if the next one would be a verso, ships a blank leaf so
what follows opens on a recto — the page a new chapter belongs on. The leaf it inserts carries
neither head nor folio, so it comes out genuinely blank. The `book` format opens every chapter
and part with it when the book is two-sided, and takes the next page when it is not.

Both read `pageno`, the folio a page prints rather than a count of sheets, which is the parity
the margins are mirrored on too — so a front matter renumbered to restart at 1 keeps the
margins, the blank leaves and the printed folios all agreeing.

A running head can also name a different thing on each side of the opening, which is what the
second mark stream is for: `\mark` records the division and `\submark` the subdivision, read
back as `firstmark` and `firstsubmark`. The `book` format marks chapters with the one and
sections with the other, so a two-sided book names the chapter on the verso and the current
section on the recto.

See [figures and images](/guide/figures-and-images/) for `\figure`, `\caption`, and
centering, and [text and markup](/guide/text-and-markup/) for emphasis, code, colour, and
links.
