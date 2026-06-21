---
title: "Command Cheat Sheet"
weight: 2
---

A quick index of the commands. Those marked *(format)* require `\use{document}`; the rest
are engine primitives available in any document.

## Document structure *(format)*

| Command | Effect |
|---------|--------|
| `\title{…}` `\author{…}` `\date{…}` | set the title-block fields |
| `\maketitle` | typeset the centered title block |
| `\begin{abstract}…\end{abstract}` | a centered, indented summary |
| `\section{…}` `\subsection{…}` | auto-numbered headings |
| `\begin{itemize}` `\begin{enumerate}` `\item` | bulleted and numbered lists |
| `\begin{quote}` `\begin{quotation}` | indented quotation blocks |
| `\footnote{…}` | a numbered footnote |
| `\figure[htb]{…}` `\table[htb]{…}` `\caption{…}` | floats with numbered captions |

## Cross-references and contents *(format)*

The document is typeset twice over a shared label table, so forward references resolve.

| Command | Effect |
|---------|--------|
| `\label{name}` | bind a name to the current section/figure number and its page |
| `\ref{name}` | print that number (`??` until resolved) |
| `\pageref{name}` | print the page the label landed on |
| `\tableofcontents` `\contents` | a contents list built from the section headings |

## Text and markup

| Command | Effect |
|---------|--------|
| `\emph{…}` `\textit{…}` `\textbf{…}` | italic, italic, bold |
| `\texttt{…}` | monospaced (Latin Modern Mono) |
| `\textsub{…}` `\textsup{…}` | text subscript / superscript |
| `\color{name}` `\textcolor{name}{…}` | pen colour (CSS name or `#RRGGBB`) |
| `\href{url}{text}` `\url{url}` | clickable links (real PDF annotations) |
| `\TeX` `\LaTeX` `\TeXish` | the logos |
| `` ``…'' `` `--` `---` `...` `~` | quotes, dashes, ellipsis, tie |
| `\$` | a literal dollar sign |

## Mathematics

| Command | Effect |
|---------|--------|
| `$…$` `$$…$$` | inline and display math |
| `^` `_` | super/subscript |
| `\frac{a}{b}` `a \over b` | fractions |
| `\sqrt{x}` `\sqrt[3]{x}` | roots |
| `\sum` `\int` `\prod` `\limits` | big operators |
| `\left( … \right)` | stretchy delimiters |
| `\hat` `\vec` `\widehat` | accents |
| `\text{…}` `\mathcal{…}` | upright / calligraphic |
| `\mathbf` `\mathit` `\mathrm` `\mathsf` `\mathtt` `\mathbb` `\mathfrak` | math alphabets (bold, italic, roman, sans, mono, blackboard, fraktur) |
| `\phantom` `\hphantom` `\vphantom` | reserve a box's size without drawing it |
| `\smash{…}` | draw a box but report zero height and depth |
| `\longrightarrow` `\rightleftharpoons` | long / equilibrium arrows |
| `\,` `\:` `\;` `\!` | thin / medium / thick / negative-thin space |
| `\matrix` `\pmatrix` `\bmatrix` `\cases` | matrices |
| `\eqno(…)` | display equation number |

## Boxes, spacing, and pages

| Command | Effect |
|---------|--------|
| `\hbox` `\vbox` `\vtop` | explicit boxes |
| `\centerline` `\leftline` `\rightline` `\centering` | alignment |
| `\rlap` `\llap` | zero-width overlap boxes |
| `\kern` `\hskip` `\vskip` | rigid / glue spacing |
| `\smallskip` `\medskip` `\bigskip` | named vertical gaps |
| `\vfil` `\vfill` `\hfil` `\hss` | stretchable fill |
| `\lower` `\raise` | shift a box vertically |
| `\setbox` `\box` `\copy` `\wd`/`\ht`/`\dp` | box registers and their dimensions |
| `\vsplit name to:<dim>` | cut a saved vbox at a page-style breakpoint |
| `\columns {n} {…}` | set the body as *n* balanced columns (`gap:<dim>` before `{n}` sets the gutter) |
| `\leaders` `\cleaders` `\xleaders` | fill space by tiling a box |
| `\dotfill` `\hrulefill` | a dotted / ruled leader (e.g. contents lines) |
| `\discretionary{pre}{post}{no}` `\softhyphen` | author-controlled break points |
| `\penalty` `\nobreak` `\eject` | page-break control (breaks are chosen by cost) |

## Images and graphics

| Command | Effect |
|---------|--------|
| `\includegraphics[width=,height=,scale=]{path}` | place a PNG or JPEG |
| `\picture width:… height:… {…}` | open a vector-graphics drawing |

## Chemistry *(`\use{chem}`)*

| Command | Effect |
|---------|--------|
| `\ce{CH4 + 2 O2 -> CO2 + 2 H2O}` | a reaction equation (mhchem-style) |
| `\dn{n}` `\up{n}` | formula subscript / superscript |
| `\atom{name}{x y}{label}` | place and label an atom |
| `\bond` `\dbond` `\tbond` | single / double / triple bond |

## Plotting *(`\use{plot}`)*

| Command | Effect |
|---------|--------|
| `\xrange{min}{max}` `\yrange{min}{max}` | the plot's data ranges |
| `\autorange{x y x y …}` | derive both ranges from the data |
| `\xcategories{A B C …}` | name the x ticks instead of numbering them |
| `\xlabel{…}` `\ylabel{…}` `\plottitle{…}` | axis labels and title |
| `\xstep{s}` `\ystep{s}` | force a tick step (default: a *nice* step) |
| `\xtickformat{pre}{suf}` `\ytickformat{pre}{suf}` | wrap numeric tick labels (e.g. `\$`, `\%`) |
| `\plot{ … }` | draw the axes, ticks, grid, and labels, then the body's series |
| `\lineplot[colour][label]{x y x y …}` | a polyline through the data points |
| `\scatter[colour][label]{x y x y …}` | a marker at each point (shape from `plotmarkshape`) |
| `\bars[colour][label]{x y x y …}` | a vertical bar to each point (`plotvalues` labels them) |
| `\areaplot[colour][label]{x y x y …}` | the band under the curve, filled to the baseline |
| `\stepplot[colour][label]{x y x y …}` | a staircase holding each value to the next x |
| `\fnplot[colour][label]{expr in x}` | a sampled curve of a function |
| `\bubble[colour][label]{x y size …}` | a translucent disc at each point, sized by a third value |
| `\errorbars[colour][label]{x y err …}` | a capped y ± err whisker at each point |
| `\trendline[colour][label]{x y x y …}` | the least-squares line of best fit |
| `\hline[colour]{y}` `\vline[colour]{x}` | a dashed reference line at a data value |
| `\legend[ne\|nw\|se\|sw]` | draw a key for the labelled series |

Both bracket arguments are optional: with no colour (or `[]`) a series takes the next
palette colour, and a label adds a legend entry.

## Macros and programming

| Command | Effect |
|---------|--------|
| `\def name args {body}` | define a macro (`[name:default]` optional args, `<name>` a verbatim argument) |
| `\newenvironment name {begin}{end}` | define a `\begin`/`\end` environment |
| `\let` `\global` `\gdef` | aliasing and global definitions |
| `\if` `\ifx` `\else` `\fi` | conditionals |
| `\calc{…}` `\+ \- \* \/` | arithmetic |
| `\round{value}{places}` | round a number to a fixed number of decimals |
| `\= \!= \< \> \<= \>=` | comparisons (each yields a capturable boolean) |
| `\for … \done` | iteration |
| `\seq{…}` `\words{s}` `\head` `\tail` `\size` `\cat` | sequences; `\words` splits a string on whitespace |
| `\map{…}` `\mapset` `\mapget` `\maphas` | keyed maps |
| `\message{…}` | write expanded text to standard error (a diagnostic; no page output) |
| `\oklch{L}{C}{h}` `\oklchof{color}` | build / read a colour in the Oklch space (lightness, chroma, hue) — derive shades by varying L |
| `\newcounter` `\stepcounter` `\value` `\arabic`/`\roman`/`\Roman`/`\alph`/`\Alph` | counters and number formatting |
| `\use{name}` `\include{path}` | load a format / include raw input |

## Railroad diagrams *(`\use{railroad}`)*

| Command | Effect |
|---------|--------|
| `\railroad{ <grammar> }` | draw a railroad (syntax) diagram per rule from W3C-style EBNF |

The grammar is read verbatim. Rules are `name ::= expression`; an expression supports `\|`
(alternation), juxtaposition (concatenation), `?`/`*`/`+` (optional / zero-or-more /
one-or-more), `(…)` grouping, `"…"`/`'…'` terminals (rounded boxes), `[…]` character classes
(pointed hexagons), and bare names as nonterminals (square boxes). Labels are set in JetBrains
Mono with terminals in bold.

## Units

`pt` (the base unit, 1/72 in), `in`, `cm`, `mm`, `em`, `ex`.
