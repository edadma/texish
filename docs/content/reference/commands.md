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
| `\penalty` `\nobreak` `\eject` | page-break control |

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
| `\xlabel{…}` `\ylabel{…}` `\plottitle{…}` | axis labels and title |
| `\xstep{s}` `\ystep{s}` | force a tick step (default: a *nice* step) |
| `\plot{ … }` | draw the axes, ticks, grid, and labels, then the body's series |
| `\lineplot{colour}{x y x y …}` | a polyline through the data points |
| `\scatter{colour}{x y x y …}` | a filled marker at each point |
| `\bars{colour}{x y x y …}` | a vertical bar to each point |
| `\fnplot{colour}{expr in x}` | a sampled curve of a function |

## Macros and programming

| Command | Effect |
|---------|--------|
| `\def name args {body}` | define a macro (`[name:default]` optional args) |
| `\newenvironment name {begin}{end}` | define a `\begin`/`\end` environment |
| `\let` `\global` `\gdef` | aliasing and global definitions |
| `\if` `\ifx` `\else` `\fi` | conditionals |
| `\calc{…}` `\+ \- \* \/` | arithmetic |
| `\round{value}{places}` | round a number to a fixed number of decimals |
| `\= \!= \< \> \<= \>=` | comparisons (each yields a capturable boolean) |
| `\for … \done` | iteration |
| `\newcounter` `\stepcounter` `\value` `\arabic`/`\roman`/`\Roman`/`\alph`/`\Alph` | counters and number formatting |
| `\use{name}` `\include{path}` | load a format / include raw input |

## Units

`pt` (the base unit, 1/72 in), `in`, `cm`, `mm`, `em`, `ex`.
