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
| `\color{name}` `\textcolor{name}{…}` | pen colour (CSS name or `#RRGGBB`) |
| `\href{url}{text}` `\url{url}` | clickable links (real PDF annotations) |
| `\TeX` `\LaTeX` `\TeXish` | the logos |
| `` ``…'' `` `--` `---` `...` `~` | quotes, dashes, ellipsis, tie |

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

## Macros and programming

| Command | Effect |
|---------|--------|
| `\def name args {body}` | define a macro (`[name:default]` optional args) |
| `\newenvironment name {begin}{end}` | define a `\begin`/`\end` environment |
| `\let` `\global` `\gdef` | aliasing and global definitions |
| `\if` `\ifx` `\else` `\fi` | conditionals |
| `\calc{…}` `\+ \- \* \/` | arithmetic |
| `\for … \done` | iteration |
| `\newcounter` `\stepcounter` `\value` `\arabic`/`\roman`/`\Roman`/`\alph`/`\Alph` | counters and number formatting |
| `\use{name}` `\include{path}` | load a format / include raw input |

## Units

`pt` (the base unit, 1/72 in), `in`, `cm`, `mm`, `em`, `ex`.
