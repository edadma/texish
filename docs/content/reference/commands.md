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
| `\label{name}` | bind a name to the current section/figure number, kind, title and page |
| `\ref{name}` | print that number (`??` until resolved) |
| `\pageref{name}` | print the page the label landed on |
| `\eqref{name}` | print the number in parentheses, as for an equation |
| `\autoref{name}` | print the number with its kind in front (e.g. `Section 3`, `Figure 1`) |
| `\nameref{name}` | print the title of the labelled section or caption |
| `\tableofcontents` `\contents` | a contents list built from the section headings |
| `\listoffigures` `\listoftables` | lists of captioned figures / tables (`\listoffigs` `\listoftabs` add the heading) |

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
| `\dfrac{a}{b}` `\tfrac{a}{b}` | forced display-style / text-style fractions |
| `\binom{n}{k}` `\dbinom` `\tbinom` | binomial coefficients |
| `\sqrt{x}` `\sqrt[3]{x}` | roots |
| `\sum` `\int` `\prod` `\limits` | big operators |
| `\operatorname{…}` | an upright custom operator (like `\sin`, with operator spacing) |
| `\left( … \right)` | stretchy delimiters |
| `\hat` `\vec` `\widehat` | accents |
| `\overset{a}{b}` `\underset{a}{b}` `\substack{a \\ b}` | stack material above / below / in a script pile |
| `\boxed{…}` | a framed formula |
| `\text{…}` `\mathcal{…}` | upright / calligraphic |
| `\mathbf` `\mathit` `\mathrm` `\mathsf` `\mathtt` `\mathbb` `\mathfrak` | math alphabets (bold, italic, roman, sans, mono, blackboard, fraktur) |
| `\phantom` `\hphantom` `\vphantom` | reserve a box's size without drawing it |
| `\smash{…}` | draw a box but report zero height and depth |
| `\longrightarrow` `\rightleftharpoons` | long / equilibrium arrows |
| `\,` `\:` `\;` `\!` | thin / medium / thick / negative-thin space |
| `\matrix` `\pmatrix` `\bmatrix` `\vmatrix` `\Vmatrix` `\Bmatrix` `\smallmatrix` `\cases` | matrices |
| `\begin{aligned}…\end{aligned}` | equations aligned on their `&` relations (`\\` between rows) |
| `\eqno(…)` | display equation number |

## More mathematics *(`\use{math}`)*

The amsmath operator and connective names, on top of the built-in math above.

| Command | Effect |
|---------|--------|
| `\arcsin` `\arccos` `\arctan` `\Pr` | further upright operators |
| `\bmod` `\pmod{m}` `\mod{m}` `\pod{m}` | modular-arithmetic forms |
| `\implies` `\impliedby` `\iff` | spaced implication arrows |
| `\dots` | low ellipsis (alias for `\ldots`) |

## Boxes, spacing, and pages

| Command | Effect |
|---------|--------|
| `\hbox` `\vbox` `\vtop` | explicit boxes |
| `\mbox{…}` | an unbreakable horizontal box of its natural width |
| `\makebox[width][l\|c\|r\|s]{…}` | a box of a set width, content aligned (width = a dimension or a `\linewidth` factor) |
| `\parbox[t\|c\|b]{width}{…}` | set a paragraph in a box of the given width, aligned on the baseline |
| `\begin{minipage}[t\|c\|b]{width}…\end{minipage}` | the environment form of `\parbox` *(format)* |
| `\newlength{name}` `\setlength{name}{d}` `\addtolength{name}{d}` | declare / set / adjust a length (a dimension variable, read back as `\name`) |
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
| `\ifthenelse{test}{then}{else}` `\equal{a}{b}` | LaTeX-style branch on a boolean test / string comparison *(format)* |
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
Mono with terminals in bold. Common shapes are drawn idiomatically: a separated list
`A (sep A)*` becomes `A` with `sep` on a return loop, and an optional choice `(A|B|C)?` folds
the skip into the choice. See the [guide](/guide/railroad/) for customising colours and sizes.

## Node-and-edge diagrams *(`\use{diagram}`)*

Declared inside a `\picture`. A node is a named, measured box of a shape; an edge is an arrow
between two named nodes that meets each node's true boundary.

| Command | Effect |
|---------|--------|
| `\node [shape] {name} {placement} {label}` | a node; shape ∈ box, round, stadium, diamond, parallelogram, ellipse, circle, hexagon, subroutine (default box) |
| `\edge [label] {from} {to}` | a straight arrow between two nodes |
| `\link [label] {from} {to}` | a straight undirected line (no arrowhead) |
| `\edgehv` / `\edgevh [label] {from} {to}` | an orthogonal arrow (across-then-down / down-then-across) |
| `\cedge [bend] {from} {to} {label}` | a curved arrow, bowed by `bend` points |
| `\loop {name} {label}` | a self-loop above a node |
| `\dgentry {name} {dir} {len}` | a short entry stub pointing into a node |

A placement is `at X Y`, `at (X,Y)`, or `below`/`above`/`left`/`right REF [gap]` (relative to
another node). The look is set with `\set` on the `dg…` variables (`dgfill`, `dglinecolor`,
`dgfont`, `dggap`, `dgarrow`, …).

The `flowchart` package (`\use{flowchart}`) adds role names — `\start` `\stop` (terminal),
`\process` (box), `\decision` (diamond), `\io` (parallelogram), `\subroutine` — and flow
edges `\flow` / `\branch` / `\rejoin`. The `automaton` package (`\use{automaton}`) adds
`\state` (circle), `\accepting` (double circle), `\initial`, `\trans`, `\arc` and
`\loopabove`. The `er` package (`\use{er}`) draws Chen-notation entity-relationship diagrams
— `\entity`, `\weakentity`, `\relationship`, `\weakrelationship`, `\attribute`,
`\keyattribute`, `\multivalued`, `\derived`, and `\connect` / `\connecttotal` with
cardinalities. All three are thin layers over `diagram`; see the [guide](/guide/diagrams/).

## Units

`pt` (the base unit, 1/72 in), `in`, `cm`, `mm`, `em`, `ex`.
