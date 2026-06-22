# LaTeX-in-texish — Scope

Purpose of the `latex` worktree: port useful **LaTeX definitions** into texish — as
engine primitives where macros can't fake it, otherwise as `\use`-able packages written
in the document language.

## Framing (carried from the roadmap)

texish is an **evaluated tree-walker** with typed values and **fixed catcodes**, not a
two-stage expansion machine. So LaTeX's expansion plumbing is *unnecessary, not missing* —
do NOT port `\expandafter`, `\noexpand`, `\edef`, `\protect`/robust-command machinery,
runtime catcode changes, `\makeatletter`. The goal is LaTeX's **authoring surface** (the
commands a document author types), not its internal `.ltx` implementation.

Each candidate below is tagged:
- **[have]** — already implemented (listed for completeness / compatibility-alias only)
- **[pkg]** — pure document-language package macros; no engine change
- **[prim]** — needs a small new engine primitive/helper
- **[ENGINE]** — needs a substantial engine feature (the real blockers)

---

## A. LaTeX command-name compatibility layer  [pkg, cheap, high value]

texish already has the capabilities; LaTeX authors just type different names. A thin
`latex` compat package gives muscle-memory commands.

- `\newcommand`/`\renewcommand`/`\providecommand`(`*`) → wrap `\def`/`\gdef`. **[pkg]**
  (optional-arg + `[n]` arg-count form maps onto existing macro param + xparse optional args)
- `\newcommand`'s `\ensuremath{…}` → run body in math if not already. **[pkg]** (small)
- `\vspace{d}` / `\hspace{d}` → `\vskip` / `\hskip`; `\vspace*` (non-discardable). **[pkg]**
- `\rule[lift]{w}{h}` → `\hrule`/`\vrule` box with explicit dims. **[pkg/prim]**
- `\smallskip`/`\medskip`/`\bigskip` **[have]**, `\newline`/`\\` **[have]** (`\cr`)
- `\linebreak`/`\nolinebreak`/`\pagebreak`/`\nopagebreak` → `\penalty`. **[pkg]**
- `\phantom`/`\hphantom`/`\vphantom`/`\smash` → zero-ink boxes of measured size. **[prim]**
  (needs box measure, which exists via `\setbox`/`\wd`/`\ht`/`\dp`)
- `\stretch{n}`, `\hfill`**[have]**/`\dotfill`**[have]**/`\hrulefill`**[have]**
- `\xspace` (smart trailing space). **[pkg]**

## B. Math enrichment — an `amsmath`-style package  [mostly pkg, some prim]

Highest author-visible payoff after refs. Core math mode is already TeX-class.

- Math alphabets `\mathbb`, `\mathbf`, `\mathrm`, `\mathsf`, `\mathtt`, `\mathfrak`
  (`\mathcal` **[have]**). **[prim]** — each is a font-axis restyle of the formula run;
  `\mathbb`/`\mathfrak` need blackboard/fraktur glyphs (Latin Modern Math has some).
- Operators `\lim \sup \inf \max \min \det \gcd \deg \exp \ln \log \sin \cos \tan …`
  and `\DeclareMathOperator`. **[pkg]** over `\text` + `\limits` + spacing.
- `\binom`/`\dbinom`/`\tbinom`, `\dfrac`/`\tfrac` → `\over`/`\atop` + delimiter sizing. **[pkg]**
- `\overbrace`/`\underbrace` (with scripts), `\overline`/`\underline` in math,
  `\overrightarrow`. **[prim]** (stretchy horizontal brace/line over a sub-box)
- `\stackrel`, `\substack`, `\boxed`, `\pmod`/`\bmod`, `\bigl…\Biggr` delimiter sizes. **[pkg]**
- Numbered display environments `equation`/`equation*`, `align`/`align*`, `gather`,
  `multline`, `cases`**[have]**, `matrix`/`array` math envs. **[ENGINE-ish]** — `align`
  needs `\halign`-in-math + equation numbering hooked to a counter; build on existing
  `\halign` + `\eqno`.
- `\eqref`, `\tag` — depend on cross-ref system (section E).

## C. Tables — `booktabs` / `array` / spanning  [pkg + one engine gap]

`\tabular` and `\halign` exist; LaTeX table idioms layer on top.

- `booktabs`: `\toprule`/`\midrule`/`\bottomrule`/`\cmidrule` → ruled `\noalign{\hrule}`
  with proper rule weights & spacing. **[pkg]**
- `\multicolumn{n}{spec}{text}` and `\multirow`. **[ENGINE]** — needs `\span`/`\multispan`
  column spanning in `HAlignMode` (roadmap lists this as not-yet-done).
- `array` math environment, `tabularx` (auto-width columns via `\halign to:`). **[prim]**
  (`\halign to:`/`spread` infra exists per roadmap, just unwired)
- `\arraystretch`, `p{width}` paragraph columns. **[prim]**

## D. Floats, captions, lists  [pkg, except cross-listing]

- `\listoffigures`/`\listoftables` → needs section E (two-pass). **[ENGINE]**
- `subcaption`/`subfigure`, `wrapfigure` (text wrap around float). **[pkg/prim]**
- Float placement `[H]` (hard-here, `float` package). **[pkg]** (over existing `h` spec)
- `enumitem`-style list tuning (`\begin{enumerate}[label=…,leftmargin=…]`),
  `\setlist`. **[pkg]** (lists + counters already exist)
- `\caption*`, caption styling, `\captionof`. **[pkg]**

## E. Cross-references & TOC — `\label`/`\ref`/`\tableofcontents`  [DONE]

The aux store + two-pass driver (`ReferenceTable` + `Passes.untilStable`) and
`\label`/`\ref`/`\pageref`/`\tableofcontents` landed earlier (carried in from `dev`). The
remaining commands were added in this worktree:

- `\eqref{k}` (parenthesised number), `\autoref{k}` (kind word + number), `\nameref{k}`
  (title) — over a `RefEntry` that now carries the label's kind and name, captured at
  `\label` from `currentlabeltype` / `currentlabelname`.
- `\listoffigures` / `\listoftables` — over named contents lists ("toc"/"lof"/"lot") in
  `ReferenceTable`; `\addcontentsline{list}{lvl}{num}{title}` files an entry, `\caption`
  files into lof/lot. PageMode's shipout walk recurses into floats so a caption's `\label`
  / `\addcontentsline` learns the page its float ships on.

## F. Page layout & headers  [pkg, one engine gap]

- `geometry`-style margins → set `hsize`/`vsize`/`hoffset`/`voffset`/`paperwidth/height`. **[pkg]**
- `fancyhdr` (`\fancyhead`/`\fancyfoot`/`\pagestyle`) → over `headline`/`footline`/`\mark`. **[pkg]**
- `setspace` (`\onehalfspacing`/`\doublespacing`) → `baselineskip`. **[pkg]**
- `titlesec` section-format hooks. **[pkg]** (sectioning macros already exist to restyle)
- `multicol` / `\twocolumn`. **[ENGINE]** — page-builder/output-routine hook (roadmap Tier-1 #3).
- `\marginpar`. **[ENGINE]** (same page-builder hook)

## G. Graphics transforms & framing — `graphicx`/`xcolor`  [prim]

- `\fbox`/`\framebox`/`\colorbox`/`\fcolorbox`/`\boxed`. **[prim]** (box measure exists;
  needs a frame/background-fill box) — roadmap flags `\fbox` as an unblocked next step.
- `\rotatebox`/`\scalebox`/`\reflectbox`/`\resizebox`. **[prim]** — apply a CTM to a typeset
  box; `\picture` already has translate/scale/rotate, generalize the transform seam to boxes.
- `\raisebox`. **[pkg]** (`\raise`/`\lower` + measure).
- `xcolor` `\definecolor`/named-model colors. **[pkg/prim]** (`\color` exists).

## H. Verbatim & code listings  [prim, separate track]

Already designed in its own roadmap (`verbatim_code` memo): engine `verbatim`/`\verb`
(raw-capture seam) then `\code` highlighting via the `highlighter` dep. **[prim]** —
not duplicated here; pull from that plan.

## I. Misc box/layout commands  [mostly DONE]

- `\parbox[pos]{w}{…}`, `\begin{minipage}[pos]{w}` — **DONE** (engine primitives `\parbox`
  and `\beginminipage`/`\endminipage`; set hsize → build vbox at the width → align on the
  baseline, `c` via a metric-adjusting RaiseBox). `\minipage` env wired in `document.texish`.
- `\mbox`/`\makebox[w][pos]` — **DONE** (engine primitives; `\makebox` pads the content with
  fil glue inside an hbox set to the width, l/c/r/s alignment).
- `\newlength`/`\setlength`/`\addtolength` — **DONE** (engine primitives over the `\set`
  variable store; the length is a plain Dimen variable, read back as `\name` / in `\calc`).
- `\ifthenelse{test}{then}{else}` + `\equal` — **DONE** (`document.texish` macros over `\if`
  and `\=`; compound tests use the expression operators). `\ifdefined` not done.
- `siunitx` (`\SI`/`\num`/`\si`) — **NOT DONE.** A faithful siunitx needs a number formatter
  (grouping/rounding/exponents — no grouping primitive exists yet) plus a unit-macro algebra
  (`\kilo\gram\per\second`); it is a substantial standalone package, deferred to its own pass.

---

## Recommended sequencing

Two independent spines (can run in parallel):

**Author-surface spine (fast, high coverage, low risk):**
1. **A — LaTeX compat package** (`\newcommand`, `\vspace`/`\hspace`, `\rule`, `\xspace`, …) —
   cheapest win, makes pasted LaTeX "mostly work."
2. **B — amsmath package** (operators, alphabets, `\binom`/`\dfrac`, `\overbrace`) — biggest
   visible payoff; mostly macros + a few math prims.
3. **F — geometry/fancyhdr/setspace** packages — pure macros over existing registers.
4. **C/D — booktabs + list/caption tuning** — pure macros; defer `\multicolumn` (needs `\span`).

**Engine spine (deep, unlocks the rest):**
5. **G — `\fbox` + box-transform seam** (`\rotatebox`/`\scalebox`/`\fbox`/`\parbox`) — self-contained.
6. **E — cross-references + TOC** (aux store + two-pass) — the flagship; design the aux/two-pass
   driver first, ship `\label`/`\ref` for eval-time numbers, then `\pageref`/TOC.
7. **C `\span`/`\multicolumn`** and **F `multicol`/`\marginpar`** (page-builder hook) — last,
   they share the alignment-spanning and output-routine machinery.

**Start here:** A (compat package) + B (amsmath) give the most "renders real LaTeX" per unit
effort with little engine risk. E (cross-refs) is the one feature that genuinely changes the
engine and is worth a dedicated design pass.

> Note: prior firm decision (document-features handoff) deferred `\ref`/`\label`/`\cite`.
> This worktree exists to revisit that — confirm before starting section E.
