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

## E. Cross-references & TOC — `\label`/`\ref`/`\tableofcontents`  [ENGINE, the big one]

The single largest missing LaTeX subsystem. Previously deferred; this worktree is the
natural place to take it on.

- `\label{k}`/`\ref{k}`/`\pageref{k}`/`\eqref{k}`/`\autoref{k}`/`\nameref{k}`.
- `\tableofcontents`, `\listoffigures`, `\listoftables`.
- **Hard part:** `\pageref` and TOC page numbers are unknown until page breaks happen →
  resolve after shipout and feed back. Needs **(a)** an aux store (file-writing `\write`,
  absent today, or an in-memory equivalent) and **(b)** a run-twice driver. This is the
  biggest single plumbing piece — design it first, then `\label`/`\ref` for section/figure
  numbers (known at eval time) is the easy 80%; only `\pageref`/TOC-folios need pass two.

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

## I. Misc box/layout commands  [prim/pkg]

- `\parbox{w}{…}`, `\begin{minipage}{w}` → `\vbox to/spread` at a width. **[prim]**
- `\makebox`/`\mbox`/`\framebox`. **[prim/pkg]**
- `\newlength`/`\setlength`/`\addtolength` → typed Dimen vars via `\set`/`\calc`. **[pkg]**
- `\ifthenelse`/`ifthen` package, `\ifdefined`. **[pkg]** (over `\if`/`\ifx`/`\calc`)
- `siunitx` (`\SI`/`\num`/`\si`). **[pkg]** (number/unit formatting; large but pure)

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
