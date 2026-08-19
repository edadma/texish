# LaTeX-in-texish — Scope

What it would take to give texish LaTeX's **authoring surface** — the commands a document
author types — as engine primitives where macros can't fake it, and otherwise as `\use`-able
packages written in the document language.

Statuses below were checked against the tree, command by command: the registered primitive
names in `shared/src/main/scala-3/io/github/edadma/texish/parser/Primitives*.scala`, the
`\def`s in `packages/*.texish`, and `docs/content/reference/commands.md`.

## Framing

texish is an **evaluated tree-walker** with typed values and **fixed catcodes**, not a
two-stage expansion machine. So LaTeX's expansion plumbing is *unnecessary, not missing* —
do NOT port `\expandafter`, `\noexpand`, `\edef`, `\protect`/robust-command machinery,
runtime catcode changes, `\makeatletter`. The goal is the authoring surface, not LaTeX's
internal `.ltx` implementation.

A second line, learned since: **a LaTeX command that is only a different name for something
texish already does is not worth adding.** Two ways to say one thing is a cost paid by every
reader forever. What earns its place is a capability that is missing, not a spelling that is
unfamiliar.

Each candidate below is tagged:

- **[done]** — implemented; the command exists today
- **[pkg]** — pure document-language package macros; no engine change
- **[prim]** — needs a small new engine primitive/helper
- **[ENGINE]** — needs a substantial engine feature (the real blockers)
- **[skip]** — deliberately not doing it; the reason is given

---

## A. LaTeX command-name compatibility layer  [mostly skip]

texish already has these capabilities under its own names. A compat package would buy muscle
memory and cost a second vocabulary; by the rule above, most of it is not worth it.

- `\newcommand`/`\renewcommand`/`\providecommand` → `\def` handles this, with optional and
  defaulted parameters already. **[skip]**
- `\vspace{d}`/`\hspace{d}` → `\vskip`/`\hskip`. **[skip]**
- `\smallskip`/`\medskip`/`\bigskip` **[done]**, `\newline`/`\\` **[done]** (`\cr`)
- `\hfill`/`\dotfill`/`\hrulefill` **[done]**; `\stretch{n}` → glue with a `fil` component. **[skip]**
- `\phantom`/`\hphantom`/`\vphantom`/`\smash` **[done]**
- `\linebreak`/`\nolinebreak`/`\pagebreak`/`\nopagebreak` → `\penalty`, and `\nobreak`/`\eject`
  are there. **[skip]**
- `\rule[lift]{w}{h}` — a rule of explicit dimensions in running text; `\hrule`/`\vrule` are
  vertical/horizontal-list items, so this one is a real gap rather than a renaming. **[prim]**
- `\ensuremath{…}` — run the body in math if not already there. Genuinely useful in a macro
  body that may be called from either mode. **[prim]**
- `\xspace` — a smart trailing space, which needs lookahead at the call site. **[prim]**

## B. Math enrichment — an `amsmath`-style package  [largely done]

- Math alphabets `\mathbb` `\mathbf` `\mathrm` `\mathsf` `\mathtt` `\mathfrak` `\mathcal` **[done]**
- Operators `\lim \sup \inf \max \min \det \gcd \deg \exp \ln \log \sin \cos \tan …` **[done]**,
  and `\operatorname` **[done]**. `\DeclareMathOperator` is `\def name {\operatorname{name}}`. **[skip]**
- `\binom`/`\dbinom`/`\tbinom`, `\dfrac`/`\tfrac` **[done]**, and `\frac` now takes
  `rule:`/`left:`/`right:`/`style:`, which covers every other fraction-like stack **[done]**
- `\overline`/`\underline` **[done]**; `\overbrace`/`\underbrace` **[done]**
- `\overrightarrow` and the extensible arrows (`\xrightarrow`) — the horizontal-variant path
  the braces use would carry these; an arrow that stretches over a label is the remaining
  case of it. **[prim]**
- `\substack`, `\boxed`, `\pmod`/`\bmod` **[done]**; `\stackrel` is
  `\mathrel{\mathop{…}\limits^{…}}` over primitives that all exist. **[skip]**
- `\bigl…\Biggr` — superseded by `\fence size:n`, one command in place of twelve. **[done]**
- Numbered display environments `equation`/`equation*`, `align`/`align*`, `gather`,
  `multline` — `cases` **[done]**, the matrix and `aligned` environments **[done]**, `\eqno`
  and `\leqno` **[done]**, `\halign` with `\tabskip` **[done]**. What is left is the numbering:
  an equation counter wired to `\eqno`, `\tag`, and `\eqref`. **[pkg]**, plus `\displaywidth`/
  `\displayindent` if the alignment is to reach the full measure. **[prim]**

## C. Tables — `booktabs` / `array` / spanning  [one engine gap, the rest packages]

`\tabular` (an l/c/r/`|` column spec with `\hline`, over `\halign`) exists **[done]**.

- `booktabs`: `\toprule`/`\midrule`/`\bottomrule`/`\cmidrule` → ruled `\noalign{\hrule}` at
  proper weights and spacing. **[pkg]** — the cheapest visible win left in the whole document.
- `\multicolumn{n}{spec}{text}` and `\multirow`. **[ENGINE]** — needs `\span`/`\multispan`
  column spanning in `HAlignMode`, which is still the one unbuilt alignment feature.
- `tabularx` (auto-width columns) — `\halign to:`/`spread` is not wired. **[prim]**
- `\arraystretch`, `p{width}` paragraph columns. **[prim]**

## D. Floats, captions, lists  [packages]

- `\listoffigures`/`\listoftables` **[done]**, `\caption` **[done]**, `figure`/`table` with
  placement **[done]**, `\wrapfigure`/`\wraptable` **[done]** — with shaped cutouts, which is
  past what `wrapfig` does.
- Float placement `[H]` (hard-here). **[pkg]** (over the existing placement spec)
- `subcaption`/`subfigure`. **[pkg]**
- `enumitem`-style list tuning (`\begin{enumerate}[label=…,leftmargin=…]`), `\setlist`. **[pkg]**
- `\caption*`, `\captionof`. **[pkg]**

## E. Cross-references & TOC  [done]

The aux store and two-pass driver (`ReferenceTable` + `Passes.untilStable`), with `\label`,
`\ref`, `\pageref`, `\eqref`, `\autoref`, `\nameref`, `\tableofcontents`, `\addcontentsline`,
`\listoffigures` and `\listoftables`. A label or contents entry buried in a float learns the
page its float ships on.

## F. Page layout & headers  [one engine gap]

- `geometry`-style margins → `\geometry`. **[done]**
- `fancyhdr` → `headline`/`footline` and `\mark` are there and `book.texish` uses them; a
  package that wraps them as `\fancyhead`/`\fancyfoot`/`\pagestyle` is optional. **[pkg]**
- `setspace` (`\onehalfspacing`/`\doublespacing`) → `baselineskip`. **[pkg]**
- `titlesec` section-format hooks. **[pkg]** (the sectioning macros are already redefinable)
- `multicol` / `\twocolumn`. **[ENGINE]** — `\columns{n}{…}` balances columns **[done]**, but as
  a single box within a page: a balanced block taller than a page is not yet split across
  pages, and that needs the page-builder/output-routine hook.
- `\marginpar`. **[done]** — a `document` package macro since v0.28.0, with `\marginparwidth`,
  `\marginparsep` and `\marginparstyle`; it needed no page-builder hook after all.

## G. Graphics transforms & framing  [done bar one]

`\fbox`/`\framebox`/`\colorbox`/`\fcolorbox`/`\boxed`, `\rotatebox`/`\scalebox`/
`\reflectbox`/`\resizebox`, `\raisebox`, `\color` — all **[done]**.

- `xcolor` `\definecolor` and its named colour models. **[prim]**

## H. Verbatim & code listings  [done]

`\verb` and the `verbatim`/`code` raw environments, with highlighting.

## I. Misc box/layout commands  [done bar two]

`\parbox`/`minipage`, `\mbox`/`\makebox`, `\newlength`/`\setlength`/`\addtolength`,
`\ifthenelse`/`\equal` — all **[done]**.

- `\ifdefined`. **[prim]**
- `siunitx` (`\SI`/`\num`/`\si`) — **[ENGINE-ish]**, and deferred: it needs a number formatter
  (grouping, rounding, exponents — no grouping primitive exists) plus a unit-macro algebra
  (`\kilo\gram\per\second`). A substantial standalone package, on its own pass.

---

## What this document cannot see

It audits LaTeX's surface, so a **TeX** primitive that texish lacks never appears in it — and
for a long time four of the most-typed ones did not. The math-mode gaps found by auditing
against TeX rather than LaTeX are now closed: `\displaystyle` and its three companions,
`\fence` (which covers TeX's `\big` family), `\overbrace`/`\underbrace`, `\vcenter`, `\leqno`,
the `mu` unit, and `\frac`'s general parameters.

Two smaller ones remain unbuilt, both narrow: `\mathchoice`, which picks one of four
renderings by the style in force, and `\nonscript`, which cancels a following space in script
styles. Neither has come up in a real document.

## Recommended sequencing

**Package spine (cheap, no engine risk):**

1. **booktabs** (C) — rules at proper weights; the most visible table improvement per line of code.
2. **Equation numbering** (B) — an equation counter over `\eqno`/`\leqno`, with `\tag` and `\eqref`.
3. **setspace, titlesec, enumitem, caption tuning** (F, D) — macros over registers that exist.

**Engine spine:**

4. **`\rule`, `\ensuremath`, `\ifdefined`, `\definecolor`** (A, G, I) — small, self-contained prims.
5. **`\halign to:`/`spread`, `p{width}`, `\arraystretch`** (C) — finish the alignment surface.
6. **`\span`/`\multispan` → `\multicolumn`/`\multirow`** (C) — the alignment engine gap.
7. **The page-builder hook** (F) — page-spanning `multicol`/`\twocolumn`; the deepest of these, and
   the one that unlocks the rest of the page-layout column. (`\marginpar`, once expected to need
   this, shipped without it.)
