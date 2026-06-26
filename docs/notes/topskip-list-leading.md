# Bug memo: inflated interline leading on a list item that begins a page

**Status:** FIXED 2026-06-25 (`PageMode`/`VerticalMode`, regression test in `PageBreakPrimitivesTests`).
**Found:** 2026-06-25, while typesetting a half-letter document (`apologia`) with bulleted cards.
**Area:** page builder / interline glue (`PageMode.breakPage`, `VerticalMode.add`).

## Symptom

When a `\begin{itemize}` item wraps to two lines and its **first** line happens to be the
**first line on a page** (the item continues across a page break), the baseline-to-baseline
distance from that item's first line to its second line is **~2.9pt too large**.

Measured in the field document (half-letter, body leading 12.0pt):

- the offending item: line 1 → line 2 baseline gap = **14.89pt**
- every other line in the document: **12.05pt**

Only the *first* item at the top of the page is affected; later items on the same page are
spaced normally.

## Minimal reproduction (single pass — no cross-references needed)

```texish
\use{document}
\geometry paperwidth:5.5in paperheight:8.5in left:0.5cm right:0.5cm top:0.6in bottom:0.6in
\section{A}

// 35 one-line filler paragraphs (count matters — see below)
Filler paragraph 1 here now.

... (through 35) ...

\begin{itemize}
\item The vast majority of variants are spelling or word order; textual criticism reconstructs the wording with high confidence item 1.
... (six such items) ...
\end{itemize}
```

Render and measure the first interline gap on page 2 (e.g. with `pdftotext -bbox`).

## What it depends on (and does NOT)

A scan of the filler-paragraph count (each filler is one line, so each step shifts the page
break by one line):

| filler lines | first page-2 interline gap |
|---:|---|
| 34 | 12.00 ✓ |
| 35 | **14.89 ✗** |
| 36 | 12.00 ✓ |
| 37 | **14.89 ✗** |
| 38 | 12.00 ✓ |
| 39 | **14.89 ✗** |

The defect alternates with the parity of where the break lands: it appears exactly when the
**first line of a two-line list item** is the first box on the new page (so its own second
line follows it at the top of the page).

Ruled out:
- **Not** hyphenation — persists with hyphenation off.
- **Not** the two-pass / `\contents` — reproduces in a single pass. (`\contents` only shifts
  content down so the break lands on a "bad" line; `\maketitle` alone never triggers it.)
- **Not** the running head — reproduces without a `headline`.
- **Not** list-straddle in general — a list whose break lands between items, or on a second
  line, is spaced correctly.

## Root cause (confirmed)

`PageMode.breakPage` re-contributes the carried overflow by feeding each box back through
`this.add` (so material taller than a page cascades into further breaks). But `VerticalMode.add`
**synthesises interline glue** before every box it receives — and the carried boxes *already*
carry the interline glue computed when they were first contributed. So the first carried line
pair ended up with **two** spacing boxes between them (the carried glue plus a fresh one),
doubling the leading there. Instrumented dump of the carried run and the rebuilt page:

```
carried = HBox(d=2.06), Penalty, Glue(2.89), HBox            // one interline glue, as contributed
page    = VSpaceBox(topskip), HBox, Penalty, Glue(2.89), Glue(2.89), HBox   // re-add added a SECOND
```

Baseline distance = descent 2.06 + 2.89 + 2.89 + ascent 7.05 = **14.89** — the observed value.
Only the first carried pair shows it; later pairs are governed by their own carried glue. Plain
paragraphs are unaffected because a paragraph materialises its line skips internally and is
contributed as already-spaced lines; the list path contributes item lines through `add`, which
is where the second glue was synthesised.

## The fix

A `preglued` flag on `VerticalMode`: when set, `add` appends boxes without synthesising interline
glue (the run already carries it). `PageMode.breakPage` sets it (save/restore, so cascaded breaks
nest correctly) only around the carried re-contribution. Ordinary contribution is unchanged.
Regression test: *"a list item carried across a page break is not given doubled interline glue"*
in `PageBreakPrimitivesTests` — the carried item's first-line advance must equal a freshly set
item's (fails as 14.0 vs 12.0 without the fix).

## Relation to the folio bug (fixed `1fd9827`)

Same surface (`\contents` + lists across a page break) surfaced both, but the causes are
distinct: the folio bug was a group-scoped `set` for `pageno`; this one is interline glue at
the top of a page. Independent fix.
