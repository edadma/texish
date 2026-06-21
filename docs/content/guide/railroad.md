---
title: "Railroad diagrams"
weight: 8
---

The `railroad` package turns a grammar written in W3C-style EBNF into railroad (syntax)
diagrams — one picture per rule, drawn over the [vector-graphics](/guide/graphics/) layer.
Load it with:

```texish
\use{railroad}
```

## Drawing a grammar

`\railroad` takes its grammar **verbatim** (like `\verb` or `\url`), so the EBNF specials —
`::=`, `|`, `?`, `*`, `+`, `(`, `)`, `"`, `'` — pass through untouched. Each rule becomes its
own self-contained diagram, complete with a bold rule-name title, so it needs no surrounding
spacing or paragraph commands:

```texish
\railroad{
  greeting ::= "hello" name ("," name)*
  name     ::= [A-Za-z] [A-Za-z0-9]*
}
```

## Grammar syntax

A grammar is a list of rules, each `name ::= expression`. An expression is built from:

| Notation | Meaning | Drawn as |
|----------|---------|----------|
| `name` | a nonterminal | a square box |
| `"…"` or `'…'` | a terminal (literal text) | a rounded stadium, in **bold** |
| `[…]` | a character class | a pointed hexagon, in **bold** |
| `a b c` | juxtaposition (sequence) | boxes in a row |
| `a \| b \| c` | alternation (choice) | stacked alternatives joined by a fork |
| `( … )` | grouping | (transparent — no box of its own) |
| `x?` | optional | the item with a skip rail bypassing it |
| `x*` | zero or more | a skip rail and a repeat loop |
| `x+` | one or more | a repeat loop |

A terminal that itself contains a quote is wrapped in the other quote: `'"'` is a literal
double-quote, `"'"` a literal single-quote. Spaces inside a terminal are kept (`"end of
line"` is one box). Labels are set in JetBrains Mono.

## Readable idioms

Two common grammar shapes are recognised and drawn the intuitive way rather than literally:

- **Separated lists** — `A ( sep A )*` (with `A` a single symbol), the usual "one or more
  `A` separated by `sep`" pattern, is drawn as `A` on the line with `sep` riding a return
  loop above it, instead of "`A` then zero-or-more of (`sep A`)". So
  `expression ::= term ( "|" term )*` shows a single `term` box with `|` on the loop.
- **Optional choices** — `( A | B | C )?` folds the skip into the choice itself, as an empty
  top branch on the main line with the alternatives forked below, rather than arching a
  separate bypass over a nested choice.

## Customising the look

Set any of these with `\set` before calling `\railroad`. Colours are the most useful: the
whole palette is derived from one base colour, with terminals and character classes drawn at
a lower [Oklch](/reference/commands/#macros-and-programming) lightness so they read as shades
of the same hue.

| Variable | Default | Effect |
|----------|---------|--------|
| `rrbase` | `#dde8fb` | base fill colour for the boxes |
| `rrtermdark` | `0.08` | how far to darken (lower Oklch L) a terminal box |
| `rrccdark` | `0.17` | how far to darken a character-class box |
| `rrlinecolor` | `black` | ink for the rails and box outlines |
| `rrface` | `jetbrains` | typeface for the labels |
| `rrfont` / `rrtitlefont` | `9` / `10` | label and rule-title point sizes |
| `rrboxh` / `rrpadx` | `20` / `11` | box height and horizontal padding |
| `rrhgap` / `rrvgap` | `15` / `11` | gaps between boxes in a row / stacked alternatives |

```texish
\set rrbase {#ffe6cc}      % a warm palette instead of the default blue
\railroad{ digit ::= [0-9] }
```
