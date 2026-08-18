---
title: "Chemistry"
weight: 7
---

The `chem` package adds chemical reaction equations and 2D structure drawings. Load it
with:

```texish
\use{chem}
```

## Reaction equations

`\ce` typesets a reaction from a compact, mhchem-style notation — and like mhchem it sets
the result **inline**, flowing in the running text:

```texish
Methane burns: \ce{CH4 + 2 O2 -> CO2 + 2 H2O}.
```

Species are separated by spaces. Within a species, a run of letters is an element symbol
(set upright), a digit run after letters is a subscript count, and a leading number is a
stoichiometric coefficient (set full size, with a thin space before its species). The
operator `+` and the arrows are math symbols, so they get the proper math spacing.

| You write | You get |
|-----------|---------|
| `\ce{CH4 + 2 O2 -> CO2 + 2 H2O}` | CH₄ + 2 H₂O ⟶ CO₂ + 2 O₂ |
| `\ce{N2 + 3 H2 <=> 2 NH3}` | N₂ + 3 H₂ ⇌ 2 NH₃ |

Arrow tokens: `->` (reaction), `<=>` (equilibrium), `<-`, `<->`. To show a reaction
centered on its own line, wrap it (at a larger size if you like) in `\centerline`:

```texish
\centerline{\font lmroman 14 regular \ce{CH4 + 2 O2 -> CO2 + 2 H2O}}
```

`\ce` is a pure document-language macro, not a built-in: it parses the notation and emits
math markup that ordinary math mode renders. Neutral species are fully supported; for an
ion, write the charge with `\up` outside `\ce` (a `^` cannot pass through a macro argument).

## Formula helpers

For a formula in running prose without `\ce`, the helpers set counts and charges directly:

```texish
a litre of H\dn{2}O        % subscript
the ion Na\up{$+$}         % superscript charge
sulfate SO\dn{4}\up{$2-$}
```

`\dn` and `\up` are the chemistry-friendly names for the engine's `\textsub` / `\textsup`.

## Structure drawings

Over the [vector-graphics](/guide/graphics/) layer, `chem` draws skeletal structures from
named coordinates: `\atom{name}{x y}{label}` places and labels an atom, and `\bond`,
`\dbond`, `\tbond` draw single, double, and triple bonds between two named atoms, shortened
at each end so the labels stay clear.

```texish
\picture width:2in height:1in {
  \atom{c}{0.4in 0.5in}{C}
  \atom{o}{1.4in 0.5in}{O}
  \dbond{c}{o}
}
```

Bond geometry is tunable through `bondgap` (the clearance at each end) and `doublesep` (half
the gap between the two lines of a double bond).
