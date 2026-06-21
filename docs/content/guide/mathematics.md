---
title: "Mathematics"
weight: 3
---

texish has a TeX math mode set in Latin Modern Math through an OpenType `MATH` table. Math
is delimited by dollar signs: a single `$…$` for inline math, a doubled `$$…$$` for a
display centered on its own line.

```texish
Inline, like $a^2 + b^2 = c^2$, or displayed:
$$ e^{i\pi} + 1 = 0. $$
```

To render math on a web page (KaTeX-style, in the browser), see
[Rendering in the Browser](/guide/browser-rendering/).

## Scripts

`^` and `_` attach a superscript and subscript to the preceding atom. Braces group a
multi-token script.

```texish
$x^2$    $a_i$    $x_i^2$    $e^{-x^2}$    $\sum_{i=1}^{n}$
```

## Fractions and radicals

```texish
$\frac{a}{b}$            % a fraction
$a \over b$              % the infix form
$\sqrt{2}$               $\sqrt[3]{x}$        % square and higher roots
```

## Big operators and limits

```texish
$\sum_{i=1}^{n} i$       $\int_0^\infty f$
$\sum\limits_{i=1}^{n}$  % force stacked limits in inline style
```

In display style, the limits of `\sum`, `\prod`, and the like stack above and below by
default.

## Delimiters

`\left` and `\right` grow a delimiter to the height of the material between them.

```texish
$\left( \frac{a}{b} \right)$
$\left[ \sum_{i} x_i \right]$
```

## Accents

```texish
$\hat{x}$    $\vec{v}$    $\widehat{abc}$
```

## Roman text and the math alphabets

`\text` sets upright words inside a formula, through the normal text path:

```texish
$V = \text{volume}$        $x \text{ for } x > 0$
```

The math alphabets remap their letters into the corresponding Unicode Mathematical Alphanumeric
block, so the same letter can be set in any of the standard math typefaces:

| Command | Alphabet |
|---------|----------|
| `\mathbf{…}` | bold |
| `\mathit{…}` | italic |
| `\mathrm{…}` | upright roman |
| `\mathsf{…}` | sans-serif |
| `\mathtt{…}` | monospace |
| `\mathbb{…}` | blackboard bold |
| `\mathfrak{…}` | fraktur |
| `\mathcal{…}` | calligraphic (script) |

```texish
$\mathbb{N} \subset \mathbb{Z} \subset \mathbb{R} \subset \mathbb{C}$
$\mathcal{F} : \mathfrak{A} \to \mathfrak{B}$        $\mathbf{x} \in \mathbb{R}^n$
```

A character an alphabet has no shape for — a digit in italic or fraktur, say — falls back to its
ordinary form.

## Phantoms and smash

A *phantom* reserves the size of its argument without printing it; `\smash` does the opposite,
printing the argument but reporting zero height and depth. They line things up that would not
otherwise align.

| Command | Effect |
|---------|--------|
| `\phantom{…}` | an invisible box the full size of its argument |
| `\hphantom{…}` | reserve only the width |
| `\vphantom{…}` | reserve only the height and depth |
| `\smash{…}` | draw the argument, but report zero height and depth |

```texish
$a \phantom{=} b$           % a gap exactly as wide as "="
$\smash{\frac{p}{q}} + r$   % a fraction that no longer spreads the line's spacing
```

## Matrices

```texish
$\matrix{ a & b \cr c & d }$       % unbracketed
$\pmatrix{ a & b \cr c & d }$      % parentheses
$\bmatrix{ a & b \cr c & d }$      % brackets
$\cases{ x & if positive \cr -x & otherwise }$
```

## Arrows and relations

Alongside `\to` / `\rightarrow` and the basic arrows, the long arrows and the equilibrium
harpoon are available as relations:

```texish
$\longrightarrow$   $\longleftarrow$   $\longleftrightarrow$   $\longmapsto$
$\rightharpoonup$   $\rightleftharpoons$
```

## Spacing

The TeX math-space commands insert a rigid space scaled to the font (a mu is 1/18 em):

| Command | Width |
|---------|-------|
| `\,` | thin (3 mu) |
| `\:` | medium (4 mu) |
| `\;` | thick (5 mu) |
| `\!` | negative thin (−3 mu) |

```texish
$f(x)\,dx$        $a\;b$        $\int\!f$
```

## Displayed equations with numbers

`\eqno` sets an equation number flush right on a display line.

```texish
$$ x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a} \eqno(1) $$
```
