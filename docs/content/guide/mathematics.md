---
title: "Mathematics"
weight: 3
---

texish has a TeX math mode set in Latin Modern Math through an OpenType `MATH` table. Math
is delimited by dollar signs: a single `$…$` for inline math, a doubled `$$…$$` for a
display centered on its own line.

```
Inline, like $a^2 + b^2 = c^2$, or displayed:
$$ e^{i\pi} + 1 = 0. $$
```

## Scripts

`^` and `_` attach a superscript and subscript to the preceding atom. Braces group a
multi-token script.

```
$x^2$    $a_i$    $x_i^2$    $e^{-x^2}$    $\sum_{i=1}^{n}$
```

## Fractions and radicals

```
$\frac{a}{b}$            % a fraction
$a \over b$              % the infix form
$\sqrt{2}$               $\sqrt[3]{x}$        % square and higher roots
```

## Big operators and limits

```
$\sum_{i=1}^{n} i$       $\int_0^\infty f$
$\sum\limits_{i=1}^{n}$  % force stacked limits in inline style
```

In display style, the limits of `\sum`, `\prod`, and the like stack above and below by
default.

## Delimiters

`\left` and `\right` grow a delimiter to the height of the material between them.

```
$\left( \frac{a}{b} \right)$
$\left[ \sum_{i} x_i \right]$
```

## Accents

```
$\hat{x}$    $\vec{v}$    $\widehat{abc}$
```

## Roman text and calligraphic letters

`\text` sets upright words inside a formula; `\mathcal` sets calligraphic capitals.

```
$V = \text{volume}$        $\mathcal{C}$        $\mathcal{P}(S)$
```

## Matrices

```
$\matrix{ a & b \cr c & d }$       % unbracketed
$\pmatrix{ a & b \cr c & d }$      % parentheses
$\bmatrix{ a & b \cr c & d }$      % brackets
$\cases{ x & if positive \cr -x & otherwise }$
```

## Displayed equations with numbers

`\eqno` sets an equation number flush right on a display line.

```
$$ x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a} \eqno(1) $$
```
