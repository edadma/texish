---
title: "Mathematics"
weight: 4
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

## Style

A formula is set in one of four styles — display, text, script and scriptscript. Which one
you are in decides the type size and how scripts are placed: a display `\sum` stacks its
bounds above and below, while the same sum inline sets them beside it and at a smaller size.
`$…$` opens in text style and `$$…$$` in display style.

The four declarations switch style for the rest of the enclosing sub-formula, the way
`\bfseries` switches weight for the rest of its group. Braces bound the switch, because a
`{…}` in math is a sub-formula of its own:

```texish
The sum $\displaystyle \sum_{i=1}^{n} x_i$ sets its bounds above and below,
even here in running text.

$$ \sum_{i=1}^{n} {\textstyle \frac{1}{2}} x_i $$   % one small fraction in a big display
```

The declaration has to be inside the math, not around it: the braces that bound it are a
sub-formula's braces, so `{\displaystyle …}` is written within a `$…$`, never outside one.

| Declaration | Size |
|-------------|------|
| `\displaystyle` | the full size, with the open display gaps |
| `\textstyle` | the full size, inline spacing |
| `\scriptstyle` | the size a superscript is set at |
| `\scriptscriptstyle` | the size a script of a script is set at |

A switch keeps the crampedness of where it sits, so one made under a radical or in a
denominator still sets its superscripts at the lowered, cramped height.

## Fractions and radicals

```texish
$\frac{a}{b}$            % a fraction
$a \over b$              % the infix form
$\sqrt{2}$               $\sqrt[3]{x}$        % square and higher roots
```

`\frac` takes parameters that between them cover every fraction-like stack: the bar
thickness, a pair of fences around the stack, and the style to set it at.

| Parameter | Effect |
|-----------|--------|
| `rule:<dim>` | the bar thickness; `rule:0` stacks with no bar at all |
| `left:<delim>` `right:<delim>` | fences around the stack, sized to it |
| `style:display\|text\|script\|scriptscript` | set this one fraction at a chosen style |

```texish
$\frac rule:0 {n}{k}$                        % a bare stack
$\frac left:( right:) rule:0 {n}{k}$         % which is what \binom is
$\frac rule:1.2pt {a}{b}$                    % a heavier bar
$\frac style:display {1}{2}$                 % a big fraction in running text
```

`\dfrac`, `\tfrac`, `\binom`, `\dbinom` and `\tbinom` are the combinations common enough to
have names of their own.

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

That is the right rule whenever there is a formula between the two fences. A fence that
stands on its own has nothing to be sized from — an opening bracket whose partner is a line
away, the bar of a set-builder, a divider in a piecewise definition — and `\fence` sets one
at a size you choose instead:

```texish
$\{\, x \fence{|} x > 0 \,\}$              % a divider at the ordinary size
$\fence size:2 {(} \frac{a}{b} \fence size:2 {)}$
```

`size:0` is the plain glyph and each step up climbs to the font's next larger variant,
stopping at the largest it has; the default is `size:1`. The space around the fence follows
from the delimiter — an opener keeps none from what follows it, a symmetric fence like `|`
keeps a relation's space on both sides — and `class:open`, `class:close`, `class:rel` or
`class:ord` overrides that for a fence used against its usual sense.

## Accents

```texish
$\hat{x}$    $\vec{v}$    $\widehat{abc}$
```

## Braces over and under

`\overbrace` and `\underbrace` grow a brace to span whatever they cover. A script attached
to one rides centred over (or under) the brace rather than beside it, which is how the brace
gets its label:

```texish
$\overbrace{a + b + c}^{n \text{ terms}}$
$\underbrace{x_1 + x_2 + x_3}_{\text{the sum}}$
```

## Boxes on the math axis

The math axis is the invisible line a fraction bar sits on and a fence centres about.
`\vcenter` sets a box centred there rather than standing on the baseline, so a stack of
lines beside a formula reads level with it:

```texish
$x = \vcenter{\hbox{first}\hbox{second}}$
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

`mu` is also a unit in its own right, so a space that is not one of the four named ones is
written with the ordinary spacing commands — there is no separate math-skip command to
learn:

```texish
$x \hskip 3mu y$              % the same space \, gives
\set g {0mu plus 6mu}          % and it works in a glue spec too
```

## Displayed equations with numbers

`\eqno` sets an equation number flush right on a display line, and `\leqno` flushes it left.
The formula stays centred on the measure either way. Which side a document numbers on is a
house style, so it is normally set once, in the macro that wraps the display, rather than
chosen equation by equation.

```texish
$$ x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a} \eqno(1) $$
$$ e^{i\pi} + 1 = 0 \leqno(2) $$
```
