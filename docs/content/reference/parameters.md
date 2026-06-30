---
title: "Parameters and Variables"
weight: 3
---

Every engine parameter is an ordinary variable. Read one with `\the\name` (or bare `\name`),
set one with `\set name {value}`, and use it in arithmetic with `\calc{…}`. Assignments are
local to the current group unless made `\global`; `\setlength` / `\addtolength` are the
LaTeX-named forms for the dimension-valued ones.

Dimensions accept any [unit](#units) (`pt`, `in`, `cm`, `mm`, `em`, `ex`); a bare number is
points. Glue is a natural size with optional `plus`/`minus` stretch and shrink
(`\set parskip {0pt plus 1pt}`). The defaults below are the values a fresh document starts with.

## Page and text area

| Variable | Default | Meaning |
|----------|---------|---------|
| `hsize` | `6.5in` | width of the text block (the line width) |
| `vsize` | `9in` | height of the text block (the page body) |
| `paperwidth` | `8.5in` | physical sheet width (LaTeX/pdfTeX; classic TeX has none) |
| `paperheight` | `11in` | physical sheet height |
| `hoffset` | `1in` | left margin — the text block's x offset from the sheet corner |
| `voffset` | `1in` | top margin — the text block's y offset from the sheet corner |
| `pageno` | `1` | the current page number (folio), advanced as pages ship |
| `topskip` | `10pt` | glue from the page top to the first baseline |
| `raggedbottom` | `0` | nonzero: pad short page bottoms with fil instead of stretching their glue |

The far margins are whatever is left over: `paperwidth − hoffset − hsize` and likewise vertically.
The `\geometry` primitive sets these as a group from margin/size options.

## Running heads and feet

| Variable | Default | Meaning |
|----------|---------|---------|
| `headsep` | `0.25in` | gap from the bottom of the running header to the top of the body |
| `footskip` | `0.25in` | gap from the bottom of the body to the top of the running footer |
| `topmark` | `""` | the last `\mark` of the previous page |
| `firstmark` | `""` | the first `\mark` on the page being shipped |
| `botmark` | `""` | the last `\mark` on the page being shipped |

A document supplies the header/footer content by defining `headline` / `footline` macros; `\the\pageno`
inside them is always the shipping page's number. The mark variables drive running heads that track
the section a page covers.

## Paragraph shape

| Variable | Default | Meaning |
|----------|---------|---------|
| `parindent` | `20pt` | first-line indent of a paragraph |
| `parskip` | `0pt plus 1pt` | vertical glue between paragraphs |
| `leftskip` | `0pt` | glue added at the left of every line |
| `rightskip` | `0pt` | glue added at the right of every line |
| `parfillskip` | `0pt plus 1fil` | glue that fills out a paragraph's last line |
| `hangindent` | `0pt` | hanging-indent width (negative indents the right) |
| `hangafter` | `1` | apply the hanging indent after this many lines |
| `pardir` | `0` | paragraph base writing direction: `0` left-to-right, `1` right-to-left (set by `\ltr`/`\rtl`) |

## Inter-line and inter-word spacing

| Variable | Default | Meaning |
|----------|---------|---------|
| `baselineskip` | `1.2 ×` font size | target distance between consecutive baselines |
| `lineskip` | `1pt` | baseline glue used when lines would otherwise be closer than `lineskiplimit` |
| `lineskiplimit` | `0pt` | the closeness threshold below which `lineskip` replaces `baselineskip` |
| `spaceskip` | font space | interword glue (reset when the font changes) |
| `xspaceskip` | `1.5 ×` font space | interword glue at a wider (sentence) space |

## Line breaking (Knuth–Plass)

| Variable | Default | Meaning |
|----------|---------|---------|
| `tolerance` | `200` | maximum acceptable line badness |
| `pretolerance` | `100` | first-pass tolerance, before hyphenation is tried |
| `hyphenpenalty` | `50` | penalty for breaking at a discretionary hyphen |
| `exhyphenpenalty` | `50` | penalty for breaking at an explicit hyphen |
| `linepenalty` | `10` | penalty charged per line, to prefer fewer lines |
| `adjdemerits` | `10000` | demerits for visually adjacent tight and loose lines |
| `emergencystretch` | `0pt` | extra per-line stretch in a final pass for paragraphs that will not justify |

## Page breaking

The page builder breaks by cost (badness + penalty); 0–9999 biases against a break here, `10000`
forbids one, `-10000` forces one.

| Variable | Default | Meaning |
|----------|---------|---------|
| `clubpenalty` | `10000` | against breaking after a paragraph's first line (orphans) |
| `widowpenalty` | `10000` | against breaking before a paragraph's last line (widows) |
| `interlinepenalty` | `0` | against breaking between any other two lines |
| `wrappenalty` | `10000` | against a break inside a wrapped figure's band |

## Display math

| Variable | Default | Meaning |
|----------|---------|---------|
| `abovedisplayskip` | `0.83em plus/minus 0.17em` | glue set above a displayed formula |
| `belowdisplayskip` | `0.83em plus/minus 0.17em` | glue set below a displayed formula |

## Footnotes

| Variable | Default | Meaning |
|----------|---------|---------|
| `footnotesep` | `1em` | space above the footnote separator rule |
| `footnotesize` | `0.8` | footnote text size, as a fraction of the surrounding font |
| `footnoteno` | `0` | the footnote counter, advanced by each `\footnote` |

## Floats

| Variable | Default | Meaning |
|----------|---------|---------|
| `floatsep` | `0.85em` | space between two floats stacked in a page's float area |
| `textfloatsep` | `1.4em` | space between the float area and the body text |
| `wrapsep` | `12pt` | gutter between a wrapped figure and the text flowing around it |

## Images

| Variable | Default | Meaning |
|----------|---------|---------|
| `imageScaling` | `1.0` | a global multiplier applied to placed image sizes |
| `imagessupported` | backend | read-only `1`/`0` flag: can this backend rasterize images? |

A document can branch on `imagessupported` to choose an inline bitmap where it is available and a
drawn fallback where it is not.

## Frame and rule lengths

These have no engine default; the document package and the framing primitives read them when set,
falling back to the LaTeX values shown.

| Variable | Fallback | Meaning |
|----------|----------|---------|
| `fboxsep` | `3pt` | padding between content and rule in `\fbox` / `\colorbox` |
| `fboxrule` | `0.4pt` | rule thickness for `\fbox` / `\fcolorbox` |

## Units

`pt` (the base unit, 1/72 in), `in`, `cm`, `mm`, `em` (the current font size), `ex` (its x-height).
