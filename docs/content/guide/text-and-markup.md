---
title: "Text and Markup"
weight: 2
---

## Input conventions

texish turns ASCII source into proper typography, as TeX does — the body font enables these
so they fire in ordinary running text:

| You type | You get |
|----------|---------|
| ``` ``…'' ``` | "curly double quotes" |
| `` `…' `` | 'curly single quotes' |
| `--` | en&#8211;dash |
| `---` | em&#8212;dash |
| `...` | an ellipsis (`\dots` also works) |
| `~` | a non-breaking space (a tie, as in `Figure~1`) |

## Commands and whitespace

Unlike TeX, a texish command does **not** swallow the whitespace that follows it. A space
after a command name stays in the text, so you write commands inline and the spacing comes
out as typed:

```texish
lines the way \TeX does, then \dots and on.
```

Because of this there is no control-space (TeX's `\ `): you never need `\TeX{}` or a trailing
backslash just to keep a following space — `\TeX does` already renders as "TeX does". Writing
`\TeX\ ` is in fact an error, since `\ ` is read as a command named space, which does not
exist. Use the empty group `{}` only when you genuinely need to *stop* a command's argument
or name early — e.g. to butt text directly against a command with no space, as in
`\TeX{}nically`.

## Emphasis and weight

```texish
\emph{emphasis}        % italic
\textit{italic}        \italic{…}
\textbf{bold}          \bold{…}
```

## Monospaced text

`\texttt` sets its argument in Latin Modern Mono at the surrounding size — for file names,
inline code, and the like:

```texish
The file \texttt{document.texish} defines \texttt{\\maketitle}.
```

Because the mono face is cut to sit with the roman body, the code matches the text around
it rather than looming over it.

## Subscripts and superscripts

`\textsub` and `\textsup` set a subscript or superscript in running text, sized from the
current font (so they scale with the body):

```texish
H\textsub{2}O          the 1\textsup{st}          x\textsup{2}
```

(The `chem` package's `\dn` / `\up` are these under chemistry-friendly names.)

## Colour

`\color` sets the pen for the rest of the current group; `\textcolor` colours just its
argument. A colour is a CSS name (`blue`, `darkred`, …), a `#RRGGBB` hex code, a `#RRGGBBAA`
hex code that carries its own alpha, or the keyword `transparent`.

```texish
\textcolor{firebrick}{a single red word}, then black again.
{\color{blue}the rest of this group is blue.}
```

To make a colour translucent, add an optional `[alpha]` (0–1) or use an eight-digit code —
the two are equivalent:

```texish
\textcolor[0.5]{firebrick}{a half-opaque word}
\textcolor{#b2222280}{the same, written as #RRGGBBAA}
```

`\pagecolor` sets the colour painted across the whole page, under all content — a solid
band, a translucent tint, or `transparent` to leave the page clear. It applies to the
document's pages, so give it in the preamble. A translucent or transparent page composites
correctly when the output is used as an overlay (for example a video lower-third):

```texish
\pagecolor[0.6]{black}   % a 60%-opaque black band the video shows through
\pagecolor{transparent}  % clear page; only the drawn content is opaque
```

The optional `[alpha]` and eight-digit codes work for the box fills too — `\colorbox` and
`\fcolorbox` (see [Boxes](/reference/commands/)) and the picture-mode `\fill` / `\stroke`.

## Clickable links

`\href` and `\url` produce **real clickable link annotations** in PDF output (and draw the
text blue on every backend). The URL is read verbatim, so a `//`, `~`, or `%` in the
address survives intact.

```texish
Visit the \href{https://example.org/docs}{documentation} for details, or go straight to
\url{https://example.org}.
```

`\href{url}{text}` links the display *text*; `\url{url}` typesets the address itself, in
the monospaced face, as a link to itself.
