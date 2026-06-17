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

## Emphasis and weight

```
\emph{emphasis}        % italic
\textit{italic}        \italic{…}
\textbf{bold}          \bold{…}
```

## Monospaced text

`\texttt` sets its argument in Latin Modern Mono at the surrounding size — for file names,
inline code, and the like:

```
The file \texttt{document.texish} defines \texttt{\\maketitle}.
```

Because the mono face is cut to sit with the roman body, the code matches the text around
it rather than looming over it.

## Colour

`\color` sets the pen for the rest of the current group; `\textcolor` colours just its
argument. A colour is a CSS name (`blue`, `darkred`, …) or a `#RRGGBB` hex code.

```
\textcolor{firebrick}{a single red word}, then black again.
{\color{blue}the rest of this group is blue.}
```

## Clickable links

`\href` and `\url` produce **real clickable link annotations** in PDF output (and draw the
text blue on every backend). The URL is read verbatim, so a `//`, `~`, or `%` in the
address survives intact.

```
Visit the \href{https://example.org/docs}{documentation} for details, or go straight to
\url{https://example.org}.
```

`\href{url}{text}` links the display *text*; `\url{url}` typesets the address itself, in
the monospaced face, as a link to itself.
