---
title: "Rendering in the Browser"
weight: 12
---

texish runs in the browser through its Scala.js build, so a web page can typeset math and
whole documents on the client the way [KaTeX](https://katex.org/) does — no server, no
pre-baked images, and no fonts to download separately. The Latin Modern text and math fonts
and the standard packages are embedded in the build, and glyphs are drawn from the font's
own outlines. The most common use is dropping a single formula into a page; that is where
this guide starts.

## Building the bundle

The browser build is the Scala.js artifact, linked into a single JavaScript module. From the
texish repository:

```sh
sbt texishJS/fullLinkJS
```

This produces `js/target/scala-3.8.4/texish-opt/main.js` (use `fastLinkJS` during
development — it links faster but does not optimize). Copy that `main.js` to your site and
import it; it exposes a `texish` object as a named export.

ES modules only load over HTTP, not `file://`, so serve the page rather than opening it
directly:

```sh
python3 -m http.server 8000      # then open http://localhost:8000/
```

## A single formula

The math-specific entry point is `texish.renderMath(source, container)`. The source is a
texish math fragment — `$…$` for inline math, `$$…$$` for a display equation — which the
source already distinguishes. Rendering is asynchronous because the fonts load into the
browser first, so it returns a promise; pass a container element and it is appended there.

```html
<!DOCTYPE html>
<p>The roots are <span id="quad"></span> for a quadratic.</p>
<div id="euler"></div>

<script type="importmap">
{ "imports": { "fs": "./node-fs-stub.js", "path": "./node-path-stub.js" } }
</script>
<script type="module">
  const { texish } = await import('./main.js');

  // inline — flows in the sentence, aligned on the text baseline:
  await texish.renderMath("$x = \\frac{-b \\pm \\sqrt{b^2 - 4ac}}{2a}$", document.getElementById('quad'));

  // display — its own centered block:
  await texish.renderMath("$$ e^{i\\pi} + 1 = 0 $$", document.getElementById('euler'));
</script>
```

An inline formula is given a `vertical-align` computed from its rendered baseline, so it sits
on the surrounding text's baseline rather than its bottom edge riding the line — a subscript
or a fraction lines up correctly. A display formula is emitted as a centered block.

The `<script type="importmap">` block and the two stub files are explained under
[Browser shims](#browser-shims) below; they are needed because the engine reaches for a
filesystem when resolving packages.

## Rendering many formulas at once

To typeset every formula on a page, give each its source in a data attribute and loop:

```html
<p>An inline fraction <span data-math="$\frac{a}{b}$"></span> and a sum
   <span data-math="$\sum_{i=1}^{n} i$"></span> in a sentence.</p>

<script type="module">
  const { texish } = await import('./main.js');
  for (const el of document.querySelectorAll('[data-math]'))
    await texish.renderMath(el.getAttribute('data-math'), el);
</script>
```

## SVG instead of canvas

`renderMath` draws to a `<canvas>`, which uses the browser's own hinted text rasterizer, so
on-screen text is as crisp as native text. For output you will scale or print, use the SVG
counterpart `texish.renderMathSvg(source, container)` — it returns a resolution-independent
`<svg>` element, aligned on the baseline the same way. It is synchronous (SVG needs no font
preload):

```js
const svg = texish.renderMathSvg("$\\int_0^\\infty e^{-x^2}\\,dx = \\frac{\\sqrt{\\pi}}{2}$");
document.getElementById('integral').appendChild(svg);
```

Use canvas for crisp text on screen, SVG for output that scales or prints.

## Whole documents

The same backends render whole texish documents, not just formulas — sections, lists, the
`\use{document}` furniture, and the `\TeX`/`\LaTeX`/`\TeXish` logos all work from the embed.
`texish.autoRender(selector)` walks the page like KaTeX's auto-render, taking each matching
element's text as a source and replacing it with the rendered output (default selector
`.texish`):

```html
<div class="texish">\use{document}
\section{Hello}
Body text with inline math $a^2 + b^2 = c^2$ and a display:
$$ \sum_{k=1}^{n} k = \frac{n(n+1)}{2}. $$
</div>

<script type="module">
  const { texish } = await import('./main.js');
  texish.autoRenderCanvas();   // or texish.autoRender() for SVG
</script>
```

## API

The build exports a `texish` object with these methods.

Single inline / display math (the formula's `$…$` / `$$…$$` chooses inline vs display):

| Method | Returns | Notes |
| --- | --- | --- |
| `renderMath(src, container?)` | `Promise<canvas>` | hinted text; appends to `container` if given. Async. |
| `renderMathSvg(src, container?)` | `SVGElement` | resolution-independent; same baseline alignment. Synchronous. |

Whole documents:

| Method | Returns | Notes |
| --- | --- | --- |
| `render(src)` | `SVGElement` | first page as a detached element. |
| `renderAll(src)` | `SVGElement[]` | every page. |
| `renderToString(src)` | `string` | first page as an SVG document string. |
| `renderAllToStrings(src)` | `string[]` | every page. |
| `renderToCanvas(src, container)` | `Promise<canvas>` | first page, hinted text. Async. |
| `autoRender(selector = ".texish")` | — | render each matching element in place, to SVG. |
| `autoRenderCanvas(selector = ".texish")` | — | render each matching element in place, to canvas. |

Auto-render isolates failures per element: a source that does not lay out leaves its element
untouched rather than aborting the whole sweep.

## Browser shims

The engine resolves `\use` packages through a filesystem path, so the Scala.js bundle
statically imports Node's `fs` and `path`. A browser cannot resolve those bare module
specifiers, so the page maps them to two small stubs through an `importmap`:

```html
<script type="importmap">
{ "imports": { "fs": "./node-fs-stub.js", "path": "./node-path-stub.js" } }
</script>
```

The `fs` stub reports that nothing exists; the engine then falls back to the packages
embedded in the bundle (it tolerates a host with no working filesystem). So no `\use` of an
on-disk file works in the browser — only the embedded standard packages (`document`, `book`,
`logos`, `counters`, `theorem`, …) — which is exactly what a self-contained web renderer
wants. Copy `node-fs-stub.js` and `node-path-stub.js` from `examples/web/` in the repository.

## What ships in the bundle

The embedded fonts are the Latin Modern stack — roman body text in its core styles, the
monospaced face, and Latin Modern Math (the SMaFL build, whose size-variant and assembly math
glyphs carry private-use codepoints so even a tall surd or a big operator draws through the
hinted text path). A face or package outside the embedded set fails with a clear error rather
than rendering wrong.

The complete working examples are in `examples/web/` in the repository: `math.html` (inline
and display math in flowing text), `canvas.html` (a document on canvas), and `index.html`
(the same on SVG).
