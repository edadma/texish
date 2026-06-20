# texish in the browser

A KaTeX-style client-side demo of the texish SVG backend: texish source is laid out in the browser by the
Scala.js build and rendered to SVG, with glyphs filled as vector outlines from fonts embedded in the bundle.

## Build and serve

```sh
# from the repo root — produce js/target/scala-3.8.4/texish-opt/main.js
sbt texishJS/fullLinkJS

# serve over http (ES modules do not load from file://)
python3 -m http.server 8000

# then open http://localhost:8000/examples/web/
```

During development, `sbt texishJS/fastLinkJS` builds faster; point the import in `index.html` at
`texish-fastopt/main.js` instead of `texish-opt/main.js`.

## Browser shims

The engine reaches for a filesystem when resolving `\use`, so the Scala.js bundle statically imports Node's
`fs` and `path`. A browser cannot resolve those bare module specifiers, so `index.html` maps them to the
small stubs `node-fs-stub.js` and `node-path-stub.js` via an `importmap`. The `fs` stub reports that nothing
exists; the engine then falls back to the packages embedded in the bundle (it tolerates a host with no working
filesystem). No `\use` of an on-disk file works in the browser — only the embedded standard packages — which
is exactly what a self-contained web renderer wants.

## API

The build exports a `texish` object.

SVG (vector, scales infinitely; outline-filled text is soft at small sizes):

- `texish.renderToString(source)` — the first page of `source` as an `<svg>` document string.
- `texish.renderAllToStrings(source)` — every page, as an array of strings.
- `texish.render(source)` — the first page parsed into a live `<svg>` element to insert into the page.
- `texish.renderAll(source)` — every page, as an array of `<svg>` elements.
- `texish.autoRender(selector = ".texish")` — render every matching element in place from its text content.

Canvas (raster, drawn at the device pixel ratio; text uses the browser's hinted `fillText`, so it is as crisp
as native text). The math font is the SMaFL build of Latin Modern Math: its size-variant and assembly glyphs
carry private-use codepoints, so the tall surd, big operators and stretchy delimiters take the hinted path too,
not an outline fill. Canvas rendering is asynchronous because the fonts load into the browser first:

- `texish.renderToCanvas(source, container)` — returns a promise of the first page's `<canvas>` and, if
  `container` is given, appends it there.
- `texish.autoRenderCanvas(selector = ".texish")` — render every matching element to a canvas in place.

Use SVG for output you will scale or print; use canvas for crisp on-screen text. See `index.html` (SVG) and
`canvas.html` (canvas) for the same content rendered both ways.

## What ships in the bundle

The fonts are the Latin Modern stack — roman body text, the monospaced face, and Latin Modern Math — which is
the same Computer Modern look the PDF backend defaults to. The standard packages (`document`, `book`,
`logos`, `counters`, `theorem`, …) are embedded too, so `\use{…}` of them works with no filesystem. A face or
package outside the embedded set fails with a clear error rather than rendering wrong; widening the set means
adding it to the build's embed list.
