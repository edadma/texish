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

## API

The build exports a `texish` object:

- `texish.renderToString(source)` — the first page of `source` as an `<svg>` document string.
- `texish.renderAllToStrings(source)` — every page, as an array of strings.
- `texish.render(source)` — the first page parsed into a live `<svg>` element to insert into the page.
- `texish.renderAll(source)` — every page, as an array of `<svg>` elements.
- `texish.autoRender(selector = ".texish")` — render every matching element in place from its text content.

## What ships in the bundle

The fonts are the Latin Modern stack — roman body text, the monospaced face, and Latin Modern Math — which is
the same Computer Modern look the PDF backend defaults to. The standard packages (`document`, `book`,
`logos`, `counters`, `theorem`, …) are embedded too, so `\use{…}` of them works with no filesystem. A face or
package outside the embedded set fails with a clear error rather than rendering wrong; widening the set means
adding it to the build's embed list.
