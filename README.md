# texish

[![CI](https://github.com/edadma/texish/actions/workflows/ci.yml/badge.svg?branch=dev)](https://github.com/edadma/texish/actions/workflows/ci.yml)

A TeX-style document layout and rendering engine for Scala, cross-built for the JVM and Scala
Native. Body text is set in Latin Modern Roman, with TeX-style math mode set in the matching
Latin Modern Math through an OpenType `MATH` table. Math covers inline `$…$` and centered display
`$$…$$` (with `\eqno` equation numbers): atoms and spacing, super/subscripts, fractions (`\frac` and
the infix `\over`/`\atop`), radicals (including higher roots), stretchy delimiters, accents, big
operators whose limits stack in display style, and matrices (`\matrix`, `\pmatrix`, `\bmatrix`,
`\cases`). The standard math alphabets (`\mathbf`, `\mathit`, `\mathrm`, `\mathsf`, `\mathtt`,
`\mathbb`, `\mathfrak`, `\mathcal`) and the phantom/`\smash` spacing boxes round out the math surface.

It breaks paragraphs into lines and lines into pages the way TeX does — Knuth-Plass line breaking,
Liang hyphenation (78 bundled languages, five of them compiled into the binary), French
spacing of high punctuation (`\usehyphenation{fr}` — an unbreakable, non-stretching space before
`: ; ! ?` and inside `« »`), CJK line breaking with kinsoku rules (Chinese and Japanese `\font cjksc`/`cjktc`/`japanese`
with region-specific Han forms, and Korean `\font korean` — Hangul that breaks at word spaces), right-to-left scripts
(`\rtl`/`\ltr` — Hebrew and Arabic, with Unicode bidirectional reordering, mirrored brackets,
cursive Arabic joining and ligatures via GSUB, and GPOS positioning of niqqud and harakat vowel
marks), Devanagari (Hindi) syllable shaping (`\font devanagari` — conjuncts and half-forms, short-i
and reph reordering, nukta composition, and GPOS-positioned vowel signs), Bengali–Assamese syllable
shaping (`\font bengali` — conjuncts, pre-base i/e/ai reordering, two-part o/au decomposition,
reph and ya-phalaa, nukta composition, and GPOS-positioned vowel signs), Gujarati syllable shaping
(`\font gujarati` — conjuncts and half-forms, pre-base i reordering, and a reph that closes the
cluster), Gurmukhi (Punjabi)
syllable shaping (`\font gurmukhi` — subjoined pairin forms, pre-base sihari reordering, and
GPOS-positioned addak, tippi and vowel signs), Kannada syllable shaping (`\font kannada` — fused
consonant-vowel glyphs, subjoined ottakshara conjuncts, vowel signs moved back to a leading base,
nested decomposition of the composed vowel signs, and the arkavattu reph), Telugu syllable shaping (`\font telugu` — fused
consonant-vowel glyphs, subscript conjuncts built beneath a leading base, vowel signs moved back to
that base, two-part ai decomposition, and GPOS-positioned subscripts), Tamil syllable shaping
(`\font tamil` — pre-base signs placed before their own consonant rather than the whole syllable,
two-part o/oo/au decomposition, fused u signs and width-matched i signs, the ksha and shrii
ligatures, and GPOS-positioned pulli), the Ethiopic syllabary (`\font ethiopic`/`amharic`/`tigrinya`
— Amharic, Tigrinya and Ge'ez, which need no shaping at all), automatic glyph fallback (Cyrillic and Greek set in
ordinary text with no setup, matching the surrounding weight and slope), author break control
(`\discretionary` and leaders for dotted contents lines),
and cost-based page breaks with widow/orphan control, footnotes, balanced multi-column layout
(`\columns`), page imposition onto physical sheets (`\arrange` — saddle-stitch booklets and n-up), and glue/kern spacing in a point-space coordinate system. Documents are written in a small TeX-like language (a `parser`
layer over the engine's primitives, with macros, a standard prelude/"format", `\hbox`/`\vbox`,
`\kern`, `\lower`/`\raise`, the `\TeX` and `\TeXish` logos, units like `pt`/`in`/`em`/`ex`, and
more). Pages render through pluggable backends — a Graphics2D raster (image) backend on the JVM and
a Cairo image-and-PDF backend on Native (there are also SVG and HTML-canvas backends for the browser,
kept in the tree but not currently built — see [In the browser](#in-the-browser)).

The core faces and the `base`/`document` packages are compiled into the artifact, so texish works as a
plain library dependency: nothing to install, no font tree, no environment variable.

It also has a vector-graphics mode (see below) for figures drawn inline in the document — shapes,
freeform paths, transforms, and placed type — built on the same rendering pipeline as the text.

A document format (`\use{document}`) supplies the article furniture — title blocks, numbered
sections, cross-references and a table of contents (`\label`/`\ref`/`\pageref`, `\tableofcontents`,
resolved by typesetting the document twice over a shared label table), lists whose labels change with
depth, quotations, figures and tables with captions, footnotes, margin notes (`\marginpar`, in the
outer margin of a two-sided document), a numbered reference list with citations into it
(`thebibliography`/`\cite`, resolved through the same label table), and an alphabetical index
(`\index`/`\printindex`, gathered and page-merged) — and bundled packages
add clickable links and images (`\includegraphics`, `\href`/`\url` as real PDF annotations), QR
codes and Data Matrix symbols (`\use{qrcode}`, `\use{datamatrix}` — both encoders are written
in the document language, not built into the engine), text sub/superscripts, chemistry (`\ce` reaction equations and skeletal structures), data
plots (`\use{plot}` — line, scatter, bar, and function plots with labelled axes), railroad
syntax diagrams (`\use{railroad}` — a W3C-style EBNF grammar drawn as one diagram per rule),
node-and-edge diagrams (`\use{diagram}`, with `flowchart`, `automaton` and `er` presets —
block diagrams, flowcharts, state machines and entity-relationship diagrams), month
calendars (`\use{calendar}` — a month grid with leap-year arithmetic, a configurable week
start, and a today highlight), chess (`\use{chess}` — algebraic notation read and *played*, so a
diagram is the position the moves above it reached), and geographic maps (`\use{map}` — real longitude/latitude
linework, place labels, routes and regions in the Web Mercator projection).

The everyday conveniences `\use{document}` builds on — a font-size ladder, vertical skips,
single-line alignment, the alignment and quotation environments and the `\TeX` logos — live
in a lighter `\use{base}` package that loads on its own, without the article machinery, for a
caption, flyer, or title card. The inline shape commands (`\bold`, `\italic`, `\smallcaps`,
`\slanted`, and the family roles `\mono`, `\sans`, `\serif`) are engine primitives, available
with no package at all.

**Documentation: [texish.edadma.dev](https://texish.edadma.dev/).**

## Vector graphics

`\picture` opens a fixed-size drawing that flows in the text like any other box, so a diagram sits
beside prose and prints through the same backend as the page. Inside it, coordinates are **y-up with
the origin at the bottom-left** (the PostScript/TikZ convention); a bare number is a point, and unit
suffixes (`in`, `mm`, `pt`, `em`) are honoured. Shapes draw immediately in the current graphics
state, which `\group` saves and restores.

```
\picture width:3in height:2in {
  \fill{lightsteelblue} \stroke{steelblue} \linewidth{1.5pt}
  \rect{0.2in 0.2in 1in 1in}
  \circle{2in 0.9in 0.5in}
  \nofill \stroke{firebrick} \linetype{dashed}
  \line{0.2in 0.1in 2.6in 0.1in}
  \at anchor:south {1.4in 1.6in}{$y = x^2$}
}
```

The vocabulary:

- **State** (saved by `\group`): `\stroke{color}` / `\nostroke`, `\fill{color}` / `\nofill`,
  `\linewidth{d}`, `\linecap{butt|round|square}`, `\linejoin{miter|round|bevel}`, `\dash{on off …}`,
  and `\linetype{solid|dashed|dotted|dashdot}`.
- **Transforms**: `\translate{dx dy}`, `\scale{sx sy}`, `\rotate{degrees}`.
- **Shapes**: `\line`, `\rect`, `\circle`, `\ellipse`, `\polygon`, `\polyline`, `\arc`, `\arcn`.
- **Arrows**: `\arrow[head:… size:… heads:end|start|both]{a b}` draws a shaft from `a` to `b` capped
  with a head (the shaft is shortened to meet the head's back); `\arrowhead[head:… size:…]{a b}` places
  just a head at `b` pointing away from `a`. Heads are `stealth` (default), `triangle`, `bar`, and `dot`,
  drawn in the current `\stroke` colour.
- **Freeform paths**: `\path{ \moveto{x y} \lineto{x y} \curveto{c1x c1y c2x c2y x y} \close }`. An optional
  `arrow:end|start|both` (with the same `head:`/`size:`) caps the path with a head oriented to its true end
  tangent — for a Bézier the last control point toward the endpoint — so a curved arrow follows the curve.
- **Grouping & clipping**: `\group{ … }`, `\clip{ <path body> }`.
- **Placement**: `\at[anchor:…]{x y}{content}` drops fully typeset text or math at a coordinate (it
  stays upright over the y-flip), and `\glyph[anchor:…]{x y}{codepoint}` places a single marker glyph.
  Anchors are `center`, `north`/`south`/`east`/`west`, the four corners, and `baseline`.

Coordinates can be parenthesised as well as bare: `(x, y)` is Cartesian, `(angle:radius)` is polar
(degrees), and `(name)` is a point named earlier with `\coordinate{name}{(x, y)}`. Either form may be
made **relative to the current point**: `++(dx, dy)` offsets from it and then *advances* it, so steps
chain into a shape or path — `\polygon{(10,10) ++(40,0) ++(0,40) ++(-40,0)}` walks a square — while
`+(dx, dy)` offsets without moving it, for several spokes from one hub. `\point{(x, y)}` makes a
first-class point value (for `\set`, printed by `\the` as `(x, y)`), `\xof{coord}` / `\yof{coord}`
read a point's components back as numbers, and the point operators `\padd` / `\psub` / `\pscale` /
`\pnormalize` / `\pperp` / `\pmid` / `\pdist` do vector arithmetic on points — so geometry like a bond
shortened along its own axis is written directly, e.g. `\padd{(A)}{\pscale{\pnormalize{\psub{(B)}{(A)}}}{9}}`.

Coordinates may also be computed, not just literal — a bare variable `\x` is its value and arithmetic
like `\*{\x}{14}` or `\forloop.index` works — so a plot, a chart, or a chemical diagram is just a path
built in a `\for` loop. See the picture section of [`scripts/texish-demo.script`](scripts/texish-demo.script)
for a worked demo (shapes, a Bézier wave, a y=x² line graph, and more).

## Command-line tool

The command-line tool lives in a separate native-only project, `texish-cli`, so the published `texish`
library never carries the executable's entry point or its argument-parsing dependency. It links a
standalone `texish` executable that turns a source document into a PDF (or one PNG per page) using the
Cairo backend:

```
texish [options] [input-file]

  input-file                    texish source to typeset; reads standard input if omitted
  -o, --output <file>           output path (default: beside the input file, or out)
  -t, --type <pdf | png>        output type (default: pdf)
  -p, --paper <letter|legal|a3|a4|a5>  paper size (default: letter)
  -r, --resolution <sd|hd|fhd|dpi>  PNG resolution: a named size or a DPI number (72 = one pixel per point; default: hd)
```

```sh
texish doc.texish -o doc -p a4          # writes doc.pdf
texish doc.texish -t png -r fhd         # writes doc.png (or doc_1.png, doc_2.png, … for multiple pages)
texish frame.texish -t png -r 72        # 1 point = 1 pixel: a 1280x720pt page becomes a 1280x720 PNG
cat doc.texish | texish -o doc          # read the source from standard input
```

Every [release](https://github.com/edadma/texish/releases) attaches a binary for Linux (x86_64,
arm64) and macOS (arm64), plus a `texish-<version>-share.tar.gz` carrying the font catalogue, the
packages and the hyphenation patterns. Unpack the tarball at the same prefix as `bin/texish` — it lays down `share/texish/` — and
the binary finds it with no wrapper script and no environment variable.

Or build it: `sbt texishCli/nativeLink` produces `cli/target/scala-3.8.4/texish-cli`. To run it
straight from sbt during development: `sbt "texishCli/run doc.texish"`.

## In the browser

> **Not currently built or published.** The browser backends are kept in the tree (`js/`), and what
> they need — the fonts and packages compiled into the artifact — is now how every platform loads
> them, so bringing them back is a build-configuration change: add `JSPlatform` to the `crossProject`
> in `build.sbt` and restore the `.jsSettings` block. The rest of this section then applies as
> written. Meanwhile, render to PNG or SVG and serve the images.

texish runs in the browser through its Scala.js build, so a web page can typeset math — and whole
documents — on the client the way [KaTeX](https://katex.org/) does: no server, no pre-baked images,
and no fonts to download separately (the Latin Modern text and math fonts and the standard packages
are embedded in the build).

Link the browser bundle with `sbt texishJS/fullLinkJS` (produced at
`js/target/scala-3.8.4/texish-opt/main.js`), copy it to your site, and import it. The bundle exposes
a `texish` object as a named export. The math entry point is `renderMath(source, container)`, where the
source is a math fragment — `$…$` for inline, `$$…$$` for a centered display — which the source itself
distinguishes:

```html
<p>The roots are <span id="quad"></span>.</p>
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

`renderMath` draws to a `<canvas>` using the browser's hinted text rasterizer, so on-screen text is
as crisp as native text; `renderMathSvg` is the resolution-independent SVG counterpart for output you
will scale or print. `autoRender` / `autoRenderCanvas` walk the page and render every matching element
in place. The `importmap` and the two stub files (copied from `examples/web/`) let the bundle run in a
browser, where there is no filesystem — full details, the API table, and runnable examples are in
[Rendering in the Browser](https://texish.edadma.dev/guide/browser-rendering/) and `examples/web/`.

## Installation

The command-line tool installs from Homebrew, with the font catalogue and the packages:

```sh
brew tap edadma/tap
brew trust edadma/tap
brew install edadma/tap/texish
```

macOS on Apple silicon and Linux on x86_64 or arm64; every release also attaches a plain binary per
platform. As a library, texish is cross-published for the JVM and Scala Native — add it to an sbt
build with the `%%%` operator so the right platform artifact is selected:

```scala
libraryDependencies += "io.github.edadma" %%% "texish" % "0.29.0"
```

That is the whole setup. There is no font tree to install and no environment variable to set. Compiled
into the artifact are the core faces — Latin Modern with its sans and typewriter roles, Latin Modern
Math, New Computer Modern for glyph fallback (Greek, Cyrillic), and JetBrains Mono for `\code`
listings — and the two packages a document needs to be an ordinary document: `base` and `document`.
Mathematics needs no package at all; it is part of the engine.

The wider bundled set — the complex-script faces, the CJK cuts, the alternative text families, the
SMuFL music faces — is 152MB and lives in this repository's `fonts/` folder, alongside the remaining
packages (`diagram`, `plot`, `book`, `usfm`, `music`, …) and the hyphenation patterns for every
language but the five compiled in; each release ships all three as a
`texish-<version>-share.tar.gz`. Point `Typesetter.home` at the directory holding them and call
`loadBundledCatalogue()` to make the fonts available — the packages and the patterns need no such
call, since each is resolved by name when a document asks for it. On Native,
`Install.configure()` works that out for you by locating the running executable, so an installed
program — the command-line tool, or an application whose package depends on texish's — needs no
wrapper script and no environment variable. See
[Installation](https://texish.edadma.dev/getting-started/installation/).

## License

ISC. See [LICENSE](LICENSE).
