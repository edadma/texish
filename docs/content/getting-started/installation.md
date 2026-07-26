---
title: "Installation"
weight: 1
---

texish is cross-published for the JVM and Scala Native. Use it as the standalone command-line
renderer or as a library in an sbt build. (The browser backends are still in the source tree but
are not currently built or published — see [Rendering in the Browser](/guide/browser-rendering/).)

## As a library

Add texish with the `%%%` operator so sbt selects the right platform artifact:

```scala
libraryDependencies += "io.github.edadma" %%% "texish" % "0.24.2"
```

Nothing else is needed: the Latin Modern core and the standard packages are compiled into the
artifact, so the dependency alone gives you a working engine. There is no font tree to install
and no environment variable to set. See [Fonts](#fonts) below for the wider bundled set.

The library gives you the engine and the `parser` layer — construct a typesetter for your
target backend, feed it a source document, and flush it. On the JVM the backend is a
Graphics2D raster typesetter; on Scala Native it is the Cairo PDF and image backends.

### Errors

A fault in the document raises `io.github.edadma.texish.TexishException`, carrying the message,
the source position (`pos`), and a formatted excerpt pointing at the offending token:

```
font for typeface 'nosuchfont' not found (line 5, column 2):
{\font nosuchfont 12 regular oops}
 ^
```

Catch it to report the mistake to whoever wrote the document. Anything else escaping the engine
is a defect in texish rather than in the input: such a failure still arrives as a
`TexishException` located at the token being handled, but its message is prefixed
`internal error (…)` and the original exception is kept as the cause, so the stack trace that
locates the bug survives. The two are worth telling apart — one is your user's to fix, the other
is a bug report.

## As a command-line tool

The Scala Native build links a standalone `texish` executable that turns a source document
into a PDF (or one PNG per page) using the Cairo backend. Build it with:

```sh
sbt texishNative/nativeLink
```

The binary is produced at `native/target/scala-3.8.4/texish`. See the
[command-line tool](/reference/cli/) reference for its options.

## Fonts

Fonts come in two tiers: a **core** compiled into the artifact, and a **catalogue** loaded from a
font tree on disk.

### The core — always there

The core ships inside the artifact and needs no configuration on any platform:

- **Latin Modern** — the roman body face in its bold, italic, slanted and small-caps cuts, plus the
  sans and typewriter roles of the same super-family.
- **Latin Modern Math** — the default math font.
- **New Computer Modern** — the glyph-fallback face, in all four cuts. A codepoint the body face has
  no glyph for (a Greek word, a Cyrillic name) is set from this instead of a missing-glyph box, and
  keeps the weight and slope of the text around it.

That is the guaranteed baseline: running text, mathematics, and the scripts Latin Modern does not
cover. A program that adds texish as a dependency and configures nothing gets all of it.

The standard packages are compiled in on the same terms — every package that can work from the core
alone. `music` is the exception: it sets notation from a SMuFL face, and those are catalogue fonts, so
embedding it would ship a module that resolves and then cannot draw a note. It comes from a
`packages/` folder on disk, alongside the fonts it needs.

### The catalogue — opt in

Everything else texish bundles — the complex-script faces (Hebrew, Arabic, Devanagari, Bengali,
Gurmukhi, Telugu), the CJK cuts, the alternative text families (Gentium, Charis, EB Garamond, Noto,
…) and the SMuFL music faces (Bravura, Petaluma) — comes to about 151MB, far too much to compile in.
It lives in the source tree's `fonts/` folder, and a host asks for it in two steps: say where the
tree is, then load it.

```scala
Typesetter.fontsDir = "/opt/texish"   // set before constructing a typesetter

val t = new CairoPDFTypesetter("out.pdf")

t.loadBundledCatalogue()              // then ask for the families
```

`Typesetter.fontsDir` is shorthand for one font source; a host with more to say registers them
directly, and they are consulted in the order registered:

```scala
t.registerFontSource(DirectoryFontSource("/opt/texish"))
t.registerFontSource(myOwnSource)     // anything that can produce a file or bytes for a path
t.clearFontSources()                  // or: use nothing but the embedded core
```

Loading the catalogue is tolerant, because a font tree may be partial: a family whose files no
source has is skipped rather than fatal. Naming such a family later then says so —

```
typeface 'hebrew' is one texish bundles, but its font files were not found —
no font source has 'fonts/NotoSerifHebrew/NotoSerifHebrew-Regular.ttf'
```

— and naming one when the catalogue was never loaded at all says *that* instead. Neither is the bare
"not found" a misspelled name earns, because the three are fixed in three different places.

A relative font path is resolved beside the document first, then through the registered sources, then
in the embedded core. So a document's own `\loadfont` finds a font kept beside it no matter where the
host was launched from, and a font tree carrying a core path shadows the compiled-in copy.

The command-line tool does all of this for you — see [Fonts](/reference/cli/#fonts) there.

## Requirements

- **Scala 3** for the library.
- For the native binary and PDF output, the system needs **Cairo**, **FreeType**, and
  **libjpeg-turbo** available to the linker (the Scala Native bindings link against them).
