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

The **Latin Modern core** ships inside the artifact: the roman body face in its bold, italic,
slanted and small-caps cuts, the sans and typewriter roles of the same super-family, and Latin
Modern Math. That is the default look, and it needs no configuration on any platform.

Everything else texish bundles — the complex-script faces (Hebrew, Arabic, Devanagari, Bengali,
Gurmukhi, Telugu), the CJK cuts, and the alternative text families (Gentium, Charis, EB Garamond,
Noto, …) — lives in the source tree's `fonts/` folder and is loaded when it can be found. A
relative font path is looked for beside the document, then in the current working directory, then
under `Typesetter.fontsDir` if the host set one, then under `$TEXISHHOME`. So an application that
wants the full set ships the `fonts/` folder and points at its parent:

```scala
Typesetter.fontsDir = "/opt/texish"   // before constructing a typesetter — bundled faces load in the constructor
```

A family whose files are not found is simply not registered, and a document asking for it gets a
clear "typeface not found". A document's own `\loadfont` resolves through the same roots, so a
font kept beside the document is found no matter where the host was launched from.

## Requirements

- **Scala 3** for the library.
- For the native binary and PDF output, the system needs **Cairo**, **FreeType**, and
  **libjpeg-turbo** available to the linker (the Scala Native bindings link against them).
