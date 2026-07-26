---
title: "Installation"
weight: 1
---

texish is cross-published for the JVM, Scala Native, and Scala.js. Use it as the standalone
command-line renderer, as a library in an sbt build, or in the browser through its Scala.js
build (see [Rendering in the Browser](/guide/browser-rendering/)).

## As a library

Add texish with the `%%%` operator so sbt selects the right platform artifact:

```scala
libraryDependencies += "io.github.edadma" %%% "texish" % "0.24.1"
```

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

## Requirements

- **Scala 3** for the library.
- For the native binary and PDF output, the system needs **Cairo**, **FreeType**, and
  **libjpeg-turbo** available to the linker (the Scala Native bindings link against them).
