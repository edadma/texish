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
libraryDependencies += "io.github.edadma" %%% "texish" % "0.25.0"
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
into a PDF (or one PNG per page) using the Cairo backend. See the
[command-line tool](/reference/cli/) reference for its options.

### From a release

Every [release](https://github.com/edadma/texish/releases) attaches a binary per platform —
`linux-x86_64`, `linux-arm64`, `macos-arm64` — and one platform-independent
`texish-<version>-share.tar.gz` holding the font catalogue and the packages. The binary alone is a
working renderer; the tarball is what adds the complex-script faces, the CJK cuts and the packages
beyond `base` and `document`.

```sh
V=<version>                      # the release to install, without the leading v
P=$HOME/.local                   # any prefix whose bin/ is on your PATH

curl -L -o "$P/bin/texish" \
  "https://github.com/edadma/texish/releases/download/v$V/texish-$V-macos-arm64"
chmod +x "$P/bin/texish"

curl -L "https://github.com/edadma/texish/releases/download/v$V/texish-$V-share.tar.gz" \
  | tar -xz -C "$P"
```

The tarball unpacks to `share/texish/fonts` and `share/texish/packages`, which is one of the layouts
[the binary looks for](#finding-an-installation) — so it belongs at the same prefix as `bin/`, and
there is nothing further to configure. The archive is about 92MB compressed, most of it the CJK
faces; a `.sha256` accompanies it on the release.

Intel Macs have no binary (that runner is too scarce to build on reliably) and neither does Windows,
since the PDF backend is Cairo-bound. Both can build from source, or use the library on the JVM for
PNG and SVG output.

### From source

```sh
sbt texishCli/nativeLink
```

The binary is produced at `cli/target/scala-3.8.4/texish-cli`. A checkout already has `fonts/` and
`packages/` in it, so a binary run from the source tree finds the whole catalogue with no tarball.

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
- **JetBrains Mono**, regular and bold — the face `\code` sets a listing in. `\code` is a primitive
  and its syntax grammars are compiled in, so the face has to be too.

That is the guaranteed baseline: running text, mathematics, the scripts Latin Modern does not cover,
and a syntax-highlighted source listing. A program that adds texish as a dependency and configures
nothing gets all of it.

Two **packages** are compiled in — what a document needs to be an ordinary document:

| package | why |
|---|---|
| `base` | the primitives layer the rest builds on |
| `document` | sectioning, lists, floats, the page furniture — includes `base` |

Mathematics is not among them because it needs no package: the operator names, modular forms and
implication arrows are built into the engine alongside `\frac` and the matrix environments.

Everything else — `diagram`, `plot`, `book`, `usfm`, `railroad`, `music`, … — resolves from a
`packages/` folder on disk. A package earns a place in the artifact only if it is basic enough to be
worth the weight in every build *and* can work from the core alone. `music` fails the second test as
well as the first: it sets notation from a SMuFL face, and those are catalogue fonts, so embedding it
would ship a module that resolves and then cannot draw a note.

### The catalogue — opt in

Everything else texish bundles — the complex-script faces (Hebrew, Arabic, Devanagari, Bengali,
Gurmukhi, Telugu, Tamil), the CJK cuts, the alternative text families (Gentium, Charis, EB Garamond, Noto,
…), the SMuFL music faces (Bravura, Petaluma) and the rest of JetBrains Mono's weight range — comes
to about 151MB, far too much to compile in. It lives in the source tree's `fonts/` folder, and a host
asks for it in two steps: say where the tree is, then load it.

```scala
Typesetter.home = "/opt/texish"   // set before constructing a typesetter

val t = new CairoPDFTypesetter("out.pdf")

t.loadBundledCatalogue()          // then ask for the families
```

`Typesetter.home` is the **texish home**: a directory holding the `fonts/` and `packages/` an
installation ships, and the programmatic equivalent of `$TEXISHHOME`. Both halves are found through
it — fonts as a search root, and modules under its `packages/` folder — so one setting covers a whole
installation. A program that can work out where its own files live never needs the environment
variable at all; see [Finding an installation](#finding-an-installation) below.

For fonts it is shorthand for one font source; a host with more to say registers them directly, and
they are consulted in the order registered:

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

## Finding an installation

A packaged program should not have to be told where its own files are. On Native, `Install.configure()`
locates the running executable and looks upward from it for a `share/texish/` or a `fonts/`/`packages/`
directory, setting `Typesetter.home` if it finds one:

```scala
Install.configure()               // before constructing a typesetter
```

It searches from the path the executable was *reached* by as well as the symlink-resolved one, and that
covers two different installations:

- **A program finding its own files.** The resolved path lands in the versioned directory holding both
  the binary and its data (`…/Cellar/texish/0.25.0/{bin,share}`), so nothing depends on a prefix's links
  being intact.
- **A program finding a dependency's.** A package manager links each program into a shared prefix and
  links each package's data alongside — so an application whose package depends on texish's finds
  `/opt/homebrew/share/texish/` from `/opt/homebrew/bin/itself`, even though its own versioned directory
  contains no texish data at all.

Either way there is no wrapper script and no environment variable. `$TEXISHHOME` still works as a
fallback, for a tree kept somewhere neither of those finds.

## Requirements

- **Scala 3** for the library.
- For the native binary and PDF output, the system needs **Cairo**, **FreeType**, and
  **libjpeg-turbo** available to the linker (the Scala Native bindings link against them).
