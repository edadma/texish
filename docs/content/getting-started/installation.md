---
title: "Installation"
weight: 1
---

texish is two things, and which one you want decides how you install it.

**The command-line tool** turns a source document into a PDF, and is what most people mean by
installing texish. On macOS (Apple silicon) and Linux it is one command:

```sh
brew tap edadma/tap
brew trust edadma/tap
brew install edadma/tap/texish
```

That brings the fonts and packages with it — see [As a command-line
tool](#as-a-command-line-tool) for the other ways to get it, and for what is in the box.

**The library** embeds the engine in a program of your own; it is cross-published for the JVM and
Scala Native, and is described next. (The browser backends are still in the source tree but are not
currently built or published — see [Rendering in the Browser](/guide/browser-rendering/).)

## As a library

Add texish with the `%%%` operator so sbt selects the right platform artifact:

```scala
libraryDependencies += "io.github.edadma" %%% "texish" % "0.29.0"
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

### With Homebrew

```sh
brew tap edadma/tap
brew trust edadma/tap
brew install edadma/tap/texish
```

macOS on Apple silicon, and Linux on x86_64 and arm64. This is the shortest path: it installs the
binary, pulls in `cairo`, `freetype` and `jpeg-turbo`, and puts the font catalogue and the packages
where texish looks for them, so `\use{usfm}` and a line of Hebrew work immediately with nothing to
configure. `brew upgrade texish` moves you to the next release.

`brew trust` is a recent Homebrew requirement: it refuses to load anything from a third-party tap
until told the tap is trusted.

### From a release

Every [release](https://github.com/edadma/texish/releases) attaches a binary per platform —
`linux-x86_64`, `linux-arm64`, `macos-arm64` — and one platform-independent
`texish-<version>-share.tar.gz` holding the font catalogue, the packages and the hyphenation
patterns. The binary alone is a working renderer; the tarball is what adds the complex-script faces,
the CJK cuts, the packages beyond `base` and `document`, and every language's hyphenation but the
five compiled in.

```sh
V=<version>                      # the release to install, without the leading v
P=$HOME/.local                   # any prefix whose bin/ is on your PATH

curl -L -o "$P/bin/texish" \
  "https://github.com/edadma/texish/releases/download/v$V/texish-$V-macos-arm64"
chmod +x "$P/bin/texish"

curl -L "https://github.com/edadma/texish/releases/download/v$V/texish-$V-share.tar.gz" \
  | tar -xz -C "$P"
```

The tarball unpacks to `share/texish/fonts`, `share/texish/packages` and
`share/texish/hyphenation`, which is one of the layouts
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

The binary is produced at `cli/target/scala-3.8.4/texish-cli`. A checkout already has `fonts/`,
`packages/` and `hyphenation/` in it, so a binary run from the source tree finds the whole catalogue
with no tarball.

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
would ship a module that resolves and then cannot draw a note. `chess` fails it the same way, for the
same reason — its pieces are glyphs of a catalogue face.

### The catalogue — opt in

Everything else texish bundles — the complex-script faces (Hebrew, Arabic, Devanagari, Bengali,
Gujarati, Gurmukhi, Kannada, Telugu, Tamil, Ethiopic), the CJK cuts, the alternative text families (Gentium, Charis, EB Garamond, Noto,
…), the SMuFL music faces (Bravura, Petaluma), the chess-piece face (Noto Sans Symbols 2, under the
name `chess`) and the rest of JetBrains Mono's weight range — comes to about 153MB, far too much to compile in. It lives in the source tree's `fonts/` folder, and a host
asks for it in two steps: say where the tree is, then load it.

```scala
Typesetter.home = "/opt/texish"   // set before constructing a typesetter

val t = new CairoPDFTypesetter("out.pdf")

t.loadBundledCatalogue()          // then ask for the families
```

`Typesetter.home` is the **texish home**: a directory holding the `fonts/`, `packages/` and
`hyphenation/` an installation ships, and the programmatic equivalent of `$TEXISHHOME`. Every part is
found through it — fonts as a search root, modules under its `packages/` folder, hyphenation patterns
under `hyphenation/` — so one setting covers a whole installation. A program that can work out where its own files live never needs the environment
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

## Hyphenation patterns

Hyphenation is split the same way, and for the same reason. **Five languages are compiled into the
artifact** — `en-us`, `es`, `fr`, `it` and `pt` — so a binary with nothing beside it still
hyphenates them. **The other 73 are files** in a `hyphenation/` folder, one per language tag,
verbatim from `hyph-utf8`.

Unlike the font catalogue, they need no opt-in call: patterns are small, they are resolved one
language at a time by name, and there is nothing to register in advance. `\usehyphenation{de-1996}`
searches the same places `\use` searches for a module — beside the document, the current directory,
then `hyphenation/` under the texish home and `$TEXISHHOME` — and falls back to the compiled-in copy.
A language whose file is missing says which thing is wrong, exactly as a missing font family does.

See [Hyphenation](/guide/hyphenation/) for the language list, what a pattern file carries besides
its patterns, and the ten upstream files texish does not ship.

## Finding an installation

A packaged program should not have to be told where its own files are. On Native, `Install.configure()`
locates the running executable and looks upward from it for a `share/texish/`, or for a `fonts/`,
`packages/` or `hyphenation/` directory, setting `Typesetter.home` if it finds one:

```scala
Install.configure()               // before constructing a typesetter
```

It searches from the path the executable was *reached* by as well as the symlink-resolved one, and that
covers two different installations:

- **A program finding its own files.** The resolved path lands in the versioned directory holding both
  the binary and its data (`…/Cellar/texish/<version>/{bin,share}`), so nothing depends on a prefix's links
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
