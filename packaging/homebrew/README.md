# Homebrew packaging

`texish.rb` is the formula, with the version and the four checksums left as `REPLACE_*` markers so
that this copy is not a place a release has to remember to bump. Fill them in from a published
release and copy the result into the tap.

## Cutting a formula for a release

The workflow attaches four assets per release: three binaries and the share tarball. Homebrew pins
each by SHA256, and GitHub does not publish those, so they come from the downloads themselves. The
share tarball ships its own `.sha256`, which saves fetching 92MB just to hash it.

```sh
V=0.26.0   # whichever release you are packaging — this file does not track it
B=https://github.com/edadma/texish/releases/download/v$V

curl -sL "$B/texish-$V-share.tar.gz.sha256"          # the share resource's sha256
for a in macos-arm64 linux-x86_64 linux-arm64; do
  curl -sL "$B/texish-$V-$a" | shasum -a 256         # each binary's
done
```

The binaries are ~28MB each, so that loop moves about 84MB. Then substitute:

| marker | value |
|---|---|
| `REPLACE_VERSION` | `$V` |
| `REPLACE_MACOS_ARM64` | sha256 of `texish-$V-macos-arm64` |
| `REPLACE_LINUX_X86_64` | sha256 of `texish-$V-linux-x86_64` |
| `REPLACE_LINUX_ARM64` | sha256 of `texish-$V-linux-arm64` |
| `REPLACE_SHARE` | sha256 of `texish-$V-share.tar.gz` |

## The tap

The tap is a repository named `homebrew-texish` under the `edadma` account, with the filled-in
formula at `Formula/texish.rb`. That is the whole tap — Homebrew needs nothing else.

```sh
brew tap edadma/texish
brew install texish
brew test texish
```

`brew audit --strict --new texish` before the first publish is worth the minute it takes; it catches
the conventions this template cannot.

## What the formula relies on

- **Self-location.** The binary finds `share/texish` by walking up from its own path, through both
  the resolved Cellar path and the linked prefix, so there is no wrapper script and no
  `$TEXISHHOME`. `native/src/main/scala/io/github/edadma/texish/Install.scala` is the search.
- **A dependent package can find texish's tree too.** An application linked into the same prefix
  reaches `/opt/homebrew/share/texish/` from `/opt/homebrew/bin/itself`, which is what lets a
  formula for a program that embeds texish just `depends_on "texish"`.
- **A partial tree is not an error.** `loadBundledCatalogue` skips a family whose files are absent
  and says so only if a document names it, so trimming the tarball later would degrade rather than
  break.
