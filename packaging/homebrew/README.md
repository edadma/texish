# Homebrew packaging

`texish.rb` is the formula, with the version and the four checksums left as `REPLACE_*` markers so
that this copy is not a place a release has to remember to bump.

**A release includes a formula update.** A release whose binaries are attached to GitHub but whose
formula still names the previous version is half a release — `brew install` and `brew upgrade` are
how most people actually get texish, and they would keep getting the old one.

## Cutting a formula for a release

```sh
tools/brew-formula.sh 0.27.0 -o /tmp/texish.rb
```

That fetches the release's four assets, hashes them, and fills in the template. Homebrew pins each
download by SHA256 and GitHub does not publish those, so they have to come from the assets
themselves; the share tarball ships its own `.sha256`, which saves fetching 96MB just to hash it.
The release must already carry all four assets — the "Release binaries" workflow attaches them when
a release is published.

The release workflow runs the script itself and pushes the result, so running it by hand is for a
release made outside that path, or for checking what the workflow will produce.

## The tap

`edadma/homebrew-tap` — the same tap that carries the Roamer, Caldera and Asteroids casks. The
formula goes at `Formula/texish.rb`; that is the whole of it, Homebrew needs nothing else.

```sh
brew tap edadma/tap
brew trust edadma/tap
brew install edadma/tap/texish
brew test texish
```

`brew trust` is needed since Homebrew began refusing to load anything from an untrusted third-party
tap.

**Homebrew on the development machine belongs to the `work` account**, so all of the above is run
from a `work` shell. Running it as another user fails partway and leaves files that break the *next*
brew command, with an error naming something unrelated. Pushing the formula needs none of that — a
tap is an ordinary git repository.

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
- **A reproducible share tarball.** The release workflow builds it with a fixed member order, zeroed
  timestamps and a gzip header carrying no mtime, so re-running the workflow against an existing tag
  produces a byte-identical archive — otherwise `--clobber` would replace the asset with an
  equivalent one whose checksum no longer matches the published formula.

## The test block

`brew test texish` renders a probe that uses `\use{usfm}` and sets a word in Hebrew. Neither is
compiled into the binary, so the test passes only if the share resource landed where self-location
finds it — which is the one thing about this formula that could silently be wrong.
