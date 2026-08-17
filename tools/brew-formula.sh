#!/bin/sh
# Render the Homebrew formula for a published release.
#
# Homebrew pins every download by SHA256 and GitHub does not publish those, so they have to come
# from the assets themselves. This fetches them, hashes them, and fills in the template at
# packaging/homebrew/texish.rb — the whole of what "cut a formula for this release" means.
#
#   tools/brew-formula.sh 0.26.0              # write the formula to stdout
#   tools/brew-formula.sh 0.26.0 -o out.rb    # ...or to a file
#
# The release must already carry its four assets (the three binaries and the share tarball), which
# the "Release binaries" workflow attaches when a release is published. The share tarball ships its
# own .sha256, so that one is read rather than recomputed — it saves fetching 92MB to hash it.
#
# The result goes to Formula/texish.rb in the edadma/homebrew-tap repository; see
# packaging/homebrew/README.md. The release workflow runs this and pushes for you when the tap
# token is configured, so running it by hand is for a release made outside that path.

set -eu

usage() {
  echo "usage: $0 <version> [-o <file>]" >&2
  exit 2
}

[ $# -ge 1 ] || usage

V=$1
shift
OUT=-

while [ $# -gt 0 ]; do
  case $1 in
    -o) [ $# -ge 2 ] || usage; OUT=$2; shift 2 ;;
    *)  usage ;;
  esac
done

BASE=https://github.com/edadma/texish/releases/download/v$V
TEMPLATE=$(dirname "$0")/../packaging/homebrew/texish.rb

[ -f "$TEMPLATE" ] || { echo "$0: no template at $TEMPLATE" >&2; exit 1; }

# Hash one asset by streaming it: these are ~28MB each and there is no reason to keep them. macOS has
# shasum and no sha256sum; a Linux CI runner has both, so prefer whichever is there.
if command -v shasum >/dev/null 2>&1; then
  sha256() { shasum -a 256; }
else
  sha256() { sha256sum; }
fi

hash_asset() {
  curl -fsSL "$BASE/$1" | sha256 | cut -d' ' -f1
}

echo "$0: hashing the assets of v$V" >&2

MACOS_ARM64=$(hash_asset "texish-$V-macos-arm64")
LINUX_X86_64=$(hash_asset "texish-$V-linux-x86_64")
LINUX_ARM64=$(hash_asset "texish-$V-linux-arm64")

# The share tarball publishes its own checksum beside it, in `sha256sum` format: hash then filename.
SHARE=$(curl -fsSL "$BASE/texish-$V-share.tar.gz.sha256" | cut -d' ' -f1)

for h in "$MACOS_ARM64" "$LINUX_X86_64" "$LINUX_ARM64" "$SHARE"; do
  case $h in
    ????????????????????????????????????????????????????????????????) ;;
    *) echo "$0: expected a 64-character sha256, got '$h' — is v$V published with all four assets?" >&2
       exit 1 ;;
  esac
done

render() {
  sed -e "s/REPLACE_VERSION/$V/g" \
      -e "s/REPLACE_MACOS_ARM64/$MACOS_ARM64/" \
      -e "s/REPLACE_LINUX_X86_64/$LINUX_X86_64/" \
      -e "s/REPLACE_LINUX_ARM64/$LINUX_ARM64/" \
      -e "s/REPLACE_SHARE/$SHARE/" \
      "$TEMPLATE"
}

if [ "$OUT" = - ]; then
  render
else
  render > "$OUT"
  echo "$0: wrote $OUT" >&2
fi
