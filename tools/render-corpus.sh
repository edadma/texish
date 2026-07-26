#!/bin/sh
# Render every demo in scripts/ and report which ones fail.
#
# The point is the working directory. texish looks for packages beside the document, then in the
# current directory and ./packages, then under its home — so rendering the corpus from a checkout
# finds everything whether or not the installation would have. This runs each demo from an empty
# directory instead, which is what a user who downloaded a binary actually has, and is how three
# defects that ~1500 unit tests could not see were caught: a fallback face that was not embedded
# after all, \code unable to find its face, and a package that resolved only from the source tree.
#
#   tools/render-corpus.sh <texish-binary>              # core only: no font tree, no packages
#   tools/render-corpus.sh <texish-binary> --all        # every demo must render, however the
#                                                       #   binary finds its tree — use this on a
#                                                       #   binary installed in a prefix, and it
#                                                       #   tests self-location as well
#   tools/render-corpus.sh <texish-binary> --home DIR   # --all, with the tree given as $TEXISHHOME
#
# In the default mode the outcome is compared against tools/render-corpus.expected, which lists the
# demos that must render from the compiled-in core alone. A demo that moves in or out of that set
# is a change in what an unconfigured texish can do, and has to be an intentional one — so the
# comparison is exact in both directions, and the script exits non-zero on any difference. A demo
# that fails must also fail for the right reason: a missing face or a missing package, not a crash.

set -eu

usage() {
  echo "usage: $0 <texish-binary> [--all | --home DIR]" >&2
  exit 2
}

[ $# -ge 1 ] || usage

BIN=$1
shift
HOME_DIR=
ALL=

while [ $# -gt 0 ]; do
  case $1 in
    --all) ALL=1; shift ;;
    --home) [ $# -ge 2 ] || usage; HOME_DIR=$2; ALL=1; shift 2 ;;
    *) usage ;;
  esac
done

[ -x "$BIN" ] || { echo "$0: '$BIN' is not an executable" >&2; exit 2; }

BIN=$(cd "$(dirname "$BIN")" && pwd)/$(basename "$BIN")
REPO=$(cd "$(dirname "$0")/.." && pwd)
EXPECTED=$REPO/tools/render-corpus.expected

[ -z "$HOME_DIR" ] || HOME_DIR=$(cd "$HOME_DIR" && pwd)

# An empty directory to render from, so that nothing resolves through the current directory. The
# output lands here too, keeping the checkout clean.
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
cd "$WORK"

RENDERED=$WORK/rendered
BADLY=$WORK/badly
LOG=$WORK/log
: > "$RENDERED"
: > "$BADLY"

for script in "$REPO"/scripts/*.script; do
  name=$(basename "$script" .script)

  if [ -n "$HOME_DIR" ]; then
    TEXISHHOME=$HOME_DIR "$BIN" -o "$WORK/$name" "$script" > "$LOG" 2>&1 && status=0 || status=$?
  elif [ -n "$ALL" ]; then
    # Deliberately not clearing TEXISHHOME: this mode asks how the binary behaves as installed,
    # and self-location is the path that has no environment variable to lean on.
    "$BIN" -o "$WORK/$name" "$script" > "$LOG" 2>&1 && status=0 || status=$?
  else
    TEXISHHOME= "$BIN" -o "$WORK/$name" "$script" > "$LOG" 2>&1 && status=0 || status=$?
  fi

  if [ "$status" -eq 0 ]; then
    printf '  ok    %s\n' "$name"
    printf '%s\n' "$name" >> "$RENDERED"
  else
    # A texish failure opens with the diagnostic and the source position; the source excerpt and
    # the trace follow. Drop the exception class so the reason reads first, and print it with
    # printf rather than echo — the excerpt is full of backslashes.
    reason=$(head -n 1 "$LOG")
    reason=${reason#*TexishException: }
    printf '  FAIL  %s — %s\n' "$name" "$reason"

    # Failing is only acceptable for the reason we are testing for. A demo that needs a face or a
    # package it has not got must say exactly that; anything else — a crash, an internal error, a
    # parse failure — is a defect wearing the same exit code, and would otherwise sit here unnoticed
    # for as long as the demo stayed off the expected list.
    case $reason in
      *"is one texish bundles, but its font files were not found"*) ;;
      *"not found on the filesystem or among the embedded modules"*) ;;
      *) printf '%s: %s\n' "$name" "$reason" >> "$BADLY" ;;
    esac
  fi
done

echo

if [ -n "$ALL" ]; then
  total=$(ls "$REPO"/scripts/*.script | wc -l | tr -d ' ')
  count=$(wc -l < "$RENDERED" | tr -d ' ')
  where=${HOME_DIR:-"the tree the binary located itself"}

  if [ "$count" -eq "$total" ]; then
    echo "all $total demos render against $where"
    exit 0
  fi

  echo "$((total - count)) of $total demos failed with a tree available — every demo must render" >&2
  exit 1
fi

if [ -s "$BADLY" ]; then
  echo "these demos failed for a reason other than a missing face or package:" >&2
  cat "$BADLY" >&2
  exit 1
fi

sort -o "$RENDERED" "$RENDERED"
grep -v '^#' "$EXPECTED" > "$WORK/expected" || true

if diff -u "$WORK/expected" "$RENDERED" > "$WORK/diff"; then
  echo "the $(wc -l < "$RENDERED" | tr -d ' ') demos that render from the core alone are exactly the expected ones"
  exit 0
fi

echo "what renders from the core alone has changed (-expected +actual):" >&2
cat "$WORK/diff" >&2
echo >&2
echo "If this is intended, update tools/render-corpus.expected. A demo LEAVING the list means an" >&2
echo "unconfigured texish can do less than it could — check that before accepting it." >&2
exit 1
