# Hyphenation patterns

TeX hyphenation patterns, one file per language tag, taken **verbatim** from the `hyph-utf8`
distribution — the same files a TeX installation loads.

| | |
|---|---|
| Upstream | <https://github.com/hyphenation/tex-hyphen> |
| Path | `hyph-utf8/tex/generic/hyph-utf8/patterns/tex/` |
| Revision | `5684c0f51c0b81133db2efbe60a408b4155a3ff5` (2026-02-24) |
| Files | 78 — see *What is not here* |

Every file is byte-identical to its upstream copy, licence header and all. Updating the set is a
copy of that directory, minus the files listed below, and nothing else; nothing here is generated,
edited or reformatted.

## What is not here

**Ten of upstream's 88 files are left out because their licence does not permit texish to pass them
on freely, and one because it holds no patterns.** Every file that ships here may be redistributed
by anyone, in source or binary form, without asking and without conditions on the rest of the work
it travels in.

| Not shipped | Why |
|---|---|
| `hyph-cs` (Czech), `hyph-id` (Indonesian), `hyph-mk` (Macedonian), `hyph-sr-cyrl` (Serbian, Cyrillic) | GPL only |
| `hyph-hy` (Armenian), `hyph-lv` (Latvian) | LGPL only |
| `hyph-hu` (Hungarian) | MPL / GPL / LGPL, and no permissive option among them |
| `hyph-ro` (Romanian), `hyph-mn-cyrl-x-lmc` (Mongolian, LMC encoding) | the file states no licence at all |
| `hyph-grc-x-ibycus` (Ancient Greek, Ibycus encoding) | carries no patterns — only a pointer to `ibyhyph.tex`, a legacy 8-bit file that is not part of this directory |

A copyleft licence is not a defect and these are all free software; the reason they are not here is
that texish ships as one tarball under one licence, and a file that puts conditions on the work it
is distributed with does not belong in it. Anyone who wants one of these languages can fetch its
file from upstream and load it with `\loadhyphenation`, which is exactly what that command is for.

Two languages lose less than the table suggests: Serbian in Cyrillic is also covered by
`hyph-sh-cyrl`, and Mongolian by `hyph-mn-cyrl` — the file dropped there is the variant for an
8-bit encoding texish never uses.

Four of the files — `hyph-ar`, `hyph-fa`, `hyph-he`, `hyph-vi` — carry an empty `\patterns{}`
block. That is upstream's deliberate statement that the language is not hyphenated, and it is why
naming one of them succeeds and produces no break points rather than failing.

## Which of these ship where

`hyph-en-us`, `hyph-es`, `hyph-fr`, `hyph-it` and `hyph-pt` are compiled into the texish artifact
(the whitelist is in `build.sbt`), so they work with no tree on disk. The rest resolve from this
directory, wherever an installation puts it — see the *Hyphenation* section of the guide.

## Licences

The patterns are not under one licence: each file carries its own in its header, and between them
they are MIT, LPPL, BSD 3-clause, the Unicode data licence, the Unlicense, public domain and the
all-permissive "copying and distribution … permitted in any medium without royalty", with a few
bespoke grants besides. What they have in common is the only thing that had to be common: none of
them restricts what texish may be distributed as. Read the header of the file you care about — that
header travels with the file precisely so that it can be read.
