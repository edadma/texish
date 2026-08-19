---
title: "Hyphenation"
weight: 4
---

Hyphenation is what lets a justified paragraph keep its spacing even. Without it the line breaker
can only stretch and shrink the spaces between whole words, so a long word arriving at the wrong
moment leaves a line full of gaps — and the narrower the measure, the worse it gets. With it the
breaker has a break inside almost every long word and can choose the one that costs least.

texish breaks words with Liang's algorithm, the one TeX uses, reading the same pattern files a TeX
installation reads. Nothing is hyphenated until a document says which language it is written in:

```texish
\usehyphenation{en-us}
```

That loads the patterns for a language tag and makes it the document's language. Everything set
afterwards is hyphenated by it, and `\language{tag}` switches back to a language already loaded —
useful in a document that quotes one language inside another.

## Where the patterns live

**Five languages are compiled into the texish binary** — `en-us`, `es`, `fr`, `it` and `pt` — so
they work on an installation with nothing on disk at all: an executable somebody downloaded and put
in a folder of its own still hyphenates English, Spanish, French, Italian and Portuguese.

**The other 73 are files**, in the `hyphenation/` folder an installation ships, beside `fonts/` and
`packages/`. `\usehyphenation` reaches both tiers the same way and searches the same places `\use`
searches for a module: beside the document, in the current directory, then under the texish home
and `$TEXISHHOME`. A document therefore names a language and does not have to know which tier it is
in — and a file on disk shadows the compiled-in copy of the same language, so an installation can
carry newer patterns than the binary was built with.

Naming a language whose file is not there says so, and says which thing is wrong:

```
\usehyphenation: texish bundles patterns for 'de-1996', but no hyphenation folder was found —
this installation is missing its hyphenation/ tree; \loadhyphenation{de-1996}{path} reads a
pattern file directly
```

That is a different sentence from the one a misspelled tag gets, because it is a different problem:
one is a broken installation, the other a typo, and they are fixed in different places.

## The languages

| Tag | Language | | Tag | Language |
|---|---|---|---|---|
| `af` | Afrikaans | | `kk` | Kazakh |
| `ar` | Arabic *(not hyphenated)* | | `kmr` | Northern Kurdish (Kurmanji) |
| `as` | Assamese | | `kn` | Kannada |
| `be` | Belarusian | | `la-x-classic` | Classical Latin |
| `bg` | Bulgarian | | `la-x-liturgic` | Liturgical Latin |
| `bn` | Bengali | | `la` | Latin |
| `ca` | Catalan | | `lt` | Lithuanian |
| `cop` | Coptic | | `ml` | Malayalam |
| `cu` | Church Slavonic | | `mn-cyrl` | Mongolian, Cyrillic |
| `cy` | Welsh | | `mr` | Marathi |
| `da` | Danish | | `mul-ethi` | Ethiopic-script languages |
| `de-1901` | German, traditional spelling | | `nb` | Norwegian Bokmål |
| `de-1996` | German, reformed spelling | | `nl` | Dutch |
| `de-ch-1901` | German, traditional Swiss spelling | | `nn` | Norwegian Nynorsk |
| `el-monoton` | Greek, monotonic | | `no` | Norwegian |
| `el-polyton` | Greek, polytonic | | `oc` | Occitan |
| `en-gb` | English, British | | `or` | Odia |
| `en-us` | English, American *(compiled in)* | | `pa` | Punjabi |
| `eo` | Esperanto | | `pi` | Pāli |
| `es` | Spanish *(compiled in)* | | `pl` | Polish |
| `et` | Estonian | | `pms` | Piedmontese |
| `eu` | Basque | | `pt` | Portuguese *(compiled in)* |
| `fa` | Persian *(not hyphenated)* | | `rm` | Romansh |
| `fi-x-school` | Finnish, school rules | | `ru` | Russian |
| `fi` | Finnish | | `sa` | Sanskrit |
| `fr` | French *(compiled in)* | | `sh-cyrl` | Serbo-Croatian, Cyrillic |
| `fur` | Friulan | | `sh-latn` | Serbo-Croatian, Latin |
| `ga` | Irish | | `sk` | Slovak |
| `gl` | Galician | | `sl` | Slovenian |
| `grc` | Ancient Greek | | `sq` | Albanian |
| `gu` | Gujarati | | `sv` | Swedish |
| `he` | Hebrew *(not hyphenated)* | | `ta` | Tamil |
| `hi` | Hindi | | `te` | Telugu |
| `hr` | Croatian | | `th` | Thai |
| `hsb` | Upper Sorbian | | `tk` | Turkmen |
| `ia` | Interlingua | | `tr` | Turkish |
| `is` | Icelandic | | `uk` | Ukrainian |
| `it` | Italian *(compiled in)* | | `vi` | Vietnamese *(not hyphenated)* |
| `ka` | Georgian | | `zh-latn-pinyin` | Mandarin Chinese, pinyin |

Four of them — Arabic, Persian, Hebrew and Vietnamese — ship a deliberately empty table, which is
upstream's way of saying the language is not hyphenated. Naming one succeeds and produces no
breaks, so a document may set its language honestly without having to know that.

## What a pattern file settles besides the patterns

A `hyph-utf8` file carries three things, and texish reads all three.

**The patterns.** Digits between letters, odd proposing a break and even forbidding one; a word is
scored by laying every matching pattern over it and keeping the highest digit at each position.

**An exception list.** The handful of words the patterns get wrong, spelled out by hand. English
has fourteen of them: `as-so-ciate`, `ta-ble`, `re-cog-ni-zance` — and `project`, written with no
hyphen at all, which is how TeX says *never break this word*.

**The two minima.** How many letters must be left before a break and after it. These differ by
language and are not decoration: English asks for three after, which is why `comput-er` is offered
by the patterns and appears in no English book. Reading the minima from the file is what makes each
language set the way its own typographers set it, rather than the way some default does.

## A pattern file of your own

`\loadhyphenation` reads a pattern file from a path, under whatever name you give it:

```texish
\loadhyphenation{hu}{patterns/hyph-hu.tex}
```

This is the way to use a language texish does not bundle, or your own patterns for one it does. The
file is read in the dialect `hyph-utf8` is written in — a `\patterns{…}` block, an optional
`\hyphenation{…}` exception list, a `hyphenmins:` header, one-argument macros, and `\input` of
another file beside it — so a file taken from a TeX installation works as it stands.

## Languages texish does not bundle

Ten of upstream's 88 files are not shipped. Nine of them — Czech, Indonesian, Hungarian, Romanian,
Latvian, Armenian, Macedonian, Serbian in Cyrillic, and Mongolian in the LMC encoding — are left
out because their licences do not let texish pass them on freely: some are GPL or LGPL only, and
two state no licence at all. The tenth, Ancient Greek in the Ibycus encoding, holds no patterns.

None of them is out of reach: fetch the file from
[hyphenation/tex-hyphen](https://github.com/hyphenation/tex-hyphen) and load it with
`\loadhyphenation`. Two of the nine also have a substitute already bundled — `sh-cyrl` covers
Serbian in Cyrillic, and `mn-cyrl` covers Mongolian.

`hyphenation/README.md` in the source tree records exactly which files are shipped, from which
upstream revision, and why each of the others is not.

## Scripts that break differently

Hyphenation is for scripts that write words with spaces between them. Chinese, Japanese and Korean
break between characters rather than inside words, and Thai has no spaces at all — both are handled
by the line breaker directly and neither needs patterns. See
[Chinese, Japanese & Korean](../cjk/) and the Thai section there.

## French spacing

Declaring French — `\usehyphenation{fr}`, or any regional tag built on it — also switches on French
spacing of high punctuation: a space before the colon, semicolon, exclamation and question marks and
inside guillemets, which neither stretches when the line is justified nor offers the breaker a place
to break. See the [command reference](../../reference/commands/#language).
