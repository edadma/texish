---
title: "Chinese, Japanese & Korean"
weight: 17
---

texish sets the CJK scripts — Chinese, Japanese and Korean — with the line breaking each needs and a
bundled face for each region. The three share the Han characters but draw them with region-specific
forms, and Korean adds its own Hangul syllables, so each region has its own cut of Noto Serif CJK.

## The problem CJK line breaking solves

Knuth's line breaker finds breaks at glue, penalties and discretionary hyphens. Latin prose is full of
interword glue, so it wraps naturally. **Chinese and Japanese** are written without spaces between
characters, so they offer the breaker nothing — the whole paragraph is one rigid box that runs off the
page. Following `xeCJK` and `luatexja`, texish inserts the missing break opportunities *between*
adjacent characters (as thin, stretchable glue, so the line still justifies) and applies **kinsoku**: a
closing mark (`。`, `、`, `」`, …) never starts a line, and an opening mark (`「`, `（`, …) never ends
one. The Knuth–Plass breaker itself is untouched. This happens automatically for any run of CJK text.

**Korean is different.** It is written *with* spaces between words, so its lines break at those spaces
exactly the way Latin does, and a Korean word is not split across a line. Its Hangul syllables are
therefore kept off the *free* per-character break path; only Chinese and Japanese get inter-character
breaks. A break inside a Korean word is still offered, but at a heavy penalty, so the breaker reaches
for one only where no break at a space will fit — the same bargain TeX strikes with hyphenation. That
is what keeps a run with no space in it (a long compound, a spaceless title, a very narrow column)
wrapping instead of running off the page.

## The bundled faces

| command | alias | script |
|---|---|---|
| `\font cjksc` | | Simplified Chinese |
| `\font cjktc` | | Traditional Chinese |
| `\font cjkjp` | `\font japanese` | Japanese — Japanese kanji forms, hiragana, katakana |
| `\font cjkkr` | `\font korean` | Korean — Hangul (and Hanja) |

Each has a regular and a bold cut (`\font japanese 12 bold`). The Japanese face draws the kanji their
Japanese way rather than the Chinese way, and the Korean face carries the Hangul the Chinese faces have
no glyph for at all. Any other CJK font can be brought in from disk with `\loadfont{name}{path}`.

```texish
{\font cjksc 12 regular 这是中文排版的示例。}

{\font japanese 12 regular これは日本語の組版の例です。}

{\font korean 12 regular 이것은 한국어 조판의 예입니다.}
```

(As with the other bundled complex-script faces, the in-browser Scala.js build does not ship the CJK
fonts, to keep the download small.)

The bundled `scripts/cjk-demo.script` sets a page of Chinese, and `scripts/japanese-korean-demo.script`
sets Japanese and Korean, covering the kinsoku breaking and the Korean word-space breaking.
