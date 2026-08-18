---
title: "Ethiopic"
weight: 20
---

texish sets the Ethiopic script — Amharic, Tigrinya and Ge'ez — and it is the one non-Latin script
here that needs no shaping at all. It is written left to right, so there is no bidirectional
reordering; it is not cursive, so there are no joined forms; and unlike every Indic script it has no
dependent vowel signs to reorder or position, because the vowel is written into the letterform
itself.

That is what makes it a syllabary rather than an alphabet. Where Devanagari writes a consonant and
hangs a matra on it, Ethiopic has a separate letter for each consonant-and-vowel pair: a base
consonant and its six vowel orders are seven distinct characters, drawn as variations on one shape.
The cost is a large character set — some 350 letters in daily use, spilling out of the main block
into three more — and the saving is that one codepoint is one glyph and nothing has to be rearranged.
Ethiopic text takes the plain path through the engine and breaks at its spaces, exactly as a roman
paragraph does.

## The Ethiopic font

An Ethiopic face, Noto Serif Ethiopic, is bundled in a regular and a bold cut. Select it with
`\font ethiopic`, or with the aliases `\font amharic` and `\font tigrinya`, which name the same
family:

```texish
{\font ethiopic 14 regular
አማርኛ በኢትዮጵያ የሚነገር ቋንቋ ነው።
}
```

```texish
{\font ethiopic 18 bold ሰላም!}
```

The face covers the Ethiopic block along with the Supplement and both Extended blocks, so it reaches
past Amharic to Tigrinya, Ge'ez and the smaller languages of the region. (As with the bundled Hebrew,
Arabic, CJK and Indic faces, the in-browser Scala.js build does not ship the Ethiopic font, to keep
the download small.)

## The seven orders

A letter's vowel is written into its shape. Each row below is one consonant through its seven
orders — *hä*, *hu*, *hi*, *ha*, *he*, *hə*, *ho* — and the family resemblance down each row is the
whole design of the script:

```texish
{\font ethiopic 16 regular
ሀ ሁ ሂ ሃ ሄ ህ ሆ
ለ ሉ ሊ ላ ሌ ል ሎ
መ ሙ ሚ ማ ሜ ም ሞ
}
```

Nothing about this needs the engine's help: each of those is one character in memory and one glyph on
the page.

## Punctuation and numerals

Ethiopic has its own marks and its own numerals, and they are typed literally — no ASCII shorthand
produces them:

| Mark | Character | Use |
|---|---|---|
| Wordspace | `፡` | between words, in traditional setting |
| Full stop | `።` | end of sentence |
| Comma | `፣` | |
| Semicolon | `፤` | |

The numerals `፩ ፪ ፫ ፬ ፭ ፮ ፯ ፰ ፱ ፲` are letters in their own right rather than positional digits, so
they are typed as characters and not built from ASCII digits.

The face also carries Latin, so a document can set a roman word or a page folio without dropping to
the fallback face. Because it sets left to right like a Latin text face, the TeX-style punctuation
shorthands work in it too: `` `` `` and `''` become curly quotes, `--`/`---` en/em dashes, and `...`
an ellipsis.

## Setting a paragraph

A whole Ethiopic paragraph needs nothing beyond selecting the font — the line breaker finds its
breaks at the interword spaces exactly as for Latin.

```texish
{\font ethiopic 12 regular
እያንዳንዱ ፊደል አንድ ተነባቢና አንድ አናባቢ አንድ ላይ ይዞ ይጻፋል። ስለዚህ የፊደሉ ቅርጽ ራሱ አናባቢውን ያሳያል።
}
```

The bundled `scripts/ethiopic-demo.script` sets a full page covering each of these features.
