---
title: "Tamil"
weight: 21
---

texish sets the Tamil script with the syllable shaping it needs. Like Devanagari, Bengali, Gurmukhi
and Telugu it is written left to right, so it needs no bidirectional reordering; what it needs is
work *inside* each syllable. The engine segments a run into orthographic syllables and shapes each
one — splitting the two-part vowel signs, moving the pre-base signs to where they are drawn, and
positioning the marks — all automatically from text typed in the usual Unicode order.

## The Tamil font

A Tamil face, Noto Serif Tamil, is bundled in a regular and a bold cut. Select it with `\font tamil`:

```texish
{\font tamil 14 regular
தமிழ் ஒரு அழகான மொழி.
}
```

```texish
{\font tamil 18 bold வணக்கம்!}
```

(As with the bundled Hebrew, Arabic, CJK and other Indic faces, the in-browser Scala.js build does
not ship the Tamil font, to keep the download small.)

## What the engine shapes

Everything below happens automatically for any run set in a Tamil font — you type the characters in
reading order and the engine does the rest.

**A silenced consonant keeps its pulli.** Tamil does not fold a virama-joined consonant into a
half-form as Devanagari does, nor subjoin it beneath the base as Telugu does. The virama stays on the
page as a dot above the letter — the *pulli* — and the consonant carrying it is a complete letter in
its own right: `க்`, `ம்`, `ன்`.

**A pre-base vowel sign moves before its own consonant, and no further.** Three signs are drawn to
the left of the consonant they belong to (`ெ`, `ே`, `ை`), and because a silenced consonant ahead of
the base is a letter rather than half of a conjunct, the sign stops at the pulli instead of leaping
over it. `க்கே` sets *ka*, pulli, the ee sign, *ka* — the vowel is drawn between the two consonants.
This is the one place Tamil parts company with Devanagari and Bengali, whose pre-base signs go to the
very front of the syllable.

**Two-part signs are split.** Three vowel signs are one character in memory and two signs on the
page: `ொ` and `ோ` as an e or ee sign before the base and an aa sign after it, `ௌ` as an e sign before
and a length mark after. The engine splits each so both parts land where the font expects them.

**The u signs fuse, and the i signs take width-matched forms.** `கு` and `கூ` are drawn as single
glyphs rather than a letter and a mark, and the short-i and long-i signs come in variants sized to
the consonant they follow — `தி` and `மி` use different forms of the same sign. Both are the font's
own post-base substitutions, and neither needs anything from the document.

**Only two conjuncts survive.** Tamil borrowed a handful of Sanskrit conjuncts and kept two in
ordinary use: `க்ஷ` and the sacred `ஸ்ரீ`. The font draws each as a single ligature, and because the
ligature swallows the pulli there is nothing left to stop a pre-base sign, so in `க்ஷே` the vowel
does reach the front.

**There is no reph.** Unlike Devanagari and Bengali, a word-initial *ra* with a virama does not rise
as a mark above the syllable. In `ர்க` the ra keeps its pulli and stays an ordinary letter.

**The short-i sign does not reorder.** Bengali's `ি` is drawn before its consonant; Tamil's `ி` is
drawn above and to the right of it and stays where it is typed. So does `ீ`.

## Setting a paragraph

Tamil sets in ordinary running text: the line breaker finds its breaks at the interword spaces
exactly as for Latin, and only the shaping within each word is special. A whole Tamil paragraph needs
nothing beyond selecting the font.

```texish
{\font tamil 12 regular
இது தமிழ் எழுத்தில் எழுதப்பட்டுள்ளது. ஒவ்வொரு எழுத்தும் ஒரு மெய்யெழுத்தும் அதனுடன்
சேர்ந்த உயிரெழுத்தும் இணைந்து உருவாகிறது.
}
```

Because Tamil sets left to right like a Latin text face, it uses the same TeX-style punctuation
shorthands: `` `` `` and `''` become curly quotes, `--`/`---` en/em dashes, and `...` an ellipsis.
(The right-to-left Hebrew and Arabic faces do not — their quote conventions differ, so those
documents type the marks literally.)

The bundled `scripts/tamil-demo.script` sets a full page covering each of these features.
