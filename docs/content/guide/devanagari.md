---
title: "Devanagari (Hindi)"
weight: 15
---

texish sets Devanagari — the script of Hindi, Marathi and Sanskrit — with the syllable shaping
the script needs. Devanagari is written left to right, so unlike Hebrew and Arabic it needs no
bidirectional reordering; what it needs is work *inside* each syllable. A consonant carries an
inherent vowel; a vowel sign attaches above, below, before or after it; and two consonants joined
by the virama fuse into a conjunct or a half-form. The engine segments a run into orthographic
syllables and shapes each one — building the conjuncts, reordering the signs that are written and
drawn in different places, and positioning the marks — all automatically from text typed in the
usual Unicode order.

## The Devanagari font

A Devanagari face, Noto Serif Devanagari, is bundled in a regular and a bold cut. Select it with
`\font devanagari` (or the alias `\font hindi`):

```texish
{\font devanagari 14 regular
हिन्दी भारत की एक प्रमुख भाषा है।
}
```

```texish
{\font devanagari 18 bold नमस्ते!}
```

(As with the bundled Hebrew, Arabic and CJK faces, the in-browser Scala.js build does not ship the
Devanagari font, to keep the download small.)

## What the engine shapes

Everything below happens automatically for any run set in a Devanagari font — you type the
characters in reading order and the engine does the rest.

**The short-i sign reorders.** The short-i (ि) is typed after its consonant but drawn before it.
In `कि` (ka + short-i) the sign is set to the left of the ka even though it follows it in memory,
and the font's width-matched variant is chosen from the consonant it now precedes.

**Consonants fuse into conjuncts and half-forms.** A consonant, the virama, and another consonant
join: `क्ष` (kṣa) and `त्र` (tra) become single ligature glyphs, while `न्द` sets a half-form of
the first consonant against the full second.

**A word-initial ra becomes a reph.** `र` followed by the virama at the head of a syllable is not
drawn in place but as a small hook above the syllable's base — `कर्म`, `धर्म`, `सूर्य`. A ra
*after* another consonant instead hangs below it, either fused into one glyph (`प्र`, `क्र`) or set
as a stroke beneath the base (`ट्र`).

**A nukta composes with its consonant.** The dot written below a consonant to mark a borrowed sound
joins it into a single letter — `क़`, `ज़`, `ड़`, as in `ज़रूरी` or `बड़ा`.

**Vowel signs and syllable marks are positioned by the font.** The below-base u signs, the
above-base e and ai signs, and the anusvara and visarga are placed on their base by the font's
anchors, the same mechanism that positions Hebrew niqqud and Arabic harakat: `कु`, `के`, `कं`.

## Setting a paragraph

Devanagari sets in ordinary running text: the line breaker finds its breaks at the interword
spaces exactly as for Latin, and only the shaping within each word is special. A whole Hindi
paragraph needs nothing beyond selecting the font.

```texish
{\font devanagari 12 regular
यह देवनागरी लिपि में लिखा गया है। हर अक्षर एक व्यंजन और उसके साथ का स्वर मिलकर बनता है,
और दो व्यंजन हलन्त से जुड़कर संयुक्त अक्षर बनाते हैं।
}
```

Because Devanagari sets left to right like a Latin text face, it uses the same TeX-style
punctuation shorthands: `` `` `` and `''` become curly quotes, `--`/`---` en/em dashes, and
`...` an ellipsis, all of which the bundled face carries. (The right-to-left Hebrew and Arabic
faces do not — their quote conventions differ, so those documents type the marks literally.)

The bundled `scripts/devanagari-demo.script` sets a full page covering each of these features.
