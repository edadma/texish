---
title: "Gujarati"
weight: 17
---

texish sets the Gujarati script with the syllable shaping it needs. Like Devanagari, Bengali and the
other Indic scripts it is written left to right, so it needs no bidirectional reordering; what it
needs is work *inside* each syllable. The engine segments a run into orthographic syllables and
shapes each one — building the half-forms and conjuncts, reordering the pre-base vowel sign, lifting
a syllable-initial `ra` out as the reph, and positioning the marks — all automatically from text
typed in the usual Unicode order.

Of the scripts texish sets, Gujarati is Devanagari's closest relative: the same consonant-and-virama
model, the same half-forms and conjuncts, the same reph above the syllable — written without the
shirorekha, the headline that joins Devanagari's letters. If you have read the
[Devanagari](/guide/devanagari/) page, everything here will be familiar.

## The Gujarati font

A Gujarati face, Noto Serif Gujarati, is bundled in a regular and a bold cut. Select it with
`\font gujarati`:

```texish
{\font gujarati 14 regular
ગુજરાતી એક સમૃદ્ધ ભાષા છે.
}
```

```texish
{\font gujarati 18 bold નમસ્તે!}
```

(As with the bundled Hebrew, Arabic, CJK and other Indic faces, the in-browser Scala.js build does
not ship the Gujarati font, to keep the download small.)

## What the engine shapes

Everything below happens automatically for any run set in a Gujarati font — you type the characters
in reading order and the engine does the rest.

**The i-sign is drawn before its consonant.** `િ` is typed after the consonant it belongs to but
rendered to its left, ahead of the whole syllable — so `કિ` sets the sign first and the `ક` after it.
It is the only Gujarati vowel sign that reorders; every other sign stays where it was typed and is
placed above, below or after the base by the font. Words like `દિન` (day), `પિતા` (father) and
`વિદ્યા` (knowledge) all show it.

**Joined consonants take half-forms and conjuncts.** A consonant joined to the next by the virama
gives up its inherent vowel. Some pairs draw the first as a half-form beside the base — the `ન્દ` of
`ન્દિ` — and others fuse into a single ligature glyph: `ક્ષ` is one glyph built from three
characters, and so is the `સ્ત` of `પુસ્તક` (book). The base of a conjunct is its *last* consonant,
as in Devanagari.

**A syllable-initial `ra` becomes the reph.** A `ra` joined by the virama at the head of a syllable
is not drawn in place: it rises as a mark above the syllable, and — as in Devanagari, but unlike
Bengali — it closes the cluster, sitting *after* any post-base vowel sign the base carries. So `ર્મા`
sets *ma*, then the aa sign, then the reph above them. `ધર્મ` (duty), `કર્મ` (action), `સૂર્ય` (sun)
and `અર્થ` (meaning) are the everyday cases.

**There are no two-part vowel signs.** Unlike Bengali's o and au, or Telugu's ai, every Gujarati
vowel sign is a single codepoint the font maps on its own — including `ો` and `ૌ`, which are drawn as
an aa sign with a mark above it but are never split.

**The anusvara, candrabindu and visarga follow the base.** They attach to the syllable after the base
and are placed by the font: `કં`, `કઁ`, `કઃ`.

## Setting a paragraph

Gujarati sets in ordinary running text: the line breaker finds its breaks at the interword spaces
exactly as for Latin, and only the shaping within each word is special. A whole Gujarati paragraph
needs nothing beyond selecting the font.

```texish
{\font gujarati 12 regular
આ ગુજરાતી લિપિમાં લખેલું છે. દરેક અક્ષર એક વ્યંજન અને તેની સાથેના સ્વરથી બને છે,
અને બે વ્યંજન વિરામથી જોડાઈને સંયુક્ત રૂપ બનાવે છે.
}
```

Because Gujarati sets left to right like a Latin text face, it uses the same TeX-style punctuation
shorthands: `` `` `` and `''` become curly quotes, `--`/`---` en/em dashes, and `...` an ellipsis.
(The right-to-left Hebrew and Arabic faces do not — their quote conventions differ, so those
documents type the marks literally.)

The bundled `scripts/gujarati-demo.script` sets a full page covering each of these features.
