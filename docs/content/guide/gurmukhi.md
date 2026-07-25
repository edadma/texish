---
title: "Gurmukhi (Punjabi)"
weight: 14
---

texish sets the Gurmukhi script — used for Punjabi as written in India — with the syllable shaping
the script needs. Like Devanagari and Bengali it is written left to right, so it needs no
bidirectional reordering; what it needs is work *inside* each syllable. A consonant carries an
inherent vowel; a vowel sign attaches above, below, before or after it; and a consonant joined to
the next by the virama takes a subjoined form beneath the base. The engine segments a run into
orthographic syllables and shapes each one — building the subjoined forms, reordering the sign that
is written and drawn in different places, and positioning the marks — all automatically from text
typed in the usual Unicode order.

## The Gurmukhi font

A Gurmukhi face, Noto Serif Gurmukhi, is bundled in a regular and a bold cut. Select it with
`\font gurmukhi` (or the alias `\font punjabi`):

```texish
{\font gurmukhi 14 regular
ਪੰਜਾਬੀ ਇੱਕ ਅਮੀਰ ਭਾਸ਼ਾ ਹੈ।
}
```

```texish
{\font gurmukhi 18 bold ਸਤਿ ਸ੍ਰੀ ਅਕਾਲ!}
```

(As with the bundled Hebrew, Arabic, CJK, Devanagari and Bengali faces, the in-browser Scala.js
build does not ship the Gurmukhi font, to keep the download small.)

## What the engine shapes

Everything below happens automatically for any run set in a Gurmukhi font — you type the characters
in reading order and the engine does the rest.

**The pre-base sihari reorders.** The i-sign, the sihari (ਿ), is typed after its consonant but drawn
before it. In `ਕਿ` (ka + sihari) the sign is set to the left of the ka even though it follows it in
memory — as in `ਦਿਨ` (day) and `ਪਿਤਾ` (father).

**Consonants take subjoined forms.** A consonant joined to the next by the virama does not sit in
line but hangs beneath the base as the *pairin* — the subjoined ra, ha and va of `ਪ੍ਰੇਮ` (love),
`ਸ੍ਰੀ` (Sri) and `ਚਿੰਨ੍ਹ` (sign). Gurmukhi has **no reph**: unlike Devanagari and Bengali, a
joined ra never rises as a mark above the syllable — it always subjoins below.

**The addak doubles the following consonant.** The addak (ੱ) is set as a mark over its base and
geminates the consonant that follows — `ਇੱਕ` (one), `ਸੱਚ` (true), `ਪੱਕਾ` (firm).

**The nasal signs are positioned by the font.** The tippi (ੰ) and bindi (ਂ) mark a nasal and are
placed on their base by the font's anchors — the same mechanism that positions Hebrew niqqud and
Arabic harakat: `ਪੰਜਾਬ` (Punjab), `ਰੰਗ` (colour), `ਸਿੰਘ` (Singh).

**The below- and above-base vowel signs stack onto their base.** The aunkar and dulankar (u and uu)
hang below, the lavan and dulavan (e and ai) sit above, again through the font's GPOS anchors:
`ਕੁ`, `ਕੂ`, `ਕੇ`, `ਕੈ`.

## Setting a paragraph

Gurmukhi sets in ordinary running text: the line breaker finds its breaks at the interword spaces
exactly as for Latin, and only the shaping within each word is special. A whole Gurmukhi paragraph
needs nothing beyond selecting the font.

```texish
{\font gurmukhi 12 regular
ਇਹ ਗੁਰਮੁਖੀ ਲਿਪੀ ਵਿੱਚ ਲਿਖਿਆ ਹੈ। ਹਰ ਅੱਖਰ ਇੱਕ ਵਿਅੰਜਨ ਅਤੇ ਉਸ ਦੇ ਨਾਲ ਦਾ ਸਵਰ ਮਿਲ ਕੇ ਬਣਦਾ ਹੈ,
ਅਤੇ ਦੋ ਵਿਅੰਜਨ ਵਿਰਾਮ ਨਾਲ ਜੁੜ ਕੇ ਸੰਯੁਕਤ ਰੂਪ ਬਣਾਉਂਦੇ ਹਨ।
}
```

Because Gurmukhi sets left to right like a Latin text face, it uses the same TeX-style punctuation
shorthands: `` `` `` and `''` become curly quotes, `--`/`---` en/em dashes, and `...` an ellipsis.
(The right-to-left Hebrew and Arabic faces do not — their quote conventions differ, so those
documents type the marks literally.)

The bundled `scripts/gurmukhi-demo.script` sets a full page covering each of these features.
