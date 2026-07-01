---
title: "Bengali–Assamese"
weight: 13
---

texish sets the Bengali–Assamese script — used for Bengali, Assamese and several other languages —
with the syllable shaping the script needs. Like Devanagari it is written left to right, so it needs
no bidirectional reordering; what it needs is work *inside* each syllable. A consonant carries an
inherent vowel; a vowel sign attaches above, below, before or after it; and two consonants joined by
the hasant (the virama) fuse into a conjunct. The engine segments a run into orthographic syllables
and shapes each one — building the conjuncts, reordering and splitting the signs that are written
and drawn in different places, and positioning the marks — all automatically from text typed in the
usual Unicode order.

## The Bengali font

A Bengali face, Noto Serif Bengali, is bundled in a regular and a bold cut. Select it with
`\font bengali` (or the alias `\font assamese`):

```texish
{\font bengali 14 regular
বাংলা একটি সমৃদ্ধ ভাষা।
}
```

```texish
{\font bengali 18 bold নমস্কার!}
```

(As with the bundled Hebrew, Arabic, CJK and Devanagari faces, the in-browser Scala.js build does
not ship the Bengali font, to keep the download small.)

## What the engine shapes

Everything below happens automatically for any run set in a Bengali font — you type the characters
in reading order and the engine does the rest.

**The pre-base vowel signs reorder.** The i, e and ai signs (ি, ে, ৈ) are typed after their
consonant but drawn before it. In `কে` (ka + e-sign) the sign is set to the left of the ka even
though it follows it in memory, and the font's word-initial variant is chosen when the sign opens a
word.

**The two-part o and au signs split.** The o sign (ো) and au sign (ৌ) are single characters that
render in two pieces — an e-sign before the base and an aa-sign (for o) or a length mark (for au)
after it. The engine splits them, following Unicode's canonical decomposition, before shaping:
`কো`, `নৌকা`.

**Consonants fuse into conjuncts.** A consonant, the hasant, and another consonant join into a
conjunct ligature: `ক্ত`, `ছাত্র` (tra), `জ্ঞান` (jñ), `কষ্ট` (ṣṭ), `বিদ্যা` (dya).

**A word-initial ra becomes a reph.** `র` followed by the hasant at the head of a syllable is not
drawn in place but as a small stroke above the syllable's base — `কর্ম`, `ধর্ম`. A `য` after the
hasant becomes the post-base *ya-phalaa* (`মধ্য`), and a ra *after* another consonant hangs below
it, fused into the glyph — `প্রেম`, `গ্রাম`.

**A nukta composes with its consonant.** The dot written below a consonant joins it into a single
letter — the rra, rha and yya of `বড়`, `আষাঢ়`, `আয়না`.

**Vowel signs and syllable marks are positioned by the font.** The below-base u signs, and the
anusvara and visarga, are placed on their base by the font's anchors — the same mechanism that
positions Hebrew niqqud and Arabic harakat: `কু`, `কং`, `কঃ`.

## Setting a paragraph

Bengali sets in ordinary running text: the line breaker finds its breaks at the interword spaces
exactly as for Latin, and only the shaping within each word is special. A whole Bengali paragraph
needs nothing beyond selecting the font.

```texish
{\font bengali 12 regular
এটি বাংলা লিপিতে লেখা হয়েছে। প্রতিটি অক্ষর একটি ব্যঞ্জন এবং তার সঙ্গের স্বর মিলে তৈরি হয়,
আর দুটি ব্যঞ্জন হসন্ত দিয়ে যুক্ত হয়ে যুক্তাক্ষর গঠন করে।
}
```

Because Bengali sets left to right like a Latin text face, it uses the same TeX-style punctuation
shorthands: `` `` `` and `''` become curly quotes, `--`/`---` en/em dashes, and `...` an ellipsis,
all of which the bundled face carries. (The right-to-left Hebrew and Arabic faces do not — their
quote conventions differ, so those documents type the marks literally.)

The bundled `scripts/bengali-demo.script` sets a full page covering each of these features.
