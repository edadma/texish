---
title: "Kannada"
weight: 19
---

texish sets the Kannada script with the syllable shaping it needs. Like the other Indic scripts it is
written left to right, so it needs no bidirectional reordering; what it needs is work *inside* each
syllable, and Kannada asks for more of it than any other script texish sets. The engine segments a
run into orthographic syllables and shapes each one — splitting the composed vowel signs, moving the
sign that belongs to the base, building the subscripts, and lifting out the reph — all automatically
from text typed in the usual Unicode order.

Kannada is [Telugu](/guide/telugu/)'s closest relative here, and the two differ in exactly two places:
Kannada forms a reph and Telugu does not, and Kannada writes far more of its vowel signs as
combinations.

## The Kannada font

A Kannada face, Noto Serif Kannada, is bundled in a regular and a bold cut. Select it with
`\font kannada`:

```texish
{\font kannada 14 regular
ಕನ್ನಡ ಒಂದು ಸುಂದರವಾದ ಭಾಷೆ.
}
```

```texish
{\font kannada 18 bold ನಮಸ್ಕಾರ!}
```

(As with the bundled Hebrew, Arabic, CJK and other Indic faces, the in-browser Scala.js build does
not ship the Kannada font, to keep the download small.)

## What the engine shapes

Everything below happens automatically for any run set in a Kannada font — you type the characters in
reading order and the engine does the rest.

**A consonant and its vowel sign fuse into one glyph.** Where Devanagari sets a letter and a sign
beside it, Kannada usually draws the pair as a single form: `ಕಿ` is one glyph, not two, and so is
`ಕೆ`.

**Joined consonants hang beneath the base.** A consonant joined to the next by the virama is drawn as
the *ottakshara*, a subscript under the base — the second `ಕ` of `ಕ್ಕ`, the `ತ` of `ಸ್ತ`. This
inverts which consonant is the base: Devanagari's is the *last* consonant of a conjunct, Kannada's the
*first*, with everything after it underneath. Words like `ಪುಸ್ತಕ` (book) and `ಚಿಹ್ನೆ` (sign) stack
this way.

**The vowel sign is set beside the base, not after the subscripts.** A sign is typed after the whole
conjunct but belongs to the base alone, so the engine moves it back across the subscripts to reach
it — `ಕ್ಕಾ` sets the *ka*, its aa sign, and only then the subjoined *ka*. Without that move the base
and its sign never meet and the font cannot fuse them. The signs drawn below the syllable rather than
on the base — the vocalic r and rr, the vocalic L and LL — are the exception and keep their place
after the subscripts, as do the two length marks.

**Composed vowel signs are split, and the split repeats.** Five of the twelve signs are written as
combinations of others: `ೀ` is the i sign plus a length mark, `ೇ` the e sign plus a length mark, `ೈ`
the e sign plus an ai length mark, `ೊ` the e sign plus a uu sign — and `ೋ` is the `ೊ` sign plus a
length mark, so it splits twice and reaches the font as three glyphs. These are Unicode's own
canonical decompositions. The au sign `ೌ` is the one composite Kannada draws with a glyph of its own,
so it does not split.

**A syllable-opening `ra` becomes the arkavattu.** A `ra` joined by the virama at the head of a
syllable is not drawn in place: it is lifted out and set after the syllable, after any vowel sign the
base carries — so `ರ್ಕಾ` sets *ka*, the aa sign, then the arkavattu. `ಧರ್ಮ` (duty), `ಕರ್ಮ` (action),
`ಸೂರ್ಯ` (sun) and `ಅರ್ಥ` (meaning) are the everyday cases. It lands where Devanagari's reph lands,
but it is not drawn the same way: Devanagari's floats above its base with no width of its own, while
Kannada's is written to the upper right and takes its own room on the line.

**The anusvara and visarga follow the base.** They are set after the syllable, outside whatever is
subjoined beneath it: `ಕಂ`, `ಕಃ`.

## Setting a paragraph

Kannada sets in ordinary running text: the line breaker finds its breaks at the interword spaces
exactly as for Latin, and only the shaping within each word is special. A whole Kannada paragraph
needs nothing beyond selecting the font.

```texish
{\font kannada 12 regular
ಇದು ಕನ್ನಡ ಲಿಪಿಯಲ್ಲಿ ಬರೆಯಲಾಗಿದೆ. ಪ್ರತಿ ಅಕ್ಷರ ಒಂದು ವ್ಯಂಜನ ಮತ್ತು ಅದರ ಸ್ವರವನ್ನು
ಒಟ್ಟಿಗೆ ಹೊಂದಿರುತ್ತದೆ.
}
```

Because Kannada sets left to right like a Latin text face, it uses the same TeX-style punctuation
shorthands: `` `` `` and `''` become curly quotes, `--`/`---` en/em dashes, and `...` an ellipsis.
(The right-to-left Hebrew and Arabic faces do not — their quote conventions differ, so those
documents type the marks literally.)

The bundled `scripts/kannada-demo.script` sets a full page covering each of these features.
