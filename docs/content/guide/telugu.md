---
title: "Telugu"
weight: 15
---

texish sets the Telugu script with the syllable shaping it needs. Like Devanagari, Bengali and
Gurmukhi it is written left to right, so it needs no bidirectional reordering; what it needs is work
*inside* each syllable, and that work falls differently in Telugu than in the others. The engine
segments a run into orthographic syllables and shapes each one — fusing consonants with their vowel
signs, building the subscripts, moving the sign that belongs to the base, and positioning the marks —
all automatically from text typed in the usual Unicode order.

## The Telugu font

A Telugu face, Noto Serif Telugu, is bundled in a regular and a bold cut. Select it with
`\font telugu`:

```texish
{\font telugu 14 regular
తెలుగు ఒక సుందరమైన భాష.
}
```

```texish
{\font telugu 18 bold నమస్కారం!}
```

(As with the bundled Hebrew, Arabic, CJK and other Indic faces, the in-browser Scala.js build does
not ship the Telugu font, to keep the download small.)

## What the engine shapes

Everything below happens automatically for any run set in a Telugu font — you type the characters in
reading order and the engine does the rest.

**A consonant and its vowel sign fuse into one glyph.** Where Devanagari sets a letter and a sign
beside it, Telugu usually draws the pair as a single form: `కి` is one glyph, not two, and so is each
of `కా కీ కు కూ కె కే కొ కో`.

**Joined consonants hang beneath the base.** A consonant joined to the next by the virama does not
stand in line but is drawn as a subscript under the base — the second `క` of `క్క`, the `త` of
`స్త`. This inverts which consonant is the base: Devanagari's is the *last* consonant of a conjunct,
Telugu's the *first*, with everything after it underneath. Words like `పుస్తకం` (book) and
`స్నేహితుడు` (friend) stack this way.

**The vowel sign is set beside the base, not after the subscripts.** A sign is typed after the whole
conjunct but belongs to the base alone, so the engine moves it back across the subscripts to reach
it — in `ప్రేమ` (love) the vowel joins the *pa* and the subjoined *ra* hangs below the pair, and in
`స్త్రే` it moves back across two subscripts. Without that move the base and its sign never meet and
the font cannot fuse them. The vocalic r signs (`ృ`, `ౄ`) are the exception: they are drawn below the
syllable themselves and keep their place after the subscripts, as in `సంస్కృతం` (Sanskrit).

**There is no reph.** Unlike Devanagari and Bengali, a word-initial `ra` with a virama does not rise
as a mark above the syllable. In `ర్క` the ra stays an ordinary base and the following consonant
subjoins beneath it.

**Two-part signs are split.** The ai sign `ై` is one character in memory but is drawn as an e sign
plus a length mark; the engine splits it so both parts land on the base — `హై`, as in `హైదరాబాద్`
(Hyderabad).

**The anusvara and visarga follow the base.** They are set as spacing marks after the syllable:
`కం`, `కః`.

## Setting a paragraph

Telugu sets in ordinary running text: the line breaker finds its breaks at the interword spaces
exactly as for Latin, and only the shaping within each word is special. A whole Telugu paragraph
needs nothing beyond selecting the font.

```texish
{\font telugu 12 regular
ఇది తెలుగు లిపిలో వ్రాయబడింది. ప్రతి అక్షరం ఒక హల్లు మరియు దానితో కలిసిన అచ్చుతో
ఏర్పడుతుంది, మరియు రెండు హల్లులు విరామంతో కలిసి సంయుక్తాక్షరం అవుతాయి.
}
```

Because Telugu sets left to right like a Latin text face, it uses the same TeX-style punctuation
shorthands: `` `` `` and `''` become curly quotes, `--`/`---` en/em dashes, and `...` an ellipsis.
(The right-to-left Hebrew and Arabic faces do not — their quote conventions differ, so those
documents type the marks literally.)

The bundled `scripts/telugu-demo.script` sets a full page covering each of these features.
