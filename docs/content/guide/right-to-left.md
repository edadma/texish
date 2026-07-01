---
title: "Right-to-left text"
weight: 11
---

texish sets right-to-left scripts — Hebrew and Arabic — alongside left-to-right text in the same
paragraph. It lays out and breaks every paragraph in logical (reading) order, exactly as TeX
does, then reorders each finished line into visual order with the Unicode Bidirectional
Algorithm (UAX&nbsp;#9) before drawing it. Numbers and embedded Latin keep their own
direction, brackets are mirrored, pointed Hebrew has its niqqud positioned by the font's
anchors, and Arabic is shaped cursively — each letter connecting to its neighbours — with its
vowel marks and ligatures.

## Base direction

`\rtl` switches the paragraph base direction to right-to-left; `\ltr` switches it back. Like
`\leftskip`, the setting persists until you change it, so wrap a passage in a group to scope
it. Under a right-to-left base the paragraph aligns to the right margin and the last line rags
to the left.

```texish
\rtl
{\font hebrew 14 regular
בראשית היה הדבר והדבר היה את האלהים ואלהים היה הדבר׃
}
\ltr
```

The line breaker is untouched — it still finds its breaks at the interword glue, in logical
order. Only the set lines are turned into visual order, so justification, hyphenation of any
embedded Latin, and page breaking all work as usual.

Margin indentation follows the reading side. Under a right-to-left base `\leftskip` sets the
leading (right) margin and `\rightskip` the trailing (left) one, so an indented block, and a
list built with `\begin{itemize}` or `\begin{enumerate}`, indents from the right and hangs its
markers on the right.

## The Hebrew font

A Hebrew face, Noto Serif Hebrew, is bundled in a regular and a bold cut. Select it with
`\font hebrew`:

```texish
{\font hebrew 18 bold בראשית היה הדבר׃}
```

(As with the bundled CJK faces, the in-browser Scala.js build does not ship the Hebrew font, to
keep the download small.)

## Bidirectional text

A Latin word or a run of digits inside a right-to-left line keeps its own left-to-right order:
the algorithm treats each as an embedded island and reverses only the Hebrew around it. Nothing
special is needed — write the text in logical (reading) order and the engine resolves the rest.

```texish
\rtl
{\font hebrew 14 regular
הבשורה על־פי יוחנן פרק 1 בתרגום Delitzsch אל העברית׃
}
\ltr
```

A bracket or parenthesis that lands in a right-to-left context is drawn mirrored (rule L4), so
parentheses around Hebrew enclose it the way a reader expects: `(אב)` written in logical order
displays with the open and close brackets on the correct sides.

The reverse case needs no base change. A Hebrew word inside an ordinary left-to-right paragraph
is reversed in place while the surrounding text is left exactly as it is:

```texish
The Hebrew word {\font hebrew 12 regular דבר} means ``word''.
```

## Pointed text (niqqud)

Hebrew is normally written without vowels, but scripture and teaching texts add niqqud — the
points set above, below and inside the letters. These are combining marks with no width of
their own; texish reads the font's GPOS anchor data to place each one on its consonant rather
than letting it fall at the pen. Write the consonants and points in the usual logical order and
they are positioned automatically:

```texish
\rtl
{\font hebrew 16 regular
בְּרֵאשִׁית בָּרָא אֱלֹהִים אֵת הַשָּׁמַיִם וְאֵת הָאָרֶץ׃
}
\ltr
```

Unpointed Hebrew needs none of this and takes the plain text path, so it costs nothing.

## Arabic

Arabic is cursive: within a word the letters connect, and each takes one of four shapes —
initial, medial, final or isolated — according to its neighbours. texish resolves each letter's
form in reading order (Unicode joining) and asks the font's shaping tables for the matching
glyph, so a run reads as one joined stroke rather than a row of separate signs. An Arabic face,
Noto Serif–style Noto Naskh Arabic, is bundled in a regular and a bold cut; select it with
`\font arabic`. Base direction, bidirectional islands and bracket mirroring all work exactly as
for Hebrew.

```texish
\rtl
{\font arabic 14 regular
اللغة العربية تكتب من اليمين إلى اليسار، وتتصل حروفها بعضها ببعض داخل الكلمة.
}
\ltr
```

Two ligatures form automatically. Where a lam is followed by an alef the pair is drawn as its
required lam-alef ligature rather than two separate letters. And when the text is pointed, the
word for God — `الله` — is drawn as the single tightly-bound calligraphic ligature that
traditional typography uses; unpointed, it stays as the ordinary connected letters.

Arabic vowels (harakat) are combining marks placed by the same anchor mechanism as the Hebrew
points, and they stack: a vowel written over a shadda sits above it, and a vowel clears a
letter's own dots. Write the letters and marks in logical order and they are positioned
automatically:

```texish
\rtl
{\font arabic 16 regular
كَتَبَ الوَلَدُ. مُحَمَّد. بِسْمِ الرَّحِيم.
}
\ltr
```

Persian, Urdu and the other languages written in the Arabic script are shaped by the same
engine; a font that covers their letters will set them.

## Scope

Hebrew and Arabic — unpointed and pointed, on their own and mixed with left-to-right text — are
supported on every backend. The bundled faces do not ship in the in-browser Scala.js build, to
keep the download small.
