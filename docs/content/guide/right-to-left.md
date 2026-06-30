---
title: "Right-to-left text (Hebrew)"
weight: 11
---

texish sets right-to-left scripts — Hebrew today — alongside left-to-right text in the same
paragraph. It lays out and breaks every paragraph in logical (reading) order, exactly as TeX
does, then reorders each finished line into visual order with the Unicode Bidirectional
Algorithm (UAX&nbsp;#9) before drawing it. Numbers and embedded Latin keep their own
direction, brackets are mirrored, and pointed Hebrew has its niqqud positioned by the font's
anchors.

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

## Scope

Hebrew — unpointed and pointed, on its own and mixed with left-to-right text — is supported on
every backend. Arabic and the other cursive right-to-left scripts need joining and contextual
shaping that texish does not yet do; they are not supported.
