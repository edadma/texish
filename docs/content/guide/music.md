---
title: "Music"
weight: 13.25
---

The `music` package sets staff notation: notes, rests, beams, slurs and ties, articulations,
dynamics, clefs, key and time signatures, lyrics under the staff and chord names above it. Load it
with:

```texish
\use{music}
```

A line of music is written as one `\score`, whose body is a space-separated list of notes:

```texish
\centerline{\score{c d e f g a b c'}}
\centerline{\score{e4 d4 c4 d4 | e4 e4 e2}}
```

The heads, clefs, flags, rests, accidentals and time-signature figures are real engraved glyphs
from a SMuFL (Standard Music Font Layout) music font; the stems, beams, staff lines, ledger lines
and barlines are ruled over the [`\picture`](/guide/graphics/) layer. SMuFL fixes where every
notation glyph lives in a font and that one staff space is a quarter of the em, so the package works
with any SMuFL face. Bravura is the default; `\set musicfont {petaluma}` chooses a handwritten,
lead-sheet alternative.

## Notes

A note is, in order:

| Part | Meaning |
|------|---------|
| `+` `-` `n` | an optional accidental — sharp, flat, natural — drawn just left of the head |
| `a` … `g` | the pitch class; lowercase `c` is middle C |
| `'` `,` | raise / lower by an octave, repeatable (`c'` is the C above middle C, `c,` the C below) |
| `1` `2` `4` `8` `16` `32` | the duration — whole, half, quarter (the default), eighth, sixteenth, thirty-second |
| `.` | an augmentation dot, lengthening the note by half |
| `>` `!` `=` `*` `;` | articulations — see below |
| `-` | a tie to the next note |

So `g,2` is the G below middle C held for a half note, `+c'8>` a sharpened eighth with an accent,
and `c2.` a dotted half.

```texish
\score{c1 c2 c4 c8 c16 c32}
\score{c2. d4 | e4. e8 f4 g4}
\score{c, e, g, c g c' e' g'}
```

Notes far above or below the staff hang on short ledger lines, drawn only as far as the note
reaches. A note below the middle line stems up and one at or above it stems down (beamed notes
stem up); a flagged note whose stem points down has the stem lengthened so the flag clears the head.

An accidental changes how the note reads, not where it sits, so `+f` and `f` share a line. A note
carrying an accidental is moved along from the one before it so the sign has room:

```texish
\score{c +c d -e e f +f g | +f nf f2}
```

## Rests and barlines

`r` is a rest, sized by a duration digit the same way a note is (`r1`, `r2`, `r4`, `r8`). Rests take
no syllable of a lyric.

```texish
\score{c r1 d r2 e r4 f r8 g}
```

`|` is a barline. A score can also write a double barline `||`, a final barline `|.`, and the start
and end of a repeated section, `|:` and `:|`:

```texish
\score{|: c d e f | g a b c' :| d' c' b a || g1 |.}
```

## Beams

`[` and `]` bracket a run of notes joined by one beam instead of separate flags. The beam tilts to
follow the melody, clamped so a wide leap does not run it off the staff. A run of sixteenths carries
a second beam and thirty-seconds a third; the bracket beams whatever durations it contains.

```texish
\score{[ c d e f g ] [ g f e d c ] [ c e g e c ]}
\score{[ c16 d16 e16 f16 ] [ g32 a32 b32 c'32 ] [ c8 d16 e16 f8 ]}
```

## Slurs and ties

`(` and `)` bracket a run drawn under one slur. A trailing `-` ties a note to the next, which should
be the same pitch. A slur goes below the notes when all their stems point up, and above them when
any stem points down.

```texish
\score{( c d e f g ) a | c'-2 c'2}
```

## Articulations

A mark after a note says how to attack it, and is drawn on the side away from the stem. Several may
follow one note; they stack outward.

| Mark | Articulation |
|------|--------------|
| `>` | accent |
| `!` | staccato |
| `=` | tenuto |
| `*` | marcato |
| `;` | fermata |

```texish
\score{c d e f | c> d> e> f> | c! d! e! f! | c= d= e= f= | g2*;}
```

## Dynamics and hairpins

A token of its own beginning with `!` is a dynamic, built from the letters after it: `!p`, `!mf`,
`!ff`, `!sfz`. It sits just below the notes it belongs to. A standalone `<` opens a crescendo
hairpin and `>` a diminuendo, each closed by `=`, spanning the notes between:

```texish
\score{!p c d < e f g a = !f b c' > b a g f = !p e2}
```

(A `>` written straight after a note is an accent; a `>` standing alone between notes opens a
diminuendo.)

## Clefs, key and time signatures

These are settings, made with `\set` before the `\score` they apply to, and they hold until changed.

`musicclef` is `treble` (the default), `bass`, `alto` or `tenor`. It chooses the clef and how pitches
sit on the staff: in the bass clef middle C is one ledger line above the staff; the C clef puts it on
the middle line (alto) or the fourth (tenor).

```texish
\set musicclef {bass}
\score{c, d, e, f, g, a, b, c}
\set musicclef {treble}
```

`musickey` is a signed count: a positive number draws that many sharps, a negative one that many
flats, each in its conventional order and position for the clef. `0` is no signature.

`musictimenum` and `musictimeden` draw a time signature after the clef; leave them empty for none.

```texish
\set musickey {2}
\set musictimenum {3}
\set musictimeden {4}
\score{d e +f g | a b +c' d' | b a g2}
\set musickey {0}
\set musictimenum {}
\set musictimeden {}
```

## Lyrics

`\lyrics{…}`, written just before a `\score`, sets words under that score, one syllable to a note:

```texish
\set musickey {1}
\set musictimenum {3}
\set musictimeden {4}
\lyrics{A -- maz -- ing _ grace, how sweet the sound}
\centerline{\score{d4 | g2 ( [ b8 g8 ] ) | b2 a4 | g2 e4 | d2. ||}}
```

- Syllables are separated by spaces and taken by the notes in order; **rests take none**.
- `--` between two syllables joins them into one word, drawn with a hyphen centred in the gap.
- `_` gives its note no syllable, so one syllable can be held across several notes.

Each syllable is centred under its note, and the notes are spread apart wherever two neighbouring
syllables need the room, so a long word moves the music along rather than running into the next.
A `\lyrics` applies to the next `\score` only.

## Chord names

A chord name in double quotes, written as its own token, is set above the staff over the note or
rest that follows it, as on a lead sheet or in ABC notation:

```texish
\lyrics{A -- maz -- ing _ grace, how sweet the sound}
\centerline{\score{d4 | "G" g2 ( [ b8 g8 ] ) | "G7" b2 a4 | "C" g2 e4 | "G" d2. ||}}
```

The name starts at the left edge of its note's head and is set the way a lead sheet sets it:

- the root at full size, with a `b` or `#` drawn as a real flat or sharp sign, and a minor `m`;
- an extension — `7`, `maj7`, `sus4`, `7b9` — raised and smaller;
- a slash bass — `F/C`, `Eb/Bb` — back at full size.

```texish
\score{"C" c'4 "Am7" a4 "Dm7" d'4 "G7" g4 | "Cmaj7" c'4 "F/C" f4 "Bbm7" -b4 "Eb" -e'4 | "Gsus4" g4 "G7b9" g4 "F#m" +f4 "C" c'4 |.}
```

A name wider than the room before the next chord moves that chord's note along.

## Scores longer than a line

A score too long for one line breaks at its barlines into **systems**, stacked down the page like
the lines of a paragraph, so a whole song is written as one `\score`. Each system but the last is
spread to the full width. Every system repeats the clef and key signature; only the first carries the
time signature. Lyrics and chord names run on from line to line, and a tie, slur or hairpin that
crosses a break is finished at the end of one system and taken up at the start of the next.

```texish
\set musickey {1}
\set musictimenum {3}
\set musictimeden {4}
\noindent
\lyrics{A -- maz -- ing _ grace, how sweet the sound that saved a _ wretch like me! 'Twas grace that _ taught my heart to fear, and grace my _ fears re -- lieved.}
\score{d4 | "G" g2 ( [ b8 g8 ] ) | "G7" b2 a4 | "C" g2 e4 | "G" d2 d4 | "G" g2 ( [ b8 g8 ] ) | "Em" b2 a4 | "D" d'2. || d4 | "G" g2 ( [ b8 g8 ] ) | "G7" b2 a4 | "C" g2 e4 | "G" d2 d4 | "G" g2 ( [ b8 g8 ] ) | "Em" b2 a4 | "D" d'2. |.}
```

The width of a system is the measure of the text unless `musicwidth` says otherwise; `musicsystemgap`
adds space between systems.

## Configuration

Every setting is made with `\set` after `\use{music}` and before the `\score` it should affect.
Sizes are in points.

| Setting | Default | Meaning |
|---------|---------|---------|
| `musicgap` | `8` | the staff space — the distance between staff lines, which everything scales from |
| `musicnote` | `22` | the least horizontal advance from one note to the next |
| `musicleft` | `44` | where the first note stands, leaving room for the clef |
| `musicpad` | `30` | vertical room above and below the staff for ledger lines, stems and flags |
| `musiccolor` | `black` | the ink |
| `musicfont` | `bravura` | the SMuFL music font (`bravura`, `petaluma`, …) |
| `musicclef` | `treble` | `treble`, `bass`, `alto` or `tenor` |
| `musickey` | `0` | sharps (positive) or flats (negative) in the key signature |
| `musictimenum` `musictimeden` | empty | the time signature; empty for none |
| `musiclyricfont` | `lmroman` | the face lyrics are set in |
| `musiclyricsize` | `11` | the lyric font size |
| `musiclyricgap` | `6` | the least white space between two neighbouring syllables |
| `musiclyricdrop` | `26` | how far the lyric baseline sits below the bottom staff line |
| `musicchordfont` | `lmroman` | the face chord names are set in |
| `musicchordsize` | `12` | the chord-name font size |
| `musicchordweight` | `bold` | the chord-name weight (`regular`, `bold`, …) |
| `musicchordgap` | `8` | the least white space between two neighbouring chord names |
| `musicchordrise` | `22` | how far the chord-name baseline sits above the top staff line |
| `musicchordsupscale` | `0.7` | an extension's size (the 7 of G7) as a fraction of the chord size |
| `musicchordsupraise` | `0.4` | how far an extension is raised, as a fraction of the chord size |
| `musicwidth` | empty | the width of a system; empty for the measure of the text (`\hsize`) |
| `musicsystemgap` | `6` | extra space between the systems of a score that runs over several lines |

The chord and lyric faces may be any loaded text face. One that has no bold cut still honours
`musicchordweight {bold}`: texish draws its glyphs emboldened (see
[Fonts and font shape](/reference/commands/#fonts-and-font-shape)).
