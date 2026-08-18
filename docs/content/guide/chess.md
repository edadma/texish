---
title: "Chess"
weight: 12
---

The `chess` package sets chess diagrams and game scores. Load it with:

```texish
\use{chess}
```

What makes it more than a board-drawing macro is that it **plays** the moves. A document
writes a line of algebraic notation the way a book prints it, and asks for a diagram; the
diagram is whatever position those moves reached, so it cannot disagree with them.

```texish
\newgame
\mainline{1.e4 e5 2.Nf3 Nc6 3.Bb5 a6 4.Ba4 Nf6 5.O-O Be7}
\centerline{\showboard}
```

## Setting a position

| Command | Effect |
|---------|--------|
| `\newgame` | the initial array, White to move |
| `\fenboard{fen}` | a position from a FEN string |
| `\fendiagram{fen}` | `\fenboard` and `\showboard` together |
| `\showboard` | draw the current position |
| `\chessfen` | write the current position back as FEN |

`\fenboard` reads the placement field and, if it is there, the side to move:

```texish
\fenboard{r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b}
```

The fields FEN carries after that — castling rights, the en-passant square, the move
clocks — are read and dropped. Nothing needs them: castling is named explicitly in the
notation, and an en-passant capture identifies itself.

`\chessfen` is the inverse. It prints where it stands, which is a quick way to check what
a line of moves reached.

## Playing moves

| Command | Effect |
|---------|--------|
| `\mainline{moves}` | print the line **and** play it |
| `\hidemoves{moves}` | play the line, print nothing |
| `\showmoves{moves}` | print the line, leave the position alone |

All three take the moves as they are written — `1.e4 e5 2.Nf3` or `1. e4 e5 2. Nf3`, with
the numbers or without them. The numbers in the input are not trusted for the numbers that
get printed: `\mainline` counts those from the position, so a line beginning at Black's
twelfth move prints `12...` whatever the source says.

`\hidemoves` is how a document reaches a position it wants to talk about without printing
the moves that got there. `\showmoves` is the opposite — a sideline set in the text beside
a main position it must not disturb:

```texish
\newgame
\hidemoves{1.d4 d5 2.c4 c6 3.Nf3 Nf6 4.Nc3 dxc4 5.a4 Bf5 6.e3 e6 7.Bxc4 Bb4 8.O-O Nbd7}
\showmoves{9.Qe2 Bg6 10.e4 O-O}
\centerline{\showboard}
```

The diagram there is the position after Black's eighth move. The two moves `\showmoves`
printed were set and then forgotten.

A word that names a result — `1-0`, `0-1`, `1/2-1/2`, `*` — is set as written and ends the
line.

## What the notation leaves out

Algebraic notation names the square a piece arrives at and leaves the square it came from
to be worked out from the position. `Nf3` is whichever knight can reach f3. The package
works it out the way a reader does — by looking outward from the destination along the
moving piece's own geometry — and then narrows what it finds:

1. **The disambiguator, when the notation gives one.** `Rad1` is the rook on the a-file;
   `R1d2` is the rook on the first rank.
2. **Legality, when it does not.** Notation omits the disambiguator when only one of the
   two candidates may legally move — which means the other is pinned. The package makes
   each candidate's move on a copy of the position and discards the one that would leave
   its own king attacked.

Castling, promotion and en passant all work. En passant needs no remembered state: a pawn
arriving on an empty square from another file can only be capturing in passing, so the
pawn it takes is the one beside it.

Check and mate marks (`+`, `#`) and annotation marks (`!`, `?`) are read and ignored —
they say nothing about the move that the position does not already say.

## When a move cannot be read

A move the position cannot account for is reported on the diagnostic channel and skipped:

```
chess: cannot read the move 'Ne4' in the position rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w
```

The rest of the line still plays, and the side to move still passes over — so a single
mistyped move does not put every move after it on the wrong side. The diagram that follows
will be visibly wrong rather than quietly so, which is the point.

## Figurines

A printed move sets its piece letter as the piece, in the convention that gives White the
outline figurines and Black the solid ones — so a move quoted on its own says whose it is.
Set `chessfigurines` to `0` for plain letters.

The glyphs come from the Miscellaneous Symbols block (U+2654–265F) of the bundled `chess`
face, which is Noto Sans Symbols 2. No text face covers those codepoints, and none is
likely to, so the face is part of the font tree rather than the compiled-in core: a
document using this package needs an installed texish, not a bare binary.

## The diagram

`\showboard` draws one `\picture` box, which flows in the text like any other box — in a
paragraph, in a `\centerline`, or in a cell of a larger layout. Everything about it is a
variable, set with `\set` after `\use{chess}`:

| Variable | Default | Effect |
|----------|---------|--------|
| `chesssquare` | `17` | the side of one square, in points |
| `chesscoords` | `1` | label the files and ranks around the board |
| `chessflip` | `0` | draw from Black's side, h1 at the top left |
| `chesshighlight` | *(empty)* | a list of squares to tint, e.g. `{e4 d5}` |
| `chesslight` | `#f0d9b5` | light-square fill |
| `chessdark` | `#b58863` | dark-square fill |
| `chessborder` | `#5d4037` | the rule around the board, and the labels |
| `chesshighlightink` | `#7fa650` | fill for the highlighted squares |
| `chesspieceink` | `black` | ink for the pieces |
| `chesspiecescale` | `1.1` | piece size as a multiple of the square |
| `chessfigurines` | `1` | set a printed move's piece letter as its glyph |
| `chessmovesize` | `9.5` | size of a figurine in a printed move |

A board small enough to sit in a line of text is `\set chesssquare {9}` with
`\set chesscoords {0}`; a green board is `\set chesslight {#eeeed2}` and
`\set chessdark {#769656}`.

## How it works

There is no chess-specific engine primitive. The position is a `\map` from a square's name
to a piece letter in FEN's convention — uppercase for White, lowercase for Black, `.` for
empty — which makes the side owning a piece a matter of case. Reading a move is a walk
over the characters; finding the piece that moved is `\for` over the knight's eight leaps
or a ray cast along each of a rook's four directions; making a move is `\mapset`. Saving a
position in order to test a move's legality is an assignment, because a map is a value.

The whole package is `packages/chess.texish`, and `scripts/chess-demo.script` exercises
all of it.
