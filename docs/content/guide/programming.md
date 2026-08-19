---
title: "Programming and Data"
weight: 3
---

Every texish document is a program, whether or not it looks like one. A `\section` is a macro, a
page number is a variable, and the packages that give a document its shape — `document`, `book`,
`plot`, `chess` — are written in the same language a document is, with no privileged access to the
engine. This page is that language: how to bind names, branch, loop, and hold data.

It is worth saying what texish is *not*, because the difference explains a lot. TeX is a
two-stage expansion machine over untyped token soup, and much of what a TeX programmer knows —
`\expandafter`, `\edef`, `\noexpand`, catcode changes — exists to manage that. texish evaluates
directly, with typed values and fixed catcodes, so none of that machinery exists and none of it is
needed. A value is a number, a string, a boolean, a sequence, a map, a dimension, a glue or a macro,
and it stays what it is.

## Variables

`\set` binds a name; `\the` prints one. A name is [**letters only**](#names-are-letters-only).

```texish
\set author {Ada Lovelace}
\set copies {12}

Written by \the\author, in \the\copies copies.
```

Assignment is scoped to the enclosing group and reverts at its close, which is what makes a font
change inside `{ … }` local. `\global` escapes that, and is how a value survives a loop iteration or
a macro's own group:

```texish
{\set copies {3}}      // reverts at the closing brace
\global\set copies {3} // persists
```

A bare name is also readable inside `\calc` (below), so `\set n {5}` can then be used as
`\calc{n * 2}` as well as `\the\n`.

A value that looks like a number is stored as one — **unless storing it would change it**. Past
about sixteen digits a whole number can no longer be held exactly, and such a run is kept as text
rather than rounded: an order number, a barcode payload or an account identifier comes back out
exactly as it went in. Nothing is lost by this, since arithmetic reads a numeric string as readily
as a number.

```texish
\set order {12345678901234567890}
\the\order          // 12345678901234567890, not 1.2345678901234567E19
```

## Arithmetic

`\calc` evaluates an infix expression to a number — the whole numeric library lives inside the
expression rather than as a control sequence per operation:

```texish
\set radius {12}
\set area {\calc{pi * radius^2}}
\set inset {\calc{0.5in - 3mm}}
```

It has the usual precedence ladder, unary sign, parentheses, `%` for remainder and right-associative
`^`; the constants `pi`, `tau` and `e`; length units (`pt pc in cm mm em ex`), so a dimension
computes directly; and about forty functions — the trigonometric family with `…d` variants that take
degrees (`sind`, `cosd`, `atan2d`), `sqrt cbrt exp ln log log2 logb pow hypot`, and
`abs floor ceil round trunc sign mod min max`.

It also does bit manipulation — `and`, `or`, `xor`, `not`, `shl` and `shr` — over the integer part of
each argument. `and`, `or` and `xor` take any number of arguments and fold, so a mask of several
flags is one call; `not` complements over 64 bits and so gives a negative, which makes
`and(not(x), 255)` the way to mask a byte. This is what lets a package compute a checksum, pack a
colour, or run the Galois-field arithmetic a barcode's error correction needs, all of which are
exclusive-or underneath.

`\round{value}{places}` trims floating-point noise off a computed number (`0.30000000000000004`
becomes `0.3`); `\fixed{value}{places}` gives exactly that many decimals, zeros kept, for a column of
prices. The simple `\+ \- \* \/` also exist, but `\calc` is almost always clearer.

`\calc` reads its argument as an expression *string*, so a variable works in it spelled either way —
`\calc{n * 2}` and `\calc{\n * 2}` both resolve the variable `n`. A call works too, as long as its
arguments are braced: `\calc{\nth{\p}{1} * 2}` and `\calc{\total{\xs} / \size{\xs}}` evaluate the
call and use what it produced. So does a dotted field, which is what lets a loop compute from its
own position: `\calc{\forloop.index * 10}`. Anything that gives a non-number is an error.

## Conditionals

`\if` takes a condition and two branches. The comparisons `\=`, `\!=`, `\<`, `\>`, `\<=`, `\>=` each
yield a boolean, and any value can be tested directly — an empty string, an empty sequence, `0`,
false and an unset name are all false. **`\while` and `\filter` test by the same rule**, so a
condition written as arithmetic — which is how a package says "in range" without four nested
comparisons — means the same thing wherever it is used.

```texish
\if {\> {\calc{copies}} {10}}
  a large printing
\else
  a small one
\fi
```

## Loops

`\for` walks something known: a sequence, a string (character by character), or a map (entry by
entry). Inside the body `\forloop` carries the position — `\forloop.index` (from 1), `.indexz` (from
0), `.first`, `.last`, `.length`, `.rindex`, `.element`.

```texish
\for\name{\words{Ada Grace Katherine}}{\if {\forloop.first}\else, \fi\name}
```

`\while` is for a loop whose length is not known before it starts — consuming input until it runs
out, or iterating until a computation converges. Its condition is re-read each time round:

```texish
\global\set x {2}
\global\set d {1}
\while {\> {\d} {0.0000001}} {
  \global\set p {\x}
  \global\set x {\calc{(x + 2/x) / 2}}
  \global\set d {\calc{abs(x - p)}}
}
```

Both open a scope per iteration, so a value meant to outlive the loop is set `\global`. Often it
need not outlive it at all — see [computing a result](#computing-a-result-from-a-sequence).

## Sequences

`\seq{…}` builds one from whitespace-separated items, and a braced item keeps its spaces.
`\words{s}` splits a string on whitespace, `\range{a}{b}` counts inclusively, and `\split` cuts on
any separator.

```texish
\set names {\seq{Ada {Grace Hopper} Katherine}}
\set fields {\split{surname,given,born}{,}}
\set tens {\range{1}{10}}
```

Reading them:

| | |
|---|---|
| `\nth{seq}{n}` | the nth item, counting from **1** |
| `\head` `\tail` `\last` | first, all-but-first, final |
| `\size{seq}` | how many |
| `\slice{seq}{from}{count}` | a run of items, clamped at both ends |
| `\contains{seq}{item}` `\indexof{seq}{item}` | membership, and where — `0` if absent |

Building them — a sequence is a **value**, not a container that is mutated, so these return a new one:

| | |
|---|---|
| `\append{seq}{item}` `\prepend{seq}{item}` | one more item at either end |
| `\put{seq}{n}{value}` | the nth item replaced — the only way to write at a position |
| `\concat{a}{b}` | one sequence after another |
| `\reverse{seq}` `\sort{seq}` | turned around, put in order |
| `\chunk{seq}{n}` | grouped into sub-sequences of `n` |
| `\join{seq}{sep}` | back to a string |
| `\total` `\minimum` `\maximum` | sum, least, greatest |

`\sort` orders numerically where the items are numbers and alphabetically where they are words, so
`2` sorts before `10` rather than after it.

> **A string is the sequence of its characters.** `\nth`, `\slice`, `\reverse`, `\size`,
> `\contains`, `\indexof`, `\head`, `\tail`, `\last` and `\for` all take a string as readily as a
> sequence, and give back the kind they were given. Characters means **code points**, so an emoji or
> a math alphanumeric counts as one — `\size{a🎲b}` is 3, and walking it by index works.
>
> **A number counts as the characters it displays as**, for the same commands. A run of digits
> becomes a number the moment it is stored, so without this the same text would answer one length
> written out and another through a variable: `\size{12345}` is 5 either way, and
> `\for\d{\n}{[\d]}` over a stored `123` visits three digits.

## Computing a result from a sequence

A loop **is** worth what it wrote: `\set totals {\for\n{\xs}{[\n]}}` collects the text of every
iteration. The same goes for `\while`, and for a conditional — an `\if` in a value position is
worth the branch it chose:

```texish
\set size {\if {\> {\n} {10}}large\else small\fi}
```

That is rarely what you want from a loop, though, because it collects **text**. Three commands
collect the body's **value** instead, binding a variable of your choosing exactly as `\for` does,
and they are what a loop that computes something should use:

```texish
\set squares {\transform\n{\range{1}{5}}{\calc{\n * \n}}}        // 1 4 9 16 25
\set big     {\filter\n{\squares}{\> {\n} {5}}}                  // 9 16 25
\set sorted  {\sortby\w{\words{Zebra apple Fig}}{\downcase{\w}}}  // apple Fig Zebra
```

`\sortby` is stable, so items with equal keys keep their original order — which matters, because a
document must come out identical from one pass to the next.

The body may be several statements, in which case its value is the last one that produced a value.
That is what lets you pull fields out of a record before computing with them, given that `\calc`
cannot call a primitive:

```texish
\set points {\chunk{\seq{1 2  3 4  5 6}}{2}}
\set body {\set x {\nth{\p}{1}}\set y {\nth{\p}{2}}\calc{x * y}}
\set sxy {\total{\transform\p{\points}{\set x {\nth{\p}{1}}\set y {\nth{\p}{2}}\calc{x * y}}}}
```

That is a fold, and it is exactly what the `plot` package's least-squares fit is built from.

## Strings

Beyond `\upcase`, `\downcase` and `\trim`: `\cat{a}{b}` joins two values as text, `\split` and
`\join` convert to and from a sequence, `\replace{text}{from}{to}` changes every occurrence,
`\repeat{text}{n}` repeats, and `\startswith` / `\endswith` test either end. Separators and patterns
are matched **literally**, never as regular expressions, so a `.` means a full stop.

```texish
\set slug {\downcase{\replace{\title}{ }{-}}}
```

`\ord{char}` gives a character's Unicode code point and `\chr{n}` gives it back, which is what makes
a string computable rather than only comparable — a document can encode text as bytes, shift a
letter, or index a table by character. Both work in code points, so an emoji or a math alphanumeric
survives the round trip in one piece.

```texish
\set bytes {\transform\c{\payload}{\ord{\c}}}
```

## Maps

A map is an insertion-ordered store with computed keys — texish's answer to TeX's `\csname`, and what
the counter machinery is built on.

```texish
\set roman {\map{i 1 v 5 x 10}}
\mapset roman {l} {50}

\set five {\roman.v}            // dotted access, for a literal key
\set n {\mapget roman {\digit}} // \mapget, for a computed one
```

Both read a map **in expression position** — inside `\set`, `\if` or another command's argument —
and neither prints on its own: `\the\roman.v` typesets the whole map followed by a literal `.v`,
and a bare `\mapget` typesets nothing at all. Bind the value to a name and print that.

`\maphas` tests a key, `\mapdel` removes one, and `\keys` and `\values` give a map's keys or values
as sequences — so a map can be sorted, filtered or counted like anything else:

```texish
\for\k{\sort{\keys{\roman}}}{\set val {\mapget roman {\k}}\k = \the\val. }
```

A `\for` over the map itself visits `{key, value}` entries in insertion order, which is stable across
passes.

## Macros

`\def` defines one. Parameters are named, and a body is not its own group:

```texish
\def greet name {Hello, \name!}
\def emph [weight:bold] word {{\font lmroman 10 \weight \word}}
\def head * title {\if {\star}\leftline{\title}\else\leftline{\thesection\ \title}\fi}
\def literal <raw> {\code{\raw}}
```

- **named** parameters are read as braced arguments in order — `\greet{Ada}`;
- **`[name:default]`** declares an optional parameter, supplied at the call site **positionally, in
  brackets**: `\emph{word}` takes the default, `\emph[light]{word}` overrides it, and a macro with
  two of them is called `\dropcap[3][6]{L}{orem}`;
- **`*`** as the first parameter is a star flag, for the LaTeX `\section*` convention, tested inside
  the body with `\if {\star}`;
- **`<name>`** takes its argument verbatim, unexpanded — how a grammar or a code sample survives
  being read.

Note the two different bracket conventions, which are easy to confuse. A **macro's** optional
arguments are positional and bracketed, as above. An **engine primitive's** are named and bare —
`\hbox to:100`, `\geometry paper:a6`, `\picture width:3in`. Writing a primitive's option in
brackets compiles, runs, and silently takes the default.

`\let` aliases a name to whatever another means right now, `\gdef` defines globally, and
`\newenvironment` pairs begin and end code for `\begin`/`\end`.

**A macro used as a value is worth what its body computes**, however many statements that takes:

```texish
\def area w h {\set inner {\calc{\w - 2 * margin}}\calc{\inner * \h}}
\set a {\area{60}{40}}
```

The body runs as it would in the document, and its value is the last value anything in it produced —
so a body that has to bind something before it can compute is fine, and a branch that yields a
sequence yields the sequence rather than the text of it. Where the body writes more than that one
value, the whole of what it wrote is its value: `\def label {Item \the\n}` is worth `Item 3`, not
`3`.

## Where a value goes

A primitive that produces a value both hands it back — so it composes inside `\set`, `\if` and
`\calc` — and typesets it, so writing it on its own puts it in the document. The exception is a
**sequence or a map**, which is silent: its `[a, b]` form is for reading while debugging, not for
setting. `\the` shows one deliberately.

```texish
\cat{Chapter }{Nine}        // sets "Chapter Nine"
\sort{\seq{b a}}            // sets nothing; the sequence is still the value
\the\sorted                 // shows [a, b]
```

## Names are letters only

A control-sequence name is letters, and `\set`, `\def` and `\let` refuse anything else: `\set count2
{5}` is an error, because `\count2` would tokenize as `\count` followed by the text `2` and the
value could never be read back. A name that is *spelled out* rather than written as a control
sequence — a counter, a map key, a bare identifier inside `\calc` — may carry a digit, since it is
read back by the same spelling that made it.

## Testing for "no value"

`\= {\x} {}` is **true** when `\x` has never been set, and stays true when it is set to `{}`: an
unset name and an empty value are both absence, and no document can tell them apart. A lookup that
finds nothing — `\mapget` of a missing key, `\nth` past the end, `\minimum` of an empty sequence —
answers undefined, which is absence too, and is falsy so `\if` tests it directly.

`\ifx` asks a different question: whether two *names* mean the same thing. Two macros defined alike
are equal, a `\let` copy equals its original, and two names that mean nothing at all are equal —
which is how a package asks whether something was ever defined.

## Traps worth knowing

These two have actually cost time, and both are about typesetting rather than about values.

### A macro that runs inside a paragraph must be one line

A newline in a macro body is an interword space, the indentation of a continuation line is another
one, and two newlines are a paragraph break. A macro laid out over twelve indented lines drags a
dozen spaces into the sentence that calls it. **Split a long routine into named one-line macros
rather than indenting it**; a `//` comment eats the newline but not the next line's indentation, so
flush-left continuation lines are the other way out. This does not bind macros that only draw
(inside `\picture` stray text is discarded) or that only work between paragraphs.

### Paragraph shape is read when the paragraph breaks

`\leftskip`, `\rightskip`, `\parfillskip`, `\hangindent` and `\hangafter` are read at the paragraph
break, not where they are set — so a closing brace can revert one out from under the break. Setting
`\rightskip` at the top of a `\parbox` body does nothing at all, silently. End the paragraph with
`\par` **inside** the group, which is what `\end{flushleft}` does for the same reason.

## Where to look next

The [Command Reference](../../reference/commands/) lists every command with its arguments, and
[Parameters and Variables](../../reference/parameters/) every engine variable a document can read or
set. The packages in `packages/` are the worked examples: `chess.texish` is a move generator and
notation parser, `plot.texish` maps data onto a coordinate system, and `document.texish` is the
article format itself — none of them using anything this page has not covered.
