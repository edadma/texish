---
title: "Barcodes"
weight: 13.5
---

Two two-dimensional symbologies, each in its own package: **QR Code** and **Data Matrix**. Both draw
the symbol as one vector rectangle per dark module over the
[graphics](/guide/graphics/) layer, so it stays crisp at every size and in every backend — there is
no raster image anywhere in either.

```texish
\use{qrcode}
\use{datamatrix}

\qrcode{https://example.com}
\datamatrix{PARCEL-4471}
```

**The data is read verbatim.** A URL's `//` is not a comment, and `%`, `~` and `#` reach the encoder
as themselves. That is also the one restriction: a verbatim argument has to be written at the call,
so the payload cannot arrive through another macro.

## Which one

A QR code is what a phone camera expects and what a member of the public will recognise, and it
carries a chosen level of error correction — up to about a third of the symbol can be damaged and
it still reads. A Data Matrix packs the same payload into rather less area and needs a smaller quiet
zone, which is why it is the symbology of shipping labels and of parts marked directly. Choose the
QR code for something a person will scan, the Data Matrix for something a machine will.

## Options

Both take one optional `[key:value …]` bracket, and every option has a document variable behind it
that sets the default for every later call — so a document that wants all its symbols in one size
sets it once.

```texish
\qrcode[ecc:h cell:4pt dark:navy]{HELLO}

\set qrcell {4pt}
\qrcode{one}  \qrcode{another}
```

| Option | QR variable | Data Matrix variable | Effect |
|---|---|---|---|
| `cell` | `qrcell` | `dmcell` | the side of one module, in any length `\calc` understands (default `3pt`) |
| `quiet` | `qrquiet` | `dmquiet` | the light border, in modules — 4 for QR, 1 for Data Matrix, which is what each standard requires |
| `dark` | `qrdark` | `dmdark` | the module colour; the pen colour in force by default, so a symbol follows the document into a dark scheme |
| `light` | `qrlight` | `dmlight` | the background; `light:none` leaves the page showing through |
| `ecc` | `qrecc` | — | `l`, `m`, `q` or `h` (default `q`). Data Matrix has none: ECC200 fixes the protection by symbol size |

The error-correction level is **raised automatically** to the strongest one the chosen symbol size
still holds, so a short payload asking for `ecc:l` generally gets `ecc:h` at no cost in size.

## How much fits

Both grow through the sizes their standards define, and pick the smallest that holds the payload.
Digits are cheapest in either — a QR code packs three digits into ten bits, a Data Matrix two digits
into one codeword — and upper-case letters and a few punctuation marks are cheaper than lower case
in a QR code, because they have a mode of their own.

A payload too long to encode is **reported on the diagnostic channel and nothing is drawn**, rather
than a truncated symbol being produced. The Data Matrix package builds the square symbols from
10×10 up to 48×48, which is 174 codewords; the rectangular symbols are not built.

## Getting the value without drawing it

`\qrmatrix` and `\dmxmatrix` encode without drawing, leaving the symbol in `qrmatrixvalue` and
`dmxmatrixvalue` as a sequence of rows, each a string of `.` and `#`. That is what the packages' own
tests read, and it is there for a document that wants to draw the modules some other way.

## They are written in texish

Neither encoder is built into the engine. The mode selection, the Reed–Solomon check codewords over
GF(256), the placement walks and — for the QR code — the eight-way mask search scored by four
penalty rules are all `\calc`, `\for` and `\put` over sequences, in
[the document language](/guide/programming/). Both load a third package, `barcode`, which holds what
they share: the payload as bytes, and the field arithmetic.

That is worth knowing for one practical reason. **Encoding is real computation, and a large QR code
is not instant** — a version-4 symbol takes a few seconds in the command-line binary, where a small
one takes about one. A document with a page of symbols will notice; a document with one will not. A
Data Matrix is far quicker, since it has no mask search.
