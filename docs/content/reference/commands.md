---
title: "Command Reference"
weight: 2
---

Every command, grouped by topic. Those marked *(format)* require `\use{document}` (or the named
package); the rest are engine primitives available in any document. The engine variables they read
are in [Parameters and Variables](../parameters/), and the `\picture` drawing commands are in the
[Drawing Reference](../drawing/).

## Document structure *(format)*

| Command | Effect |
|---------|--------|
| `\title{…}` `\author{…}` `\date{…}` | set the title-block fields |
| `\maketitle` | typeset the centered title block |
| `\begin{abstract}…\end{abstract}` | a centered, indented summary |
| `\section{…}` `\subsection{…}` | auto-numbered headings |
| `\begin{itemize}` `\begin{enumerate}` `\item` | bulleted and numbered lists |
| `\begin{quote}` `\begin{quotation}` | indented quotation blocks |
| `\footnote{…}` | a numbered footnote |
| `\figure[htb]{…}` `\table[htb]{…}` `\caption{…}` | floats with numbered captions |

## Cross-references and contents *(format)*

The document is typeset twice over a shared label table, so forward references resolve.

| Command | Effect |
|---------|--------|
| `\label{name}` | bind a name to the current section/figure number, kind, title and page |
| `\ref{name}` | print that number (`??` until resolved) |
| `\pageref{name}` | print the page the label landed on |
| `\eqref{name}` | print the number in parentheses, as for an equation |
| `\autoref{name}` | print the number with its kind in front (e.g. `Section 3`, `Figure 1`) |
| `\nameref{name}` | print the title of the labelled section or caption |
| `\tableofcontents` `\contents` | a contents list built from the section headings |
| `\listoffigures` `\listoftables` | lists of captioned figures / tables (`\listoffigs` `\listoftabs` add the heading) |
| `\addcontentsline{list}{level}{number}{title}` `\tocentry{level}{number}{title}` | low-level: file an entry into a named list (`toc`/`lof`/`lot`) — sectioning and `\caption` call these |
| `\contentslist{list}{format}` | low-level: replay any named list through a format macro called as `format{level}{number}{title}{page}` — the general form of `\tableofcontents`, for keeping several independent contents lists (e.g. one per language) |

## Text and markup

| Command | Effect |
|---------|--------|
| `\italic{…}` `\bold{…}` `\slanted{…}` `\smallcaps{…}` | italic, bold, slanted, small caps |
| `\serif{…}` `\sans{…}` `\mono{…}` | roman / sans-serif / typewriter member of the family |
| `\textsub{…}` `\textsup{…}` | text subscript / superscript |
| `\color[alpha]{name}` `\textcolor[alpha]{name}{…}` | pen colour (CSS name, `#RRGGBB`, `#RRGGBBAA`, or `transparent`; optional `[alpha]` 0–1) |
| `\pagecolor[alpha]{name}` | colour painted across the whole page under all content (translucent or `transparent` for overlays); set in the preamble |
| `\href{url}{text}` `\url{url}` | clickable links (real PDF annotations) |
| `\TeX` `\LaTeX` `\TeXish` | the logos *(`\use{document}`)* |
| `\dropcap[lines][gap]{X}{rest}` | drop cap spanning `lines` lines (default 2), sized by measurement; `rest` continues the word through the redefinable `\dropcaptext` (small caps by default) *(`\use{document}`)* |
| `` ``…'' `` `--` `---` `...` `~` | quotes, dashes, ellipsis, tie |
| `\$` `\&` `\#` `\%` `\{` `\}` `\\` `\~` | literal special characters (`$ & # % { } \ ~`) |

## Fonts and font shape

`\bold`/`\italic`/`\smallcaps`/`\slanted` wrap an argument; the declarations switch the font for
the rest of the enclosing group (so an environment can set its whole body in one shape) and revert
at the closing brace or `\end`. Family roles select the roman/sans/typewriter member of the current
super-family, keeping weight and slope.

Small caps come from a real cut when the family has one (Latin Modern, Alegreya SC); otherwise, if
the current face carries the OpenType `smcp` feature (EB Garamond and most modern text romans),
`\smallcaps`/`\scshape` synthesise true small capitals from that feature rather than
falling back to ordinary lowercase.

`\mono`/`\ttfamily` and `\sans`/`\sffamily` fall back to a document-level typewriter or sans
family when the current family has no such member (Garamond, Cinzel, Amiri …), so an inline
`\mono` never fails. `\ttdefault{name}`/`\sfdefault{name}` name those fallback families (Latin
Modern by default); the target may be a super-family with a typewriter/sans member or a standalone
family whose plain cut is the typewriter (`\ttdefault{jetbrains}`).

| Command | Effect |
|---------|--------|
| `\font{face}{size}{style}` | select typeface, size and style together |
| `\fontsize{size}` | change only the size, keeping the typeface and shape |
| `\fontscale{factor}{style}` | re-select the current typeface at the current size × factor |
| `\typeface{name}` | select a typeface by name |
| `\loadfont{name}{path}` | register a font file from disk under a typeface name |
| `\bold{…}` `\italic{…}` `\slanted{…}` `\smallcaps{…}` | shape-wrapping commands |
| `\serif{…}` `\sans{…}` `\mono{…}` | roman / sans / typewriter member of the super-family |
| `\itshape` `\slshape` `\bfseries` `\scshape` | declare italic / slanted / bold / small-caps for the group |
| `\upshape` `\mdseries` `\normalfont` | clear the shape axis / clear bold / return to the plain face |
| `\rmfamily` `\sffamily` `\ttfamily` | declare roman / sans / typewriter for the group |
| `\ttdefault{name}` `\sfdefault{name}` | name the fallback typewriter / sans family for `\mono` / `\sans` |
| `\accent{mark}{char}` | combine an accent mark with a base character |

### Bundled text faces

Latin Modern (`\font lmroman`) is the default body face. Three more Latin faces are bundled on the
JVM and Native builds (the browser build ships only Latin Modern):

- `\font cinzel` — an all-capitals inscriptional titling serif in the Trajan tradition, for titles
  and chapter openings, in six weights (`regular`, `medium`, `semibold`, `bold`, `extrabold`,
  `black`).
- `\font ebgaramond` — a Garamond revival for running text, with italic, the medium/semibold/
  extrabold weights and their italics, a real bold, and true small caps through the `smcp` feature.

Any other face is brought in with `\loadfont{name}{path}`.

### Glyph fallback (Cyrillic, Greek)

A codepoint the current face has no glyph for is set from New Computer Modern (`\font newcm`) rather
than drawn as a missing-glyph box, so **Cyrillic and Greek work in ordinary text with no setup** —
`Привет`, `αβγ` — and, because New Computer Modern *is* Computer Modern, the substituted glyphs sit
invisibly beside Latin Modern.

The substitution matches the weight and slope of the text around it: Cyrillic inside a `\bold{…}`
heading is set from the fallback's bold cut, so the weight does not break mid-phrase. The family role
is the one thing not carried over — the fallback stands in for a face's *coverage*, not its family, so
Cyrillic inside `\mono` is set in the fallback's roman rather than sent to the typewriter family, which
is no likelier to have the glyph.

## Chinese, Japanese and Korean

Chinese and Japanese text has no spaces between characters, so the line breaker inserts its own break
opportunities between adjacent characters and applies kinsoku — a closing mark never starts a line and
an opening mark never ends one — then justifies the line by stretching the inter-character space. This
happens automatically for any run of Chinese or Japanese text; the line breaker is otherwise unchanged.

Noto Serif CJK is bundled with a face for each region, since Han (kanji/hanja) has region-specific
glyph forms and Korean adds the Hangul syllables the Chinese faces do not carry:

- `\font cjksc` — Simplified Chinese (also the default `noto`-adjacent face for Han)
- `\font cjktc` — Traditional Chinese
- `\font cjkjp` (alias `\font japanese`) — Japanese, with the Japanese kanji forms and the kana
- `\font cjkkr` (alias `\font korean`) — Korean Hangul (and Hanja)

Each has a regular and a bold cut (`\font cjkjp 12 bold`). **Korean breaks differently:** it is written
with interword spaces, so its lines break at those spaces the way Latin does, rather than between
syllables. A break inside a Korean word is offered only as a last resort, at a heavy penalty, so a run
with no space in it still wraps instead of overflowing. Any other CJK font can be brought in with
`\loadfont{name}{path}`.

## Devanagari (Hindi)

Devanagari text is shaped a syllable at a time: consonants fuse into conjuncts and half-forms, the
short-i sign and a word-initial reph are reordered, a nukta composes with its consonant, and the
vowel signs are positioned by the font — all automatically from text typed in Unicode order. Noto
Serif Devanagari is bundled: `\font devanagari 12 regular` (or the alias `\font hindi`), with a
bold cut (`\font devanagari 12 bold`). See [Devanagari (Hindi)](/guide/devanagari/) for the full
treatment.

## Bengali–Assamese

Bengali text is shaped a syllable at a time: consonants fuse into conjuncts, the pre-base i/e/ai
signs are reordered, the two-part o and au signs are split into a front and a trailing part, a
word-initial reph and a post-base ya-phalaa are formed, a nukta composes with its consonant, and the
vowel signs are positioned by the font — all automatically from text typed in Unicode order. Noto
Serif Bengali is bundled: `\font bengali 12 regular` (or the alias `\font assamese`), with a bold cut
(`\font bengali 12 bold`). See [Bengali–Assamese](/guide/bengali/) for the full treatment.

## Gurmukhi (Punjabi)

Gurmukhi text is shaped a syllable at a time: a consonant joined by the virama takes a subjoined
*pairin* form beneath the base, the pre-base sihari (i-sign) is reordered, and the addak, the nasal
tippi and bindi, and the vowel signs are positioned by the font — all automatically from text typed
in Unicode order. Unlike Devanagari and Bengali, Gurmukhi has no reph. Noto Serif Gurmukhi is
bundled: `\font gurmukhi 12 regular` (or the alias `\font punjabi`), with a bold cut
(`\font gurmukhi 12 bold`). See [Gurmukhi (Punjabi)](/guide/gurmukhi/) for the full treatment.

## Telugu

Telugu builds downward: a consonant joined by the virama is drawn as a subscript beneath the base, so
the base of a conjunct is its *first* consonant rather than its last, and a consonant and its vowel sign
commonly fuse into a single drawn glyph. A vowel sign belongs to the base, so it is moved back across
the subscripts to reach it — `ప్రేమ` joins its vowel to the *pa*, not to the subjoined *ra*. The ai sign
is split into the two parts the font draws, and a word-initial *ra* with a virama forms no reph. All of
it happens automatically from text typed in Unicode order. Noto Serif Telugu is bundled:
`\font telugu 12 regular`, with a bold cut (`\font telugu 12 bold`). See [Telugu](/guide/telugu/) for
the full treatment.

## Thai

Thai is written without spaces between words and uses them to separate phrases instead, so a run of it
would reach the line breaker as one box and overflow the measure. The breaker offers somewhere to break
between orthographic clusters, and never parts a vowel or tone mark from its consonant, nor strands a
pre-posed vowel from the consonant that follows it. Charm is bundled: `\font thai 12 regular` (or
`\font charm`), with a bold cut.

Vowels and tone marks are placed on their consonant by the font's anchors, so the stacked combinations
Thai builds set correctly.

**One limitation to know.** Thai lines properly break at *word* boundaries, and finding those needs a
dictionary, which texish does not carry — so a cluster break may land inside a word. Because of that it
is offered only as a last resort, at a heavy penalty: text with normal phrase spacing breaks at those
spaces and never sees one.

## Right-to-left scripts

`\rtl` and `\ltr` set the paragraph base direction. Faces are bundled for both scripts, each in a
regular and a bold cut: `\font hebrew` (Noto Serif Hebrew) and `\font arabic` (Noto Naskh Arabic),
with the traditional book hands `\font amiri` (a Naskh alternative to Noto for Arabic) and
`\font ezra` (Ezra SIL, the Biblia Hebraica Stuttgartensia face, with vowel points and cantillation
marks) alongside them. Hebrew niqqud, Arabic cursive joining, the lam-alef and Allah ligatures, and
Arabic harakat are all applied automatically from the text in logical order — see
[Right-to-left text](/guide/right-to-left/) for the full treatment.

## Text symbols

| Command | Symbol |
|---------|--------|
| `\copyright` `\textregistered` `\texttrademark` | © ® ™ |
| `\dag` `\ddag` `\S` `\P` | † ‡ § ¶ |
| `\pounds` `\textdegree` `\textbullet` | £ ° • |
| `\textemdash` `\textendash` | — – |

## Mathematics

| Command | Effect |
|---------|--------|
| `$…$` `$$…$$` | inline and display math |
| `^` `_` | super/subscript |
| `\frac{a}{b}` `a \over b` `a \atop b` | fractions (`\atop` stacks with no rule) |
| `\dfrac{a}{b}` `\tfrac{a}{b}` | forced display-style / text-style fractions |
| `\binom{n}{k}` `\dbinom` `\tbinom` | binomial coefficients |
| `\sqrt{x}` `\sqrt[3]{x}` | roots |
| `\sum` `\int` `\prod` `\limits` `\nolimits` | big operators (`\nolimits` forces scripts to the side) |
| `\sin` `\cos` `\arctan` `\log` `\exp` `\lim` `\max` `\det` `\Pr` … | the standard upright operator names |
| `\operatorname{…}` | an upright custom operator (like `\sin`, with operator spacing) |
| `\bmod` `\pmod{m}` `\mod{m}` `\pod{m}` | modular-arithmetic forms |
| `\implies` `\impliedby` `\iff` | spaced implication arrows |
| `\ldots` `\cdots` `\vdots` `\ddots` `\dots` | ellipses (`\dots` is the low one, as `\ldots`) |
| `\left( … \right)` | stretchy delimiters |
| `\hat` `\vec` `\widehat` | accents |
| `\overset{a}{b}` `\underset{a}{b}` `\substack{a \\ b}` | stack material above / below / in a script pile |
| `\boxed{…}` | a framed formula |
| `\overline{…}` `\underline{…}` | a rule over / under the content (`\underline` also works in text) |
| `\text{…}` `\mathcal{…}` | upright / calligraphic |
| `\mathbf` `\mathit` `\mathrm` `\mathsf` `\mathtt` `\mathbb` `\mathfrak` | math alphabets (bold, italic, roman, sans, mono, blackboard, fraktur) |
| `\phantom` `\hphantom` `\vphantom` | reserve a box's size without drawing it |
| `\smash{…}` | draw a box but report zero height and depth |
| `\longrightarrow` `\rightleftharpoons` | long / equilibrium arrows |
| `\,` `\:` `\;` `\!` `\quad` `\qquad` | thin / medium / thick / negative-thin / 1em / 2em space |
| `\matrix` `\pmatrix` `\bmatrix` `\vmatrix` `\Vmatrix` `\Bmatrix` `\smallmatrix` `\cases` | matrices |
| `\begin{aligned}…\end{aligned}` | equations aligned on their `&` relations (`\\` between rows) |
| `\eqno(…)` | display equation number |

## Boxes, spacing, and pages

| Command | Effect |
|---------|--------|
| `\hbox` `\vbox` `\vtop` | explicit boxes |
| `\mbox{…}` | an unbreakable horizontal box of its natural width |
| `\makebox[width][l\|c\|r\|s]{…}` | a box of a set width, content aligned (width = a dimension or a `\linewidth` factor) |
| `\parbox[t\|c\|b][height][t\|c\|b\|s]{width}{…}` | set text (one or more paragraphs) in a box of the given width, aligned on the baseline; the optional `[height]` fixes the box height and `[inner-pos]` places the content within it (top/centre/bottom/stretch, default = outer pos) |
| `\begin{minipage}[t\|c\|b][height][t\|c\|b\|s]{width}…\end{minipage}` | the environment form of `\parbox`, same optional `[height]`/`[inner-pos]` (over the `\beginminipage`/`\endminipage` primitives) |
| `\wrapbox{side}{width}{…}` | anchor a box at the left/right margin so following text wraps beside it |
| `\wrapshape{shape}{side}{width}{…}` | the same, with text following a non-rectangular silhouette |
| `\cutout{width}{height}{side}` `\cutshape{shape}{width}{height}{side}` | reserve a rectangular / shaped hole in the running text |
| `\clearwrap` | drop past the foot of the current wrap before the next full-width block |
| `\newlength{name}` `\setlength{name}{d}` `\addtolength{name}{d}` | declare / set / adjust a length (a dimension variable, read back as `\name`) |
| `\centerline` `\leftline` `\rightline` `\centering` | alignment |
| `\rlap` `\llap` | zero-width overlap boxes |
| `\kern` `\hskip` `\vskip` | rigid / glue spacing |
| `\strut` | an invisible zero-width box one baselineskip tall, so a line keeps a regular height and depth whatever its glyphs |
| `\smallskip` `\medskip` `\bigskip` | named vertical gaps |
| `\par` | end the current paragraph (the explicit form of a blank line; useful inside a macro) |
| `\indent` `\noindent` | force / suppress the next paragraph's indent |
| `\leavevmode` | begin a paragraph if none is open, keeping the running indent state — for inline content led by a box (e.g. a superscript) that a plain character would otherwise have started |
| `\rtl` `\ltr` | set the paragraph base direction right-to-left / left-to-right (see [Right-to-left text](/guide/right-to-left/)) |
| `\newpage` | end the current page and start a new one |
| `\ignorespaces` | swallow the spaces that follow (at the end of a macro body) |
| `\vfil` `\vfill` `\vss` `\hfil` `\hfill` `\hss` | stretchable fill |
| `\lower` `\raise` | shift a box vertically |
| `\setbox` `\box` `\copy` `\unhbox` `\unvbox` `\wd`/`\ht`/`\dp` `\isvoid` | box registers, their dimensions, splicing, and the empty test |
| `\vsplit name to:<dim>` | cut a saved vbox at a page-style breakpoint |
| `\columns {n} {…}` | set the body as *n* balanced columns (`gap:<dim>` before `{n}` sets the gutter) |
| `\leaders` `\cleaders` `\xleaders` | fill space by tiling a box |
| `\dotfill` `\hrulefill` | a dotted / ruled leader (e.g. contents lines) |
| `\discretionary{pre}{post}{no}` `\softhyphen` | author-controlled break points |
| `\penalty` `\nobreak` `\eject` | page-break control (breaks are chosen by cost) |
| `\topinsert` `\midinsert` `\botinsert` | plain-TeX edge floats (the `float` package builds `\figure`/`\table` on these) |
| `\mark{…}` | drop a mark for running heads (`topmark`/`firstmark`/`botmark`) |

The framing primitives read `fboxsep` / `fboxrule`; box and page behaviour is tuned through the
[parameters](../parameters/) (`hsize`, `parindent`, `baselineskip`, `tolerance`, the page-break
penalties, and so on).

## Rules, frames, and transforms

| Command | Effect |
|---------|--------|
| `\hrule` `\vrule` | a horizontal / vertical rule (`[width:…]`, `[height:…]`, `[depth:…]` size it) |
| `\fbox{…}` `\framebox{…}` | frame the content (padding `fboxsep`, rule `fboxrule`, current pen colour) |
| `\colorbox[alpha]{color}{…}` | fill the content's background with a colour, no frame (translucent via `[alpha]` or `#RRGGBBAA`) |
| `\fcolorbox{frame}{bg}{…}` | frame in one colour around a body filled with another (either may be `#RRGGBBAA`) |
| `\rotatebox{deg}{…}` | rotate the content counter-clockwise about its left baseline |
| `\scalebox{factor}[yfactor]{…}` | scale the content (one factor scales uniformly) |
| `\reflectbox{…}` | mirror the content left-to-right |
| `\resizebox{w}{h}{…}` | scale to a width and height (`!` for either keeps the aspect ratio) |
| `\raisebox{lift}{…}` | raise the content, adjusting its reported height and depth |

## Tables

| Command | Effect |
|---------|--------|
| `\halign{template \cr rows}` | a column-aligned table; `#` marks the cell slot in the template, `&` separates cells, `\cr` ends a row |
| `\noalign{…}` | between rows of an `\halign`, insert material (often an `\hrule`) outside the column alignment |
| `\omit` | at the start of a cell, skip its template and set the cell plain |
| `\tabular{cols}{body}` | the LaTeX form: `cols` is `l`/`c`/`r` columns and `\|` rules; the body uses `&`, `\\` and `\hline` |

## Verbatim and code

| Command | Effect |
|---------|--------|
| `\verb<delim>…<delim>` | inline verbatim: the char after `\verb` delimits, the text is set literally in monospace |
| `\code[language]{…}` | a code listing in the code face; `[language]` syntax-highlights it (see the code environment for unbalanced braces) |

Both read their text raw from the input, so — like LaTeX's `\verb` — they cannot be produced by a
macro. `\code` colours by the theme in `codetheme`; the `code` environment takes its language from
`codelang`.

## Language

These set the document's language, which decides both the hyphenation patterns the line breaker uses
and the typography the language sets its punctuation by.

| Command | Effect |
|---------|--------|
| `\usehyphenation{tag}` | enable the bundled patterns for a language tag (`en-us`, `es`, `fr`, …) and make it the document language |
| `\loadhyphenation{lang}{path}` | load Liang patterns from a `.tex` file and make `lang` the document language |
| `\language{lang}` | switch to an already-loaded language |

Declaring French — `\usehyphenation{fr}`, or any regional tag built on it — also switches on French
spacing of high punctuation. French sets a space before the colon, semicolon, exclamation and question
marks, and inside its guillemets: a full space before the colon, a narrower one before the rest. That
space neither stretches when the line is justified nor offers the line breaker a place to break, so the
mark cannot drift from its word or be carried alone to the head of the next line. Type the space or
leave it out as you prefer — `« Oui ! »` and `«Oui!»` set alike — and the comma and full stop, which
French sets tight as English does, are left alone.

## Hooks

| Command | Effect |
|---------|--------|
| `\addtohook{name}{code}` | append an unevaluated code fragment to a named hook |
| `\usehook{name}` | run every fragment registered on a hook, in order |

## Page geometry

`\geometry` sets the page frame the way LaTeX's geometry package does, but with texish
`name:value` options instead of a `key=value` list. It resolves the sheet, the text size,
and the margins, then sets `paperwidth`/`paperheight`, `hsize`/`vsize` and `hoffset`/`voffset`
globally — so a `\geometry` in the preamble holds for the whole document, and a later call
re-frames the pages that follow it (as LaTeX's `\newgeometry` does). The far margin is always
the remainder, so the frame closes by construction.

| Option | Effect |
|--------|--------|
| `paper:<name>` | a named sheet: `letter`, `legal`, `a3`, `a4`, `a5`, `a6` |
| `paperwidth:<dim>` `paperheight:<dim>` | the sheet size explicitly |
| `landscape:on` `portrait:on` | swap the sheet to its wider / taller orientation |
| `margin:<dim>` | one length for all four margins (`hmargin`/`vmargin` for one axis) |
| `left:` `right:` `top:` `bottom:<dim>` | the four margins individually |
| `textwidth:` `textheight:<dim>` (or `width:`/`height:`) | the text-block size directly |
| `centering:on` | centre the text block (`hcentering`/`vcentering` for one axis) |
| `headsep:` `footskip:<dim>` | the running-head and running-foot gaps |

Example: `\geometry paper:a4 margin:2cm` or `\geometry left:2in right:1in top:1.5in bottom:1in`.

## Page arrangement

`\arrange` chooses how finished logical pages are placed onto physical sheets — the output
routine. The default ships one page per sheet; the booklet arrangements and `nup` group several.
It must appear in the preamble, before any content, because the arrangement fixes the physical
sheet size when the output surface is created. Imposition happens in box space, above the
rendering backend, so it works on every output target.

| Form | Effect |
|------|--------|
| `\arrange simple` | one logical page per sheet (the default) |
| `\arrange two-up-booklet` | pages two-up in saddle-stitch folding order on a sheet twice as wide, each sheet folded once; the whole book nests as one signature |
| `\arrange four-up-booklet` | the two-up layout stacked two folded sheets high, on a sheet twice as tall as well; print, cut in half across the middle, then fold as the two-up booklet |
| `\arrange <booklet> duplexshift:<len>` | pull the front side `<len>` left to cancel a printer's front-to-back registration error (either booklet arrangement) |
| `\arrange two-up-booklet signature:<4k>` | fold into fixed groups of `4k` pages instead of one nested stack (also on `four-up-booklet`) |
| `\arrange nup rows:<r> cols:<c>` | tile an `r`×`c` grid of pages per sheet, in reading order |

The `two-up-booklet` arrangement sets the logical page with `\geometry` and lets the arrangement
size the sheet: for a pocket A6 booklet folded from A5 paper, `\geometry paper:a6 margin:10mm`
then `\arrange two-up-booklet` gives A5-landscape sheets carrying two A6 pages each. Print
double-sided, fold each sheet once down the middle, nest, and staple, and the pages read in order.
A run is padded with blank pages to a whole number of sheets. The name describes the imposition,
not the paper, so the same arrangement serves any page size — quarter-letter pages two-up on
half-letter sheets, say. Both booklet arrangements take `duplexshift:<len>` to cancel a duplexer's
front-to-back drift (see below).

The `four-up-booklet` arrangement prints the same booklet on the next paper size up for cutting: it
stacks the two-up layout two folded sheets high, so A6 pages fall four to an A4 sheet. Print
double-sided, cut the sheet in half across the middle into two half-height sheets, then fold and
nest those exactly as the two-up booklet — no page prints upside-down and nothing is slit. The
back is laid out for long-edge duplex; if a fold-test dummy comes out mis-collated, switch the
printer to long-edge binding. Padding blanks fall just before the last page, so the front and back
covers always land on the outermost sheet. If a duplexer lands the front a millimetre or two off
the back, `duplexshift:<len>` pulls every front side that far left to bring the two into register.

## Images and graphics

| Command | Effect |
|---------|--------|
| `\includegraphics[width=,height=,scale=]{path}` | place a PNG or JPEG |
| `\image{path}` | place an image at its natural size |
| `\defbitmap{name}{w}{h}{depth}{base64}` | define an inline bitmap from embedded data (so a package can carry a glyph) |
| `\usebitmap[width:… height:…]{name}` | place a bitmap defined by `\defbitmap` |
| `\picture width:… height:… {…}` | open a vector-graphics drawing |
| `\qrcode[ecc:q cell:… quiet:… dark:… light:…]{data}` | a QR code drawn as crisp vector modules (via the `io.github.edadma.qr` library) |

The drawing commands available inside `\picture` (paths, shapes, strokes, fills, transforms, text,
arrows) are listed in the [drawing reference](../drawing/).

## Floats and text wrapping *(`\use{float}`)*

The package upgrades `\figure`/`\table` to real edge floats and adds in-flow figures the
text flows around, all sharing one caption sequence (`\caption`, numbered on independent
figure and table counters, filing into the lists of figures and tables).

| Command | Effect |
|---------|--------|
| `\figure[htb]{…}` `\table[htb]{…}` | detach the body to a page edge (here / top / bottom) |
| `\wrapfigure{side}{width}{…}` `\wraptable{side}{width}{…}` | anchor the body at the margin (`side` = `l`/`r`); running text wraps beside it |
| `\wrapfigure[shape]{side}{width}{…}` | wrap text along a non-rectangular silhouette (`shape` = `ellipse`/`triangleup`/`triangledown`/`diamond`/`rect`) |
| `\caption{…}` | a numbered caption; the float it sits in decides whether it counts as a figure or a table |

Captions file into the lists of figures and tables that `document`'s `\listoffigs` / `\listoftabs`
print, resolved on the second typesetting pass.

## Chemistry *(`\use{chem}`)*

| Command | Effect |
|---------|--------|
| `\ce{CH4 + 2 O2 -> CO2 + 2 H2O}` | a reaction equation (mhchem-style) |
| `\dn{n}` `\up{n}` | formula subscript / superscript |
| `\atom{name}{x y}{label}` | place and label an atom |
| `\bond` `\dbond` `\tbond` | single / double / triple bond |

## Plotting *(`\use{plot}`)*

| Command | Effect |
|---------|--------|
| `\xrange{min}{max}` `\yrange{min}{max}` | the plot's data ranges |
| `\autorange{x y x y …}` | derive both ranges from the data |
| `\xcategories{A B C …}` | name the x ticks instead of numbering them |
| `\xlabel{…}` `\ylabel{…}` `\plottitle{…}` | axis labels and title |
| `\xstep{s}` `\ystep{s}` | force a tick step (default: a *nice* step) |
| `\xtickformat{pre}{suf}` `\ytickformat{pre}{suf}` | wrap numeric tick labels (e.g. `\$`, `\%`) |
| `\plot{ … }` | draw the axes, ticks, grid, and labels, then the body's series |
| `\lineplot[colour][label]{x y x y …}` | a polyline through the data points |
| `\scatter[colour][label]{x y x y …}` | a marker at each point (shape from `plotmarkshape`) |
| `\bars[colour][label]{x y x y …}` | a vertical bar to each point (`plotvalues` labels them) |
| `\areaplot[colour][label]{x y x y …}` | the band under the curve, filled to the baseline |
| `\stepplot[colour][label]{x y x y …}` | a staircase holding each value to the next x |
| `\fnplot[colour][label]{expr in x}` | a sampled curve of a function |
| `\bubble[colour][label]{x y size …}` | a translucent disc at each point, sized by a third value |
| `\errorbars[colour][label]{x y err …}` | a capped y ± err whisker at each point |
| `\trendline[colour][label]{x y x y …}` | the least-squares line of best fit |
| `\hline[colour]{y}` `\vline[colour]{x}` | a dashed reference line at a data value |
| `\legend[ne\|nw\|se\|sw]` | draw a key for the labelled series |

Both bracket arguments are optional: with no colour (or `[]`) a series takes the next
palette colour, and a label adds a legend entry.

## Geographic maps *(`\use{map}`)*

Draws maps over the `\picture` layer in the Web Mercator projection the OpenStreetMap
tile ecosystem uses. You declare the geographic window in longitude/latitude with
`\mapbounds`, then draw inside `\map{…}`, writing real coordinates everywhere —
`\place{35.23}{31.78}{Jerusalem}` lands at the true point. Because Mercator is separable,
the projection is two one-argument macros (`\mapx`, `\mapy`), parallel to `plot`'s `\px`/`\py`.
Base coastlines/rivers/land are far too dense to project a point at a time, so they are
projected *offline* into a `\basemap` macro of pre-projected paths and `\include`d beside the
document; overlays and furniture are projected at run time and register exactly. While the
package is loaded its `\map{…}` shadows the keyed-map `\map`.

| Command | Effect |
|---------|--------|
| `\mapbounds{lon0}{lat0}{lon1}{lat1}` | declare the window (south-west then north-east corner) and derive the projection |
| `\map{ … }` | draw the map: the body holds the base linework, overlay, and furniture |
| `\mapx{lon}` `\mapy{lat}` | project one coordinate to a page point (a number or a variable) |
| `\place[anchor:auto]{lon}{lat}{name}` | a dot with a label at a coordinate |
| `\marker[color:…]{lon}{lat}` `\label[anchor:center]{lon}{lat}{text}` | the dot or the label on its own |
| `\route[color:…]{lon lat lon lat …}` | a polyline through a list of coordinates |
| `\region[color:…]{lon lat lon lat …}` | a filled polygon through a list of coordinates |
| `\graticule[step:1]` | a latitude/longitude grid every `step` degrees |
| `\scalebar[km:50]{x}{y}` `\northarrow{x}{y}` | a scale bar / north arrow at a page position |
| `\neatline` | a border framing the map |

The base file sets the size configuration and calls `\mapbounds` itself, so the offline
linework and the run-time overlay cannot drift out of register.

## Macros and programming

| Command | Effect |
|---------|--------|
| `\def name args {body}` | define a macro (`[name:default]` optional args, `<name>` a verbatim argument) |
| `\newenvironment name {begin}{end}` | define a `\begin`/`\end` environment |
| `\let` `\global` `\gdef` | aliasing and global definitions |
| `\if` `\ifx` `\else` `\fi` | conditionals |
| `\calc{…}` `\+ \- \* \/` | arithmetic |
| `\round{value}{places}` | round a number to a fixed number of decimals |
| `\= \!= \< \> \<= \>=` | comparisons (each yields a capturable boolean) |
| `\for … \done` | iteration |
| `\the\name` | output a variable's value (`\the\pageno`, `\the\hsize`, …) |
| `\seq{…}` `\words{s}` `\head` `\tail` `\last` `\size` `\cat` `\range{a}{b}` | sequences; `\words` splits on whitespace, `\range` counts `a`–`b`, `\last` is the final element |
| `\upcase{…}` `\downcase{…}` `\trim{…}` | uppercase / lowercase / strip surrounding whitespace |
| `\map{…}` `\mapset` `\mapget` `\maphas` | keyed maps |
| `\message{…}` | write expanded text to standard error (a diagnostic; no page output) |
| `\oklch{L}{C}{h}` `\oklchof{color}` | build / read a colour in the Oklch space (lightness, chroma, hue) — derive shades by varying L |
| `\newcounter` `\setcounter` `\addtocounter` `\stepcounter` `\value` `\counterwithin` | named counters; `\counterwithin` resets a child when its parent steps |
| `\arabic`/`\roman`/`\Roman`/`\alph`/`\Alph`/`\fnsymbol` | format a number as digits / roman / letters / footnote symbols |
| `\use{name}` `\include{path}` | load a format / include raw input |
| `\usfm{path}` | typeset a Scripture file in USFM format through the `\usfm…` macros *(`\use{usfm}`)* |

## Scripture *(`\use{usfm}`)*

`\usfm{path}` reads a book in USFM (the Unified Standard Format Markers that Bible translations
are exchanged in) and typesets it, resolving the path beside the document as `\include` does. Every
visible decision is a redefinable macro, so a document sets the appearance with ordinary commands
*before* the call — `\def usfmwj t {\t}` to drop the red-letter words of Jesus, `\set usfmqbase {24}`
for a deeper poetry indent, and so on. The defaults give a print-Bible page: a large chapter figure,
superscript verse numbers, bold section headings with italic reference lines, justified prose and
ragged poetry, real footnotes, and the words of Jesus in red.

The reader handles the full USFM marker set — including USFM 3.0 word attributes and plus-nested
character markers — and a book's front-matter **introduction** (its own titles, outline, prose and
poetry, set a size quieter than the Scripture). Suppress the introduction with `\set usfmintro {0}`
before the call — a pocket portion that wants only the text needs no change to the USFM file.

| Macro | Styles |
|-------|--------|
| `\usfmmt{n}{…}` `\usfms{n}{…}` `\usfmr{n}{…}` `\usfmd{n}{…}` | book title / section heading / reference line / psalm superscription |
| `\usfmc{N}` `\usfmv{N}` | the chapter figure and the verse number |
| `\usfmp{n}` `\usfmm{n}` `\usfmq{n}` `\usfmli{n}` `\usfmb` | indented / flush paragraph, poetry line by level, list item, stanza break |
| `\usfmimt{n}{…}` `\usfmis{n}{…}` `\usfmiot{n}{…}` `\usfmio{n}{…}` `\usfmip{n}` `\usfmiq{n}` | introduction: title / heading / outline heading and entry / prose / poetry |
| `\usfmwj{…}` `\usfmref{disp}{target}` `\usfmnd{…}` | words of Jesus, a cross-reference link, the name of God |
| `\usfmfootnote{…}` with `\usfmfr` `\usfmft` `\usfmfqa` `\usfmfv` | a footnote and its reference / text / alternative / verse-number parts |

## Railroad diagrams *(`\use{railroad}`)*

| Command | Effect |
|---------|--------|
| `\railroad{ <grammar> }` | draw a railroad (syntax) diagram per rule from W3C-style EBNF |

The grammar is read verbatim. Rules are `name ::= expression`; an expression supports `\|`
(alternation), juxtaposition (concatenation), `?`/`*`/`+` (optional / zero-or-more /
one-or-more), `(…)` grouping, `"…"`/`'…'` terminals (rounded boxes), `[…]` character classes
(pointed hexagons), and bare names as nonterminals (square boxes). Labels are set in JetBrains
Mono with terminals in bold. Common shapes are drawn idiomatically: a separated list
`A (sep A)*` becomes `A` with `sep` on a return loop, and an optional choice `(A|B|C)?` folds
the skip into the choice. See the [guide](/guide/railroad/) for customising colours and sizes.

## Node-and-edge diagrams *(`\use{diagram}`)*

Declared inside a `\picture`. A node is a named, measured box of a shape; an edge is an arrow
between two named nodes that meets each node's true boundary.

| Command | Effect |
|---------|--------|
| `\node [shape] {name} {placement} {label}` | a node; shape ∈ box, round, stadium, diamond, parallelogram, ellipse, circle, hexagon, subroutine (default box) |
| `\edge [label] {from} {to}` | a straight arrow between two nodes |
| `\link [label] {from} {to}` | a straight undirected line (no arrowhead) |
| `\edgehv` / `\edgevh [label] {from} {to}` | an orthogonal arrow (across-then-down / down-then-across) |
| `\cedge [bend] {from} {to} {label}` | a curved arrow, bowed by `bend` points |
| `\loop {name} {label}` | a self-loop above a node |
| `\dgentry {name} {dir} {len}` | a short entry stub pointing into a node |

A placement is `at X Y`, `at (X,Y)`, or `below`/`above`/`left`/`right REF [gap]` (relative to
another node). The look is set with `\set` on the `dg…` variables (`dgfill`, `dglinecolor`,
`dgfont`, `dggap`, `dgarrow`, …).

The `flowchart` package (`\use{flowchart}`) adds role names — `\start` `\stop` (terminal),
`\process` (box), `\decision` (diamond), `\io` (parallelogram), `\subroutine` — and flow
edges `\flow` / `\branch` / `\rejoin`. The `automaton` package (`\use{automaton}`) adds
`\state` (circle), `\accepting` (double circle), `\initial`, `\trans`, `\arc` and
`\loopabove`. The `er` package (`\use{er}`) draws Chen-notation entity-relationship diagrams
— `\entity`, `\weakentity`, `\relationship`, `\weakrelationship`, `\attribute`,
`\keyattribute`, `\multivalued`, `\derived`, and `\connect` / `\connecttotal` with
cardinalities. All three are thin layers over `diagram`; see the [guide](/guide/diagrams/).

## Units

`pt` (the base unit, 1/72 in), `in`, `cm`, `mm`, `em`, `ex`.
