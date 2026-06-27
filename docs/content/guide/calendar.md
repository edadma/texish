---
title: "Calendars"
weight: 10
---

The `calendar` package draws month-grid calendars over the
[vector-graphics](/guide/graphics/) layer. A month is one `\picture` box — a title bar
with the month and year, a weekday header row, and a grid of day cells — so it flows in
the text like any other box: centred on a page, dropped into a paragraph, or tiled into a
year planner. Load it with:

```texish
\use{calendar}
```

The whole package is written in the document language — `\calc` does the date arithmetic
(the weekday of the first by a standard congruence, the Gregorian leap-year rule, the row
and column of each day), a pair of maps hold the month and weekday names and the month
lengths, and the grid itself is `\picture` lines, rectangles and placed labels. There is
no calendar-specific engine primitive.

## Drawing a month

`\calendar` takes a year and a month number (`1` = January):

```texish
\calendar{2026}{6}
```

Weeks start on Sunday, the weekend columns take a distinct ink, and the grid grows to the
four, five or six week-rows the month needs. February is twenty-nine days long exactly
when the Gregorian rule says so, so `\calendar{2024}{2}` shows the leap day.

## Weeks starting on Monday

Set `calweekstart` to `1` for the ISO convention: the header rotates so Monday leads and
the weekend pair moves to the right of the grid.

```texish
{\set calweekstart {1}
 \calendar{2026}{6}}
```

Wrapping the call in a group scopes the change, so it affects only that one calendar.

## Highlighting a day

Set `caltoday` to a day number to draw a tinted cell behind it — the "today" mark on a
wall calendar. It is ignored when `0` or outside the month's range.

```texish
{\set caltoday {4}
 \calendar{2024}{2}}
```

## Customising the look

Every visual property is a configuration variable set with `\set` after `\use`, before the
`\calendar` call. Sizes are in points (72 per inch); the grid is always seven columns wide.

| Variable | Default | Effect |
|----------|---------|--------|
| `calcw` / `calch` | `34` / `30` | day-cell width and height |
| `calheadh` | `18` | weekday header-row height |
| `caltitleh` | `26` | month/year title-bar height |
| `calweekstart` | `0` | `0` weeks start on Sunday, `1` on Monday |
| `caltoday` | `0` | day to highlight (`0` = none) |
| `caltitlesize` | `14` | month/year type size |
| `calheadsize` | `9` | weekday-abbreviation size |
| `caldaysize` | `11` | day-number size |
| `caltitlebg` / `caltitleink` | `#2c5282` / `white` | title-bar fill and ink |
| `calheadbg` / `calheadink` | `#edf2f7` / `#2d3748` | weekday header fill and ink |
| `calgrid` | `#cbd5e0` | grid-line ink |
| `caltext` | `#1a202c` | weekday day-number ink |
| `calweekendink` | `#c53030` | weekend (Sat/Sun) ink, header and numbers |
| `caltodayfill` | `#fefcbf` | highlight fill behind the `caltoday` cell |

```texish
{\set calcw {54}\set calch {46}
 \set caltitlebg {#9c4221}\set calheadbg {#fffaf0}
 \set calgrid {#dd9b6c}\set calweekendink {#b7791f}
 \calendar{2026}{12}}     % a warm wall-calendar page
```

## Several months together

Because a calendar is just a box, a row of `\calendar` calls in an `\hbox` sets months
side by side — the start of a year planner:

```texish
{\set calcw {26}\set calch {22}
 \hbox{\calendar{2026}{1}\hskip 12pt\calendar{2026}{2}\hskip 12pt\calendar{2026}{3}}}
```
