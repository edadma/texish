---
title: "Node-and-edge diagrams"
weight: 11
---

The `diagram` package draws node-and-edge diagrams — block diagrams, state machines,
flowcharts, dependency graphs — over the [vector-graphics](/guide/graphics/) layer. A
**node** is a named, measured box of some shape; an **edge** is an arrow between two named
nodes. Three thin presets build on it: [`flowchart`](#flowcharts) gives the shapes their
program-flowchart meanings, [`automaton`](#automata) gives them finite-state-machine
meanings, and [`er`](#entity-relationship-diagrams) draws entity-relationship diagrams.

```texish
\use{diagram}
```

Everything is declared inside a [`\picture`](/guide/graphics/): open one, declare nodes and
edges into it, and the whole diagram is a single box that flows in the text.

```texish
\picture width:3.4in height:2.6in {
  \node{start}{at 1.2in 2.2in}{Begin}
  \node[diamond]{check}{below start}{ok?}
  \node{done}{below check 40}{Finish}
  \edge{start}{check}
  \edge[yes]{check}{done}
}
```

## The model

A node records its centre, half-width and half-height (sized to its label), and publishes
its centre as a named picture coordinate — so `(start)` works in raw picture code too. Every
edge reads that geometry back and attaches to the node's **true boundary**, along the line
toward the other node, so an arrow always meets the outline cleanly whatever the shape:
straight, diagonal, elbowed and curved edges all land on the edge of the box, the diamond,
the circle or the parallelogram, never inside it and never short of it.

There is no diagram-specific engine primitive — placement, the shapes and the arrow routing
are all document language over the picture primitives.

## Nodes

```
\node [shape] {name} {placement} {label}
```

The optional `[shape]` defaults to `box`. The recognised shapes:

| Shape | Drawn as |
|-------|----------|
| `box` | a rectangle (the default) |
| `round` | a rounded rectangle |
| `stadium` | a rectangle with fully rounded ends |
| `diamond` | a rhombus (decision) |
| `parallelogram` | a sheared rectangle (input/output) |
| `ellipse` | an ellipse |
| `circle` | a circle (always sized square) |
| `hexagon` | a hexagon with pointed ends |
| `subroutine` | a rectangle with side bars (a predefined process) |

### Placement

The `{placement}` argument is a small word language. Relative placement measures from the
reference node's boundary, then steps in by the gap and this node's own half-size, so
neighbours sit a true gap apart whatever their sizes.

| Placement | Meaning |
|-----------|---------|
| `at X Y` | an absolute coordinate, two scalar expressions (`at 1in 8in`) |
| `at (X,Y)` | an absolute coordinate as one space-free point |
| `below REF` | centred under node `REF`, the default gap between the boxes |
| `below REF 40` | …with an explicit gap of 40 points |
| `above REF` | centred over `REF` |
| `right REF` | to the right of `REF`, vertically aligned |
| `left REF` | to the left of `REF` |

```texish
\node{a}{at 1in 4in}{First}
\node{b}{below a}{Second}        // default gap below a
\node{c}{right b 50}{Beside}     % 50pt to the right of b
```

## Edges

Every edge inks in the configured colour and caps with the configured arrowhead. An optional
`[label]` rides beside the edge.

| Command | Route |
|---------|-------|
| `\edge [label] {from} {to}` | a straight arrow between the two nodes |
| `\link [label] {from} {to}` | a straight **undirected** line (no arrowhead) |
| `\edgehv [label] {from} {to}` | orthogonal: leaves horizontally, turns, enters vertically (`-\|`) |
| `\edgevh [label] {from} {to}` | orthogonal: leaves vertically, turns, enters horizontally (`\|-`) |
| `\cedge [bend] {from} {to} {label}` | a curved arrow, bowed sideways by `bend` points |
| `\loop {name} {label}` | a self-loop above a node |
| `\dgentry {name} {dir} {len}` | a short entry stub pointing into a node (`dir` = north/south/east/west) |

`\edgehv` and `\edgevh` are the natural way to send a decision's side branch across and then
down, or to route a feedback line. When the two nodes happen to be aligned on the elbow's
axis they reduce to a straight edge automatically.

`\cedge`'s bend is positive to bow left of the `from`→`to` direction; give the two directions
of a back-and-forth pair the **same** bend and they separate into two arcs instead of drawing
one arrow over another.

```texish
\picture width:4.6in height:1in {
  \node[circle]{sum}{at 0.5in 0.5in}{$\Sigma$}
  \node{ctrl}{right sum 40}{Controller}
  \node{plant}{right ctrl 40}{Plant}
  \edge[$e$]{sum}{ctrl}
  \edge[$u$]{ctrl}{plant}
}
```

## Customising the look

Set any of these with `\set` before drawing. Sizes are in points unless they carry a unit.

The two colours default to `auto`, meaning the diagram takes them from the document it sits in
rather than naming its own: the ink is whatever the pen is, and the fill is a tint of the page —
its lightness moved a little of the way toward the ink, which is the familiar cool grey-blue on
white paper and a matching slight lift on a dark one. That is what keeps a diagram legible when
the document is rendered in a dark scheme, where a named dark ink would disappear into the page
and a named pale fill would swallow the labels drawn over it in the document's own pen. Naming a
colour explicitly overrides the derivation, for a figure with a palette of its own.

| Variable | Default | Effect |
|----------|---------|--------|
| `dgfill` | `auto` | node fill colour; `auto` derives a tint of the page |
| `dglinecolor` | `auto` | ink for outlines and edges; `auto` follows the document's pen |
| `dgtintl` / `dgtintc` / `dgtinth` | `0.0373` / `0.012` / `259.8` | the `auto` fill: how far its lightness moves from the page toward the ink, and the tint's Oklch chroma and hue |
| `dglinewidth` / `dgedgewidth` | `1pt` / `1.1pt` | outline and edge widths |
| `dgface` / `dgfont` | `lmroman` / `11` | label typeface and size |
| `dgpadx` / `dgpady` | `13` / `9` | padding between a label and its box edge |
| `dgminw` / `dgminh` | `52` / `30` | minimum node width / height |
| `dggap` | `28` | default boundary gap for relative placement |
| `dgarrow` / `dgarrowsize` | `stealth` / `8pt` | edge arrowhead style and length |
| `dglabelface` / `dglabelfont` | `lmroman` / `9` | edge-label typeface and size |
| `dgcorner` / `dgslant` | `7` / `13` | rounded-corner radius / parallelogram slant |

## Flowcharts

The `flowchart` package names the diagram shapes by their roles and reads the edges as flow.
It draws nothing of its own — each command maps to a `diagram` node or edge.

```texish
\use{flowchart}
```

| Role | Shape | Edge | Route |
|------|-------|------|-------|
| `\start` `\stop` | a terminal (stadium) | `\flow [label] {from} {to}` | straight |
| `\process` | a rectangle | `\branch [label] {from} {to}` | across, then in (`-\|`) |
| `\decision` | a diamond | `\rejoin [label] {from} {to}` | down, then back (`\|-`) |
| `\io` | a parallelogram | | |
| `\subroutine` | a rectangle with side bars | | |

Each role takes the same arguments as `\node` — a name, a placement and a label.

```texish
\picture width:3in height:4in {
  \start{begin}{at 1.2in 3.6in}{Start}
  \io{read}{below begin}{Read $n$}
  \decision{check}{below read}{$n > 0$?}
  \process{print}{below check}{Print $n$}
  \stop{done}{below print}{End}
  \flow{begin}{read}
  \flow{read}{check}
  \flow[yes]{check}{print}
  \flow{print}{done}
  \branch[no]{check}{done}
}
```

## Automata

The `automaton` package gives the circle and the edges their finite-state-machine meanings.

```texish
\use{automaton}
```

| Command | Meaning |
|---------|---------|
| `\state {name} {placement} {label}` | a state (a circle) |
| `\accepting {name} {placement} {label}` | an accepting/final state (a double circle) |
| `\initial {name}` | mark a state as the start, with an incoming arrow from the left |
| `\trans [symbol] {from} {to}` | a transition (a straight labelled arrow) |
| `\arc [bend] {from} {to} {symbol}` | a curved transition (for a back-and-forth pair) |
| `\loopabove {name} {symbol}` | a self-transition (a loop above the state) |

```texish
\picture width:5in height:1.7in {
  \state{q0}{at 0.7in 0.7in}{$q_0$}
  \state{q1}{right q0 78}{$q_1$}
  \accepting{q2}{right q1 78}{$q_2$}
  \initial{q0}
  \trans[$a$]{q0}{q1}
  \trans[$b$]{q1}{q2}
  \loopabove{q0}{$b$}
  \loopabove{q2}{$a, b$}
}
```

The presets carry no geometry of their own, so anything `diagram` exposes — the other shapes,
the orthogonal and curved edges, and the full configuration — is available alongside them.

## Entity-relationship diagrams

The `er` package draws entity-relationship diagrams in **Chen notation**: an entity is a
rectangle, a relationship a diamond, an attribute an ellipse, joined by undirected lines
(`diagram`'s `\link`, no arrowhead) that carry cardinalities.

```texish
\use{er}
```

| Command | Drawn as |
|---------|----------|
| `\entity {name} {placement} {label}` | a rectangle |
| `\weakentity {name} {placement} {label}` | a double rectangle |
| `\relationship {name} {placement} {label}` | a diamond |
| `\weakrelationship {name} {placement} {label}` | a double diamond (identifying relationship) |
| `\attribute {name} {placement} {label}` | an ellipse |
| `\keyattribute {name} {placement} {label}` | an ellipse with the label underlined |
| `\multivalued {name} {placement} {label}` | a double ellipse |
| `\derived {name} {placement} {label}` | a dashed ellipse |
| `\connect [cardinality] {a} {b}` | an undirected line, the cardinality set beside it |
| `\connecttotal [cardinality] {a} {b}` | a double line (total participation) |

```texish
\picture width:4.4in height:2in {
  \keyattribute{ssn}{at 0.5in 1.7in}{ssn}
  \entity{emp}{at 1.1in 1in}{Employee}
  \relationship{wf}{right emp 60}{Works For}
  \entity{dept}{right wf 60}{Department}
  \connect{emp}{ssn}
  \connect[N]{emp}{wf}
  \connect[1]{wf}{dept}
}
```

The weak forms and the double ellipse add a second outline (like an automaton's accepting
state); the dashed ellipse and the underlined key attribute are an ordinary ellipse with a
line-style or an `\underline`d label. Everything else is a `diagram` node or `\link`.

## A note on spacing

These packages only load `diagram`, not the [document format](/guide/document-format/). A
bare-engine document is vertically justified by default, which on a sparse page stretches the
small inter-paragraph glue and spreads the paragraphs apart. Set `\raggedbottom` (what the
document format does for you) to keep natural spacing:

```texish
\set raggedbottom {1}
```

To centre a figure, an `\hbox` with `\hfil` on each side does it:

```texish
\def centerline c {\hbox to:\hsize {\hfil\c\hfil}}
\centerline{\picture width:3in height:2in { … }}
```
