# Design note: a computational sublanguage for texish

**Status:** DESIGN — not yet implemented. Grammar core decided; v2/v3 surface still open.
**Motivation:** heavy numeric work (map projection, plot sampling, future layout algorithms) is
slow and awkward in the macro/expansion language.
**Analogy:** LuaTeX — embed a real language for computation, keep macros for the light stuff — but
done more cleanly (see below).

## Motivation

The macro/document language is excellent for markup and light orchestration, but the expansion
model is slow and clumsy for tight numeric loops. The smoking gun is the `map` package: it projects
base linework **offline** and bakes in pre-projected paths, precisely because projecting tens of
thousands of vertices "one `\calc` at a time" at runtime is too slow. The same ache lurks in `plot`
(`\fnplot` sampling), any future layout algorithm, and data-driven documents. This is not a
capability gap — the macro language is Turing-complete — it is a **performance and ergonomics** gap
for computation.

**A second data point, 2026-08-19.** The `qrcode` package is an ISO/IEC 18004 encoder written
entirely in the document language, and it works — but a version-4 symbol takes about four seconds on
the native binary, against effectively zero for the compiled primitive it replaced. Three rounds of
algorithmic tuning took it from seventeen seconds to that; what remains is the evaluator's
per-operation cost, not the algorithm. `datamatrix`, which has no mask search, is well under a
second. So the ache is real and measurable outside `map` and `plot`, and it is *throughput*, not
expressiveness: the language could say everything the standard required.

## Guiding principles

1. **Host the language in Scala; do not embed a foreign runtime.** texish runs on three backends
   (JVM, JS, Native). Whatever the sublanguage is, it must run identically on all three, which rules
   out FFI-ing the C Lua library and points at a tree-walking evaluator written in Scala. "Lua-like,"
   not "Lua."
2. **Grow one language, don't bolt on a second.** Program regions are lexically-delimited,
   statically-parseable islands in the *same* grammar, producing the *same* AST as the surrounding
   document. Unlike LuaTeX, there is no `node`/`token` reflection layer — a program manipulates the
   real document constructs because they are the language's constructs.
3. **Parse-time AST, one evaluator.** The sublanguage is parsed into the same tree up front (no
   runtime re-parse), so syntax errors surface before typesetting starts. At execution it is all one
   AST walked by one evaluator.
4. **The lexical boundary is what makes this tractable.** You generally cannot statically parse
   arbitrary macro material (catcode games, `\expandafter`, control sequences built at runtime). But
   a lexically-delimited program region *is* statically parseable. The boundary is not just
   ergonomics — it is the thing that lets "one big AST" hold.

## The unified evaluator

There is a single evaluator that **always carries an environment** — empty/global at the top level,
populated inside a program region. Document mode is not a different machine; it is the same evaluator
with only the document productions in play and nothing in scope. Committing to this unified evaluator
(not merely a shared parser) is the real architectural consequence of the design.

### Two execution models, meeting only at call boundaries

- **Macros stay expansion-based** (textual substitution) and specifically do **not** capture a
  caller's program bindings.
- **Primitives become environment-aware** (easy — fixed semantics).
- **At a call, program expressions in argument position are evaluated to values first**, then handed
  to the primitive/macro, which runs in its own world. So it is "same execution, with an
  argument-evaluation step whenever a program expression sits in argument position." This is what
  keeps the two execution models from bleeding into each other.

## First-class boxes

Boxes are first-class **immutable** values.

- **Statement position emits; expression position yields a value.** `\hbox{…}` on its own in a loop
  ships a box to the current list (exactly as in document mode); `let b = \hbox{…}` evaluates to a
  box value you can store, measure, and place later.
- **Metrics via dot access:** `b.width`, `b.height`, `b.depth`, returning dimensions.
- **Markup-as-argument is free:** because a box is a value, `\hbox{…}` drops straight into a
  function-form argument list — that is how typeset content is passed to a function-form command.
- **Immutable by default** keeps the two-pass model honest (no box quietly changing shape between
  passes). A separate, explicit mutable builder type is added only if a real case needs incremental
  construction (the `String`/`StringBuilder` split).

## Syntax

### The two bridges

- **Region entry** — `{{{ … }}}` enters program mode. (A primitive-introduced `\eval{…}` /
  `\script{…}` whose *body* is lexed in program mode is the leading alternative for v1: it reuses the
  existing `readArgument` machinery and sidesteps the `{{{` / triple-group delimiter collision.)
- **Unquote** — `\(expr)` splices a program value into markup content (`\hbox{Chapter \(n)}`). This
  is the single, explicit doc←program splice; not the primary path (the function form below handles
  computation), just the escape hatch for values-in-prose.

### Universal command sigil

`\cmd` is the command/primitive/macro sigil in **both** modes; bare identifiers are program
variables/functions; operators are program operators. So `\hbox{…}` is literally the same grammar
production in program mode — the shared AST falls out rather than being engineered. The program
region only *enables additional productions* (bare-identifier expressions, `for`/`if`/`let`, `[]`).

### Two call forms, one node

- **Markup form** (unchanged): `\place{35.23}{31.78}{Jerusalem}` — braces are typeset content.
- **Function form** (program mode): `place(mapx(c.lon), mapy(c.lat), c.name)` — parens, arguments are
  program expressions.

Both parse to the **same node**; they differ only in how arguments are read (doc-content-with-unquote
vs program-expressions). Code uses `()`, markup uses `{}`, so the brace overload never bites.

### Computation vs output split

- Pure computation → program functions/expressions.
- Emitting typeset material → commands (either call form).

```
{{{
  func mapx(lon) = (rad(lon) - mapMx0) * mapScale
  func mapy(lat) = (ln(tan(pi/4 + rad(lat)/2)) - mapMy0) * mapScale

  for c in cities {
    let tag = \hbox{\(c.name)}          // a first-class box value
    if tag.width < maxLabel {
      place(mapx(c.lon), mapy(c.lat), tag)
    }
  }
}}}
```

This is the `map` projection loop with the offline-baking hack gone: pure functions for the math, a
real `for`, first-class boxes with metrics, `place(…)` emitting.

### Records vs blocks — position-based disambiguation

`{` is never allowed to mean two things in the same grammatical position:

- `{` **in expression position** (RHS of `let`, an argument, an operand) is *unconditionally* a
  **record**.
- `{` **at statement/body position** (after a control-flow keyword or `{{{`) is *unconditionally* a
  **block**.

No lookahead; `:` and `,` are *confirmation*, not the discriminator (relying on them leaks on empty
`{}`, on `:` in ternaries/type annotations, and on `,` in tuples, and needs unbounded lookahead).
**Decided: no block-as-expression** (control-flow-only blocks), which is what makes the position rule
airtight — with value-blocks you would reintroduce the ambiguity and want a `do { … }` keyword
instead. Empty `{}` is the empty record; a bare record-literal-as-statement is disallowed (costless).

### Statements — borrowed from sysl

Lift syntax from sysl so authors moving across the languages feel at home and validated decisions are
reused:

- `let` (immutable binding) / `var` (reassignable) — the binding axis, orthogonal to box-value
  immutability.
- `for i in 0..<n` (exclusive) / `0..n` (inclusive); `for c in coll`.
- `if`/`else`, `while`, `func name(args) = expr` (or block body), `match`, `if x is Pattern`.
- `//` line comments (safe here — the region is a clean lexical world, no module-newline issue).
- Field access `.`, indexing `[]`.
- **Dimension-aware values** (`2pt`, `em`, `ex`), not just floats.

### Emit

- **A command or box literal in statement position emits**, exactly as in document mode: `\hbox{…}`
  or `place(…)` alone in a loop ships to the current list, no keyword.
- **A bare value expression does not** — a lone `b`, or `x + 1`, as a statement is not an implicit
  emit (that is the surprising, typo-prone case). To ship a *stored* box, write it explicitly:
  **`emit b`**. (`emit` over `\put` — program-mode-native, and dodges a collision with picture-mode
  `\put` *and*, since 2026-08-19, with the document language's own `\put`, which writes one item of
  a sequence. The name is doubly taken now, so this choice is settled rather than merely preferred.)

## Value model

Dynamically typed with optional annotations (matches the domain and the stringy macro boundary; static
typing gets awkward exactly at the seam). Values: numbers, dimensions, booleans, strings, arrays,
records, functions/closures, and boxes (and glue). This grows the existing `Value` model, which
already reads a `Glue`'s natural size.

## Two-pass model and sandboxing

texish typesets twice (label resolution). Program regions with side effects must be idempotent or
pass-aware, the same discipline counters already follow — so the sublanguage encourages pure compute
and treats state like counters. Sandboxing (no wall-clock, seeded randomness only, IO through
controlled seams) is easier here than in LuaTeX because it is your own evaluator over your own AST —
you control exactly which operations exist. Bake the capability limits in from the start.

## `\calc` migration

`\calc` is not a separate project — an expression is the degenerate program. Reimplement `\calc` on
top of the shared expression AST (parsed once, walked N times) and **keep `\calc{…}` valid surface
syntax**, so every existing package keeps working but stops re-parsing per call. Deprecate gradually.
The grammar changes underneath; the surface stays.

## Staging

- **v1** — program regions; program functions + control flow + data structures; the function-call
  form for commands; first-class immutable boxes with metrics; `\calc` reimplemented as the
  degenerate expression case (backward-compatible surface); AST-walked. Ships the retirement of the
  `map` offline-baking hack. Program regions return values / emit via commands; **read-only** access
  to parameters.
- **v2** — richer box-emit API surface; quasiquote / inward macro nesting inside program regions;
  mutable builder type; parameter/counter mutation.
- **v3 (only if needed)** — LuaTeX-style pipeline callbacks; **closure-compilation of hot AST
  subtrees** (`AST => (Env => Value)`) if tree-walking proves too slow for the numeric loops.
  Measure first; AST-walking already kills the re-parse cost that forces today's workarounds.

## Open questions

- Exact region delimiter: raw `{{{ }}}` vs primitive-introduced `\eval{…}` / `\script{…}` (leaning
  primitive-introduced for v1).
- Record-literal exact syntax and whether to support field shorthand.
- Whether a macro body may contain a program region (probably yes; by the call-boundary rule it does
  not capture the caller's program env).
- Mutable box-builder type design (deferred to v2 unless a v1 case demands it).
