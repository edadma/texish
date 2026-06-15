package io.github.edadma.texish.parser

/** The bundled package registry: standard libraries that ship with texish but are **not** loaded unless a
  * document asks for them with `\use{name}`. Unlike the always-on [[io.github.edadma.texish.standardPrelude]],
  * a bundled package costs nothing until used — a math paper never pays for the chemistry vocabulary.
  *
  * Each package is its own document-language source (macros over the engine primitives), embedded as a string so
  * it loads identically on every platform, including Scala.js where there is no filesystem. User-written package
  * files on disk are a separate, later concern; this is the built-in set.
  */
object Packages:

  /** The source of a bundled package by name, or `None` if there is no such package. */
  def source(name: String): Option[String] = registry.get(name)

  /** Whether a bundled package of this name exists. */
  def exists(name: String): Boolean = registry.contains(name)

  private val registry: Map[String, String] = Map(
    "chem"     -> chem,
    "counters" -> counters,
    "floats"   -> floats,
  )

  /** `floats` — LaTeX-style `\figure`/`\table` floats with auto-numbered `\caption`s, layered over the
    * `\topinsert` primitive (top-of-page placement) and the [[counters]] package. `\figure{…}` and `\table{…}`
    * detach their body to the top of whatever page they land on; a `\caption{…}` written inside one steps that
    * float's own counter (figure or table, kept independent) and typesets a numbered line such as `Figure 1: …`.
    * The kind and counter name are passed to the shared `\caption` through two scratch variables the float sets
    * before delegating to `\topinsert`, so the caption inherits them when it runs inside the floated block. */
  private def floats: String =
    raw"""// floats — figure and table floats with numbered captions, over the \topinsert primitive and the counters
// package. Each source line ends with // so its newline is stripped and no stray whitespace reaches the output.
\use{counters}//
\newcounter{figure}//
\newcounter{table}//
// \figure{body} — float body to the top of its page; a \caption inside numbers as a figure.
\def figure body {{//
\set captionkind {Figure}//
\set captioncounter {figure}//
\topinsert{\body}//
}}//
// \table{body} — like \figure, but a \caption inside numbers as a table (an independent counter).
\def table body {{//
\set captionkind {Table}//
\set captioncounter {table}//
\topinsert{\body}//
}}//
// \caption{text} — step the enclosing float's counter and typeset a numbered caption line, e.g. "Figure 1: text".
// Reads captionkind/captioncounter, which the surrounding \figure or \table set before entering the float.
\def caption text {{//
\stepcounter \captioncounter//
\the\captionkind \arabic{\value \captioncounter}: \text//
}}//
"""

  /** `counters` — LaTeX-style document counters, written entirely in the document language over the engine's
    * global keyed store (`\global\mapset`/`\mapget`), `\calc`, and the number-formatting primitives. Two global
    * maps hold all state: `counterStore` (name → value) and `counterParent` (child → parent), so counters survive
    * groups. Reset-lists work the LaTeX way: stepping a counter zeroes every counter declared `\counterwithin` it,
    * recursively. Display a counter by formatting its value, e.g. `\arabic{\value section}` or `\roman{\value
    * page}`. Each macro body is wrapped in an explicit group so its scratch variables stay local while the
    * `\global` writes escape. */
  private def counters: String =
    raw"""// counters — every source line ends with // so its newline is stripped at tokenise time and the package
// emits no stray whitespace when used in running text.
//
// \newcounter{name} — create a counter, initialised to 0.
\def newcounter n {{//
\global\mapset counterStore {\n} {0}//
}}//
// \setcounter{name}{value} — set a counter to an explicit value.
\def setcounter n v {{//
\global\mapset counterStore {\n} {\v}//
}}//
// \addtocounter{name}{amount} — add amount (may be negative) to a counter.
\def addtocounter n amt {{//
\set counterCur {\mapget counterStore {\n}}//
\global\mapset counterStore {\n} {\calc{counterCur + \amt}}//
}}//
// \value{name} — the counter's number, for use in expressions and the formatters (\arabic{\value foo}).
// No scratch group here: this macro must yield a value, so its body is a bare expression.
\def value n {\mapget counterStore {\n}}//
// \counterwithin{child}{parent} — register that stepping parent resets child to 0 (the reset-list relation).
\def counterwithin child parent {{//
\global\mapset counterParent {\child} {\parent}//
}}//
// \stepcounter{name} — increment a counter by 1 and reset every counter declared within it, recursively.
\def stepcounter n {{//
\addtocounter {\n} {1}//
\resetwithin {\n}//
}}//
// \resetwithin{parent} — zero every counter whose parent is `parent`, then recurse into each so grandchildren
// reset too. Iterates the whole counterParent map and matches by value, so no per-parent child list is needed.
\def resetwithin parent {{//
\for\counterLink {\counterParent} {//
\if {\= {\counterLink.value} {\parent}}//
\set resetChild {\counterLink.key}//
\setcounter \resetChild {0}//
\resetwithin \resetChild//
\fi//
}//
}}//
"""

  /** `chem` — drawing chemical structures. Bonds between coordinates (single, double), shortened at each end so
    * atom labels have room, and a helper to name-and-label an atom. Written entirely in the document language:
    * the bond geometry is computed with `\xof`/`\yof` and `\calc`, demonstrating that a package needs no engine
    * code. Tunable through `bondgap` (the clearance at each end) and `doublesep` (half the gap between the two
    * lines of a double bond). All bond commands draw inside a `\picture`. */
  private def chem: String =
    raw"""
\set bondgap {9}
\set doublesep {2.5}

// \bond{A}{B} — a single bond between two coordinates, shortened by bondgap at each end.
\def bond a b {
  \set chemAx {\xof{\a}} \set chemAy {\yof{\a}}
  \set chemBx {\xof{\b}} \set chemBy {\yof{\b}}
  \set chemLen {\calc{hypot(chemBx - chemAx, chemBy - chemAy)}}
  \set chemUx {\calc{(chemBx - chemAx) / chemLen}}
  \set chemUy {\calc{(chemBy - chemAy) / chemLen}}
  \line{(chemAx + chemUx*bondgap, chemAy + chemUy*bondgap)
        (chemBx - chemUx*bondgap, chemBy - chemUy*bondgap)}
}

// \dbond{A}{B} — a double bond: two parallel lines, offset doublesep either side of the bond axis.
\def dbond a b {
  \set chemAx {\xof{\a}} \set chemAy {\yof{\a}}
  \set chemBx {\xof{\b}} \set chemBy {\yof{\b}}
  \set chemLen {\calc{hypot(chemBx - chemAx, chemBy - chemAy)}}
  \set chemUx {\calc{(chemBx - chemAx) / chemLen}}
  \set chemUy {\calc{(chemBy - chemAy) / chemLen}}
  \set chemOx {\calc{-chemUy * doublesep}}
  \set chemOy {\calc{chemUx * doublesep}}
  \line{(chemAx + chemUx*bondgap + chemOx, chemAy + chemUy*bondgap + chemOy)
        (chemBx - chemUx*bondgap + chemOx, chemBy - chemUy*bondgap + chemOy)}
  \line{(chemAx + chemUx*bondgap - chemOx, chemAy + chemUy*bondgap - chemOy)
        (chemBx - chemUx*bondgap - chemOx, chemBy - chemUy*bondgap - chemOy)}
}

// \tbond{A}{B} — a triple bond: a central line flanked by two parallel lines a little wider apart.
\def tbond a b {
  \set chemAx {\xof{\a}} \set chemAy {\yof{\a}}
  \set chemBx {\xof{\b}} \set chemBy {\yof{\b}}
  \set chemLen {\calc{hypot(chemBx - chemAx, chemBy - chemAy)}}
  \set chemUx {\calc{(chemBx - chemAx) / chemLen}}
  \set chemUy {\calc{(chemBy - chemAy) / chemLen}}
  \set chemOx {\calc{-chemUy * doublesep * 1.8}}
  \set chemOy {\calc{chemUx * doublesep * 1.8}}
  \line{(chemAx + chemUx*bondgap, chemAy + chemUy*bondgap)
        (chemBx - chemUx*bondgap, chemBy - chemUy*bondgap)}
  \line{(chemAx + chemUx*bondgap + chemOx, chemAy + chemUy*bondgap + chemOy)
        (chemBx - chemUx*bondgap + chemOx, chemBy - chemUy*bondgap + chemOy)}
  \line{(chemAx + chemUx*bondgap - chemOx, chemAy + chemUy*bondgap - chemOy)
        (chemBx - chemUx*bondgap - chemOx, chemBy - chemUy*bondgap - chemOy)}
}

// \atom{name}{coord}{label} — name a coordinate and set its element label, centred on the point.
\def atom n c l {
  \coordinate{\n}{\c}
  \at anchor:center {\c}{\l}
}
"""
