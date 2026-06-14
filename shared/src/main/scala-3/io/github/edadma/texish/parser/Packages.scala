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
    "chem" -> chem,
  )

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
