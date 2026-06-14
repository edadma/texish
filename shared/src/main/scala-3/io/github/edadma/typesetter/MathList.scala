package io.github.edadma.typesetter

import scala.collection.mutable.ArrayBuffer

/** A node in a math list. Stage 2 has just two kinds: an atom (a symbol or sub-formula carrying the class
  * that drives spacing) and an explicit space the author asked for (`\,`, `\;`, …). Scripts, fractions and
  * radicals introduce further node kinds in later stages.
  */
sealed trait MathNode

/** A math atom: a nucleus box together with the [[MathClass]] that governs the space around it. */
case class MathAtom(cls: MathClass, nucleus: Box) extends MathNode

/** An explicit, author-inserted space — fixed glue that is emitted verbatim and is transparent to the
  * automatic inter-atom spacing of its neighbours. */
case class MathSpace(glue: Glue) extends MathNode

/** The front half of TeX's *mlist-to-hlist*: turn a math list into a flat horizontal list of boxes and
  * glue. Stage 2 does two jobs. First it normalizes binary operators — a `Bin` atom that cannot really be
  * binary in its context is reclassified `Ord`, so `-x` sets tight (a unary sign) while `a-x` gets medium
  * space around the operator. Then it inserts the inter-atom glue from [[MathSpacing]] between each pair of
  * consecutive atoms.
  */
object MathList:
  import MathClass.*

  /** Reclassify Bin atoms that are not flanked by operands, following Appendix G's rules. In a single
    * left-to-right scan over the atoms (explicit spaces are transparent): a `Bin` is demoted to `Ord` when
    * it begins the list or follows an Op/Bin/Rel/Open/Punct; a `Rel`, `Close` or `Punct` demotes an
    * immediately preceding `Bin`; and a `Bin` that ends the list is demoted too.
    */
  def normalizeBins(nodes: Vector[MathNode]): Vector[MathNode] =
    val cls = nodes.collect { case a: MathAtom => a.cls }.toArray

    var prev: Option[MathClass] = None // class of the previous atom, after any change to it
    var prevIndex               = -1

    for i <- cls.indices do
      cls(i) match
        case Bin if prev.forall(c => c == Bin || c == Op || c == Rel || c == Open || c == Punct) =>
          cls(i) = Ord
        case Rel | Close | Punct if prev.contains(Bin) =>
          cls(prevIndex) = Ord
        case _ =>

      prev = Some(cls(i))
      prevIndex = i

    if cls.nonEmpty && cls.last == Bin then cls(cls.length - 1) = Ord

    var k = 0
    nodes.map {
      case a: MathAtom => val c = cls(k); k += 1; MathAtom(c, a.nucleus)
      case other       => other
    }

  /** Lay out the list: normalize bins, then walk the nodes emitting each atom's nucleus preceded by the
    * automatic glue for the (previous atom, this atom) class pair. Explicit spaces are emitted in place and
    * do not break the atom adjacency the spacing is computed from. `em` is the math font's size in points.
    */
  def translate(nodes: Vector[MathNode], em: Double): Vector[Box] =
    val out                         = ArrayBuffer[Box]()
    var prevAtom: Option[MathClass] = None

    for node <- normalizeBins(nodes) do
      node match
        case MathAtom(cls, nucleus) =>
          prevAtom.foreach(p => MathSpacing.glue(MathSpacing.code(p, cls), em).foreach(out += _))
          out += nucleus
          prevAtom = Some(cls)
        case MathSpace(g) =>
          out += g

    out.toVector
