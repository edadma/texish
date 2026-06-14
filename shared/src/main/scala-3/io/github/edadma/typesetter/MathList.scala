package io.github.edadma.typesetter

import scala.collection.mutable.ArrayBuffer

/** A node in a math list. Stage 2 has just two kinds: an atom (a symbol or sub-formula carrying the class
  * that drives spacing) and an explicit space the author asked for (`\,`, `\;`, …). Scripts, fractions and
  * radicals introduce further node kinds in later stages.
  */
sealed trait MathNode

/** A math atom: a nucleus box together with the [[MathClass]] that governs the space around it, and the
  * optional super- and subscript boxes attached to it. The scripts are already laid out at the script size
  * when they are attached; [[MathList.translate]] positions them around the nucleus. `italicCorrection` is
  * the nucleus's, used to set the superscript out past a slanted nucleus. */
case class MathAtom(
    cls: MathClass,
    nucleus: Box,
    sup: Option[Box] = None,
    sub: Option[Box] = None,
    italicCorrection: Double = 0.0,
) extends MathNode

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
      case a: MathAtom => val c = cls(k); k += 1; a.copy(cls = c)
      case other       => other
    }

  /** Lay out the list: normalize bins, then walk the nodes emitting each atom's nucleus — with its scripts
    * attached when it has any — preceded by the automatic glue for the (previous atom, this atom) class pair.
    * Explicit spaces are emitted in place and do not break the atom adjacency the spacing is computed from.
    * `mf` is the font this list is set in (its size drives the spacing, its constants the script placement);
    * `cramped` is whether the list's style is cramped, which lowers superscripts.
    */
  def translate(nodes: Vector[MathNode], mf: MathFont, cramped: Boolean): Vector[Box] =
    val em                          = mf.size
    val out                         = ArrayBuffer[Box]()
    var prevAtom: Option[MathClass] = None

    for node <- normalizeBins(nodes) do
      node match
        case a: MathAtom =>
          prevAtom.foreach(p => MathSpacing.glue(MathSpacing.code(p, a.cls), em).foreach(out += _))
          out += nucleusBox(a, mf, cramped)
          prevAtom = Some(a.cls)
        case MathSpace(g) =>
          out += g

    out.toVector

  /** An atom's nucleus, with its super/subscript attached if it has any. A script-less atom is just its
    * nucleus; otherwise the scripts are shifted into place by [[MathScriptBox]] using the nucleus font's
    * script parameters, with the nucleus's italic correction applied horizontally per [[scriptOffsets]]. */
  private def nucleusBox(a: MathAtom, mf: MathFont, cramped: Boolean): Box =
    if a.sup.isEmpty && a.sub.isEmpty then a.nucleus
    else
      val p              = mf.scriptParams
      val (up, down)     = MathScriptBox.shifts(a.nucleus, a.sup, a.sub, p, cramped)
      val (supDx, subDx) = scriptOffsets(a.cls, a.italicCorrection)

      new MathScriptBox(a.nucleus, a.sup, a.sub, up, down, supDx, subDx, p.spaceAfterScript)

  /** How a nucleus's italic correction offsets its scripts horizontally, returned as (superscript, subscript)
    * shifts from the nucleus's right edge. For an ordinary atom the correction sets the superscript out to
    * the right, clearing a slanted letter whose ink overhangs its advance. For a large operator it is a limit
    * skew instead: the subscript tucks left under the slant while the superscript stays at the right edge —
    * the characteristic placement of integral limits. (The font's per-corner kern cut-ins would refine both;
    * they arrive with stretchy operators.) */
  private def scriptOffsets(cls: MathClass, italicCorrection: Double): (Double, Double) =
    if cls == MathClass.Op then (0.0, -italicCorrection)
    else (italicCorrection, 0.0)
