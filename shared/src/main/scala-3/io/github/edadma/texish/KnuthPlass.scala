package io.github.edadma.texish

import scala.collection.mutable.ArrayBuffer

object KnuthPlass:

  // Items in the paragraph stream
  enum Item:
    case BoxItem(box: Box, index: Int)
    case GlueItem(glue: Glue, index: Int)
    case PenaltyItem(penalty: Double, flagged: Boolean, width: Double, index: Int)
    // An author discretionary break. `noBreak` is its width-bearing inline form; breaking here ends the line
    // with `pre` and opens the next with `post` (see DiscretionaryBox).
    case DiscItem(pre: Seq[Box], post: Seq[Box], noBreak: Seq[Box], penalty: Double, index: Int)

  import Item.*

  // A potential breakpoint with its cost. `fitness` is the fitness class of the line ENDING at this break,
  // kept so the next line can be charged \adjdemerits when adjacent lines jump more than one class.
  case class Breakpoint(
      position: Int,       // index in item list
      line: Int,           // which line number
      totalDemerits: Double,
      totalWidth: Double,
      totalStretch: Double,
      totalShrink: Double,
      fitness: Int,
      previous: Breakpoint | Null,
  )

  // Fitness classes for adjacent line demerits
  private val TightFit  = 0
  private val NormalFit = 1
  private val LooseFit  = 2
  private val VeryLoose = 3

  def computeBadness(delta: Double, totalStretch: Double, totalShrink: Double): Double =
    if delta > 0 then // need to stretch
      if totalStretch > 0 then
        val r = delta / totalStretch
        math.min(10000, 100 * r * r * r)
      else 10001 // infinitely bad
    else if delta < 0 then // need to shrink
      if totalShrink > 0 && -delta <= totalShrink then
        val r = -delta / totalShrink
        math.min(10000, 100 * r * r * r)
      else 10001 // overfull
    else 0       // perfect fit

  def computeDemerits(badness: Double, penalty: Double, linepenalty: Double): Double =
    if penalty >= 0 then math.pow(linepenalty + badness, 2) + penalty * penalty
    else if penalty > -10000 then math.pow(linepenalty + badness, 2) - penalty * penalty
    else math.pow(linepenalty + badness, 2)

  def fitnessClass(r: Double): Int =
    if r < -0.5 then TightFit
    else if r < 0.5 then NormalFit
    else if r < 1.0 then LooseFit
    else VeryLoose

  // Returns lines as sequences of boxes, ready to be added to HBoxBuilder. `extraInset(n)` is any further narrowing
  // of line n beyond the margins and hanging indent — the room taken by the figures the line flows around (see
  // Cutout). It defaults to nothing, so a paragraph with no cutouts breaks exactly as before.
  def breakParagraph(
      boxes: Seq[Box],
      hsize: Double,
      t: Typesetter,
      extraInset: Int => Double = _ => 0.0,
  ): Option[Seq[Seq[Box]]] =
    if boxes.isEmpty then return Some(Seq.empty)

    val tolerance        = t.getNumber("tolerance")
    val hyphenpenalty    = t.getNumber("hyphenpenalty")
    val linepenalty      = t.getNumber("linepenalty")
    val adjdemerits      = t.getNumber("adjdemerits")
    val emergencystretch = t.getNumber("emergencystretch")
    val parfillskip      = t.getGlue("parfillskip")

    // Glue flexibility split by infinity order: finite (order 0) stretch/shrink enters the badness ratio,
    // while any fil/fill component absorbs the whole surplus or deficit — a line containing \hfil is never
    // under- or overfull, exactly as the setter will set it.
    def finiteStr(g: Glue): Double = if g.stretchOrder == 0 then g.stretch else 0.0
    def infStr(g: Glue): Double    = if g.stretchOrder > 0 then g.stretch else 0.0
    def finiteShr(g: Glue): Double = if g.shrinkOrder == 0 then g.shrink else 0.0
    def infShr(g: Glue): Double    = if g.shrinkOrder > 0 then g.shrink else 0.0

    // Per-line measure. \leftskip and \rightskip narrow every line by their natural size and lend it
    // their flexibility; \hangindent narrows the lines selected by \hangafter by |hangindent| (a
    // hanging indent — the basis for list items and quotations). `measure(n)` is the target width for
    // the line whose 0-based number is n; the margins' stretch/shrink join the line's own glue so the
    // breaker weighs badness against the same flexible margins the line will be set with.
    val leftskip   = t.getGlue("leftskip")
    val rightskip  = t.getGlue("rightskip")
    val hangindent = t.getNumber("hangindent")
    val hangafter  = t.getNumber("hangafter").toInt
    val marginNat    = leftskip.naturalSize + rightskip.naturalSize
    val marginStr    = finiteStr(leftskip) + finiteStr(rightskip)
    val marginStrInf = infStr(leftskip) + infStr(rightskip)
    val marginShr    = finiteShr(leftskip) + finiteShr(rightskip)
    val marginShrInf = infShr(leftskip) + infShr(rightskip)
    def hung(n: Int): Boolean = if hangafter >= 0 then n >= hangafter else n < -hangafter
    def measure(n: Int): Double =
      hsize - marginNat - (if hangindent != 0 && hung(n) then math.abs(hangindent) else 0.0) - extraInset(n)

    // Glue and penalties are discardable: after a break, everything up to the next box or discretionary
    // vanishes, so the following line never opens with an interword space or a stray \hskip.
    def isDiscardable(it: Item): Boolean = it match
      case _: GlueItem | _: PenaltyItem => true
      case _                            => false

    // Convert boxes to items, expanding hyphenation points. Trailing discardables are dropped, as TeX drops
    // a paragraph's final glue before appending \parfillskip — otherwise a break at that last glue would
    // offer a legal empty final line.
    val allItems = buildItems(boxes, hyphenpenalty, t.hyphenationLanguage)
    val items    = allItems.take(allItems.lastIndexWhere(it => !isDiscardable(it)) + 1)

    if items.isEmpty then return Some(Seq.empty)

    // Does a line's target width depend on WHICH line it is? A hanging indent (\hangindent, drop caps and
    // hanging quotations) or a cutout (text flowing around a figure) narrows particular line numbers; a plain
    // paragraph gives every line the same measure. This decides how active breakpoints are pruned below: when
    // the measure is uniform, two nodes at the same position and fitness face an identical future no matter how
    // many lines preceded them, so the line number can be dropped from the prune key — which keeps the active
    // set bounded by one line's breakpoint window rather than letting it grow with the paragraph. A line count
    // cannot exceed the item count, so that bounds the cutout scan.
    val measureVariesByLine = hangindent != 0 || (0 to items.length).exists(n => extraInset(n) != 0.0)

    // Cumulative width and flexibility up to each position. A penalty contributes NO width here: its width
    // (the hyphen) counts only when the break is actually taken there, added explicitly at the break — a
    // line merely passing through hyphenation points must not be measured wider than it renders.
    val cumWidth      = new Array[Double](items.length + 1)
    val cumStretch    = new Array[Double](items.length + 1)
    val cumShrink     = new Array[Double](items.length + 1)
    val cumStretchInf = new Array[Double](items.length + 1)
    val cumShrinkInf  = new Array[Double](items.length + 1)

    for i <- items.indices do
      cumWidth(i + 1) = cumWidth(i)
      cumStretch(i + 1) = cumStretch(i)
      cumShrink(i + 1) = cumShrink(i)
      cumStretchInf(i + 1) = cumStretchInf(i)
      cumShrinkInf(i + 1) = cumShrinkInf(i)
      items(i) match
        case BoxItem(box, _) =>
          cumWidth(i + 1) += box.width
        case GlueItem(glue, _) =>
          cumWidth(i + 1) += glue.naturalSize
          cumStretch(i + 1) += finiteStr(glue)
          cumShrink(i + 1) += finiteShr(glue)
          cumStretchInf(i + 1) += infStr(glue)
          cumShrinkInf(i + 1) += infShr(glue)
        case PenaltyItem(_, _, _, _) => ()
        case DiscItem(_, _, noBreak, _, _) =>
          // unbroken, a discretionary occupies its no-break width; breaking trades it for `pre`+`post`
          cumWidth(i + 1) += noBreak.map(_.width).sum

    // Where the line after a break at `breakPos` resumes: the first non-discardable item past the break.
    val resumeAt = new Array[Int](items.length + 1)
    resumeAt(items.length) = items.length
    for i <- items.length - 1 to 0 by -1 do
      resumeAt(i) = if isDiscardable(items(i)) then resumeAt(i + 1) else i
    def lineStart(breakPos: Int): Int =
      if breakPos < 0 then 0 else resumeAt(math.min(breakPos + 1, items.length))

    // The width a broken discretionary opens the following line with (its `post` material).
    def postWidth(breakPos: Int): Double =
      if breakPos < 0 then 0.0
      else
        items(breakPos) match
          case DiscItem(_, post, _, _, _) => post.map(_.width).sum
          case _                          => 0.0

    // Badness with infinity orders honoured: infinite stretch absorbs any deficit, infinite shrink any
    // overrun, at zero badness; only the finite components enter the ratio.
    def badnessOf(delta: Double, stretch: Double, stretchInf: Double, shrink: Double, shrinkInf: Double): Double =
      if delta > 0 && stretchInf > 0 then 0.0
      else if delta < 0 && shrinkInf > 0 then 0.0
      else computeBadness(delta, stretch, shrink)

    // The stretch/shrink ratio the fitness class is judged by — zero (a decent line) when infinite
    // flexibility absorbed the difference.
    def fitnessRatio(delta: Double, stretch: Double, stretchInf: Double, shrink: Double, shrinkInf: Double): Double =
      if delta > 0 then if stretchInf > 0 then 0.0 else if stretch > 0 then delta / stretch else 0.0
      else if delta < 0 then if shrinkInf > 0 then 0.0 else if shrink > 0 then delta / shrink else 0.0
      else 0.0

    // One Knuth-Plass pass. A line may break at index i (after the active break a) only when its badness is
    // within `effTolerance`; `emergency` is extra per-line stretchability (TeX's \emergencystretch) lent to
    // every line so a paragraph that won't justify within the normal tolerance still breaks at acceptable
    // badness instead of failing. Returns the least-demerit end breakpoint, or None if no chain of legal
    // breaks reaches the paragraph's end.
    def solve(effTolerance: Double, emergency: Double): Option[Breakpoint] =
      // Keep only the best breakpoint per active class. Fitness stays in the key so a tight-ending and a
      // loose-ending chain both survive — \adjdemerits may make either the better predecessor for the next
      // line. The line number is part of the key only when the measure varies by line (see above): there a
      // node headed for an indented line must not be conflated with one headed for a full-width line, so both
      // are kept; otherwise line is omitted and the active set stays bounded by one line's breakpoint window.
      def prune(bs: ArrayBuffer[Breakpoint]): ArrayBuffer[Breakpoint] =
        val grouped =
          if measureVariesByLine then bs.groupBy(b => (b.position, b.line, b.fitness))
          else bs.groupBy(b => (b.position, b.fitness))
        grouped.map(_._2.minBy(_.totalDemerits)).to(ArrayBuffer)

      // Active breakpoints - start with break at position -1 (before first item)
      var activeBreaks = ArrayBuffer[Breakpoint](
        Breakpoint(
          position = -1,
          line = 0,
          totalDemerits = 0,
          totalWidth = 0,
          totalStretch = 0,
          totalShrink = 0,
          fitness = NormalFit,
          previous = null,
        ),
      )

      // Process each potential break point
      for i <- items.indices do
        val item = items(i)

        // Can we break here?
        val canBreak = item match
          case GlueItem(glue, _) if !glue.nobreak =>
            // glue is a legal breakpoint only after a non-discardable item (a box or a discretionary), as
            // in TeX — so \nobreak (an inhibiting penalty) before a space really does forbid the break
            i > 0 && !isDiscardable(items(i - 1))
          case PenaltyItem(p, _, _, _) =>
            p < 10000 // Can break at penalty if not infinite
          case DiscItem(_, _, _, p, _) =>
            p < 10000 // a discretionary is a break opportunity unless its penalty forbids it
          case _ => false

        if canBreak then
          // A forcing penalty (\penalty-10000, \break) must be taken: it accepts any badness, and once
          // processed every chain that would run a line past it is dropped.
          val forced = item match
            case PenaltyItem(p, _, _, _) => p <= -10000
            case DiscItem(_, _, _, p, _) => p <= -10000
            case _                       => false

          val newBreaks = ArrayBuffer[Breakpoint]()

          // Active nodes whose line to this breakpoint is already overfull are retired here (TeX's node
          // deactivation): the surviving nodes are collected as we go and become the new active set. An
          // overfull line only grows longer at every later breakpoint — cumulative width is monotonic — so
          // such a node can never start a feasible line again, and keeping it would let the active list grow
          // without bound. Omitting this step makes a long paragraph with many hyphenation breakpoints (e.g.
          // a whole Bible chapter set as one paragraph, densely hyphenated) explode into a near-hang; with it
          // the breaker stays close to linear. A node retired here contributes no break at i either: an
          // overfull line's badness exceeds any tolerance, so it never entered `newBreaks` in the first place.
          val survivors      = ArrayBuffer[Breakpoint]()
          var anyDeactivated = false

          for a <- activeBreaks do
            // Compute line width from a to i; the line resumes after the previous break's discardables and
            // opens with a broken discretionary's `post` material
            val startPos = lineStart(a.position)
            val lineWidth = postWidth(a.position) + (item match
              case GlueItem(_, _) => cumWidth(i) - cumWidth(startPos)
              case PenaltyItem(_, _, w, _) =>
                cumWidth(i) - cumWidth(startPos) + w
              case DiscItem(pre, _, _, _, _) =>
                // breaking here ends the line with `pre` instead of the disc's inline material
                cumWidth(i) - cumWidth(startPos) + pre.map(_.width).sum
              case _ => cumWidth(i + 1) - cumWidth(startPos))

            val lineStretch    = cumStretch(i) - cumStretch(startPos) + marginStr + emergency
            val lineStretchInf = cumStretchInf(i) - cumStretchInf(startPos) + marginStrInf
            val lineShrink     = cumShrink(i) - cumShrink(startPos) + marginShr
            val lineShrinkInf  = cumShrinkInf(i) - cumShrinkInf(startPos) + marginShrInf

            val delta   = measure(a.line) - lineWidth
            val badness = badnessOf(delta, lineStretch, lineStretchInf, lineShrink, lineShrinkInf)

            // Overfull: too long even at full finite shrink, with no infinite shrink to absorb it. Retire the
            // node rather than carrying it forward. (A forced break rebuilds the active set from newBreaks
            // below, so survivor bookkeeping there is moot.)
            val overfull = delta < 0 && lineShrinkInf == 0 && -delta > lineShrink
            if overfull then anyDeactivated = true
            else survivors += a

            if badness <= effTolerance || forced then
              val penalty = item match
                case PenaltyItem(p, _, _, _) => p
                case DiscItem(_, _, _, p, _) => p
                case _                       => 0.0

              val fitness  = fitnessClass(fitnessRatio(delta, lineStretch, lineStretchInf, lineShrink, lineShrinkInf))
              var demerits = computeDemerits(badness, penalty, linepenalty)
              if math.abs(fitness - a.fitness) > 1 then demerits += adjdemerits

              newBreaks += Breakpoint(
                position = i,
                line = a.line + 1,
                totalDemerits = a.totalDemerits + demerits,
                totalWidth = cumWidth(i + 1),
                totalStretch = cumStretch(i + 1),
                totalShrink = cumShrink(i + 1),
                fitness = fitness,
                previous = a,
              )

          // Rebuild the active set from the survivors plus the new breaks, pruning dominated ones; a forced
          // break replaces the actives outright. When nothing was deactivated and no break was added, the
          // active set is unchanged and left as is.
          if forced then activeBreaks = prune(newBreaks)
          else if anyDeactivated || newBreaks.nonEmpty then activeBreaks = prune(survivors ++ newBreaks)

      // Find the best final breakpoint: a virtual break at the end. The last line carries \parfillskip —
      // normally infinitely stretchable, so a short final line (even a single word) is perfectly fine,
      // exactly as the line will be set.
      val endBreaks = ArrayBuffer[Breakpoint]()
      for a <- activeBreaks do
        val startPos       = lineStart(a.position)
        val lineWidth      = cumWidth(items.length) - cumWidth(startPos) + postWidth(a.position) + parfillskip.naturalSize
        val lineStretch    = cumStretch(items.length) - cumStretch(startPos) + marginStr + emergency + finiteStr(parfillskip)
        val lineStretchInf = cumStretchInf(items.length) - cumStretchInf(startPos) + marginStrInf + infStr(parfillskip)
        val lineShrink     = cumShrink(items.length) - cumShrink(startPos) + marginShr + finiteShr(parfillskip)
        val lineShrinkInf  = cumShrinkInf(items.length) - cumShrinkInf(startPos) + marginShrInf + infShr(parfillskip)
        val delta          = measure(a.line) - lineWidth
        val badness        = badnessOf(delta, lineStretch, lineStretchInf, lineShrink, lineShrinkInf)

        // Last line is more tolerant
        if badness <= 10000 then
          val fitness  = fitnessClass(fitnessRatio(delta, lineStretch, lineStretchInf, lineShrink, lineShrinkInf))
          var demerits = computeDemerits(math.min(badness, effTolerance), 0, linepenalty)
          if math.abs(fitness - a.fitness) > 1 then demerits += adjdemerits
          endBreaks += Breakpoint(
            position = items.length,
            line = a.line + 1,
            totalDemerits = a.totalDemerits + demerits,
            totalWidth = cumWidth(items.length),
            totalStretch = cumStretch(items.length),
            totalShrink = cumShrink(items.length),
            fitness = fitness,
            previous = a,
          )

      if endBreaks.isEmpty then None else Some(endBreaks.minBy(_.totalDemerits))

    // First pass at the normal tolerance. If a paragraph can't be justified within it and the document has
    // asked for \emergencystretch, retry at the SAME tolerance but with that extra per-line give, the way
    // TeX's final pass does — just enough slack to bring otherwise-too-loose lines within tolerance, without
    // accepting the ugly badness a blanket tolerance bump would. With the default \emergencystretch of 0 no
    // second pass runs, so the result (and the greedy fallback the caller uses on None) is unchanged.
    val solution =
      solve(tolerance, 0.0).orElse(if emergencystretch > 0 then solve(tolerance, emergencystretch) else None)

    // Trace back to get break positions in item indices
    var best = solution match
      case Some(b) => b
      case None    => return None
    val breakPositions = ArrayBuffer[Int]()

    while best.previous != null do
      breakPositions.prepend(best.position)
      best = best.previous

    // Now build lines from items using break positions. A discretionary broken at the end of one line opens
    // the next with its `post` material; `pendingPost` carries that across the line boundary.
    val lines      = ArrayBuffer[Seq[Box]]()
    var startIdx   = 0
    var pendingPost: Seq[Box] = Seq.empty

    // Emit an item that falls strictly inside a line (never a break point): a discretionary shows its
    // unbroken form here, a penalty contributes nothing.
    def emitInterior(item: Item, lineBoxes: ArrayBuffer[Box]): Unit =
      item match
        case BoxItem(box, _)              => lineBoxes += box
        case GlueItem(glue, _)            => lineBoxes += glue
        case DiscItem(_, _, noBreak, _, _) => lineBoxes ++= noBreak
        case PenaltyItem(_, _, _, _)      => ()

    for breakPos <- breakPositions do
      val lineBoxes = ArrayBuffer[Box]()
      lineBoxes ++= pendingPost
      pendingPost = Seq.empty

      for item <- items.slice(startIdx, breakPos) do emitInterior(item, lineBoxes)

      // Resolve the break item itself
      if breakPos < items.length then
        items(breakPos) match
          case PenaltyItem(_, true, _, idx) =>
            // Breaking at a flagged (hyphenation) penalty: end the line with a hyphen in the run's font
            lineBoxes.lastOption match
              case Some(cb: CharBox) => lineBoxes += cb.newCharBox("-")
              case _ => // can't add hyphen without font info
          case DiscItem(pre, post, _, _, _) =>
            // Breaking at a discretionary: `pre` ends this line, `post` opens the next
            lineBoxes ++= pre
            pendingPost = post
          case GlueItem(_, _) =>
            // Breaking at glue - remove trailing glue from line
            if lineBoxes.nonEmpty && lineBoxes.last.isSpace then
              lineBoxes.remove(lineBoxes.length - 1)
          case _ =>

      lines += lineBoxes.toSeq
      // Skip the break item and the discardables after it (glue, penalties) — the same items the solver's
      // lineStart excluded from the next line's measure
      startIdx = lineStart(breakPos)

    // Handle the last line (from last break to end)
    if startIdx < items.length || pendingPost.nonEmpty then
      val lineBoxes = ArrayBuffer[Box]()
      lineBoxes ++= pendingPost
      for item <- items.slice(startIdx, items.length) do emitInterior(item, lineBoxes)
      lines += lineBoxes.toSeq

    Some(lines.toSeq)

  private def buildItems(boxes: Seq[Box], hyphenpenalty: Double, hyphLang: Option[String]): Seq[Item] =
    val items = ArrayBuffer[Item]()

    for (box, idx) <- boxes.zipWithIndex do
      box match
        case g: Glue =>
          items += GlueItem(g, idx)
        case p: Penalty =>
          // an explicit \penalty / \nobreak / \break marker: a break control, not content
          items += PenaltyItem(p.penalty.toDouble, false, 0, idx)
        case d: DiscretionaryBox =>
          items += DiscItem(d.pre, d.post, d.noBreak, hyphenpenalty, idx)
        case cb: CharBox if CJK.hasCJK(cb.text) =>
          // A run containing CJK has no interword spaces, so break it between characters instead of
          // hyphenating it (Latin words inside the run keep their own spelling, just unbroken).
          appendCJKItems(items, cb, idx)
        case cb: CharBox =>
          // Check for hyphenation opportunities, in the document's active language
          Hyphenation(hyphLang, cb.text) match
            case Some(hyphenation) =>
              val hyphenPoints = hyphenation.toList
              if hyphenPoints.nonEmpty then
                // Build segments with penalties between them
                // Note: 'before' includes the hyphen (e.g., "com-"), so actual break pos is before.length - 1
                var pos = 0
                for ((before, after), i) <- hyphenPoints.zipWithIndex do
                  val breakPos = before.length - 1  // Position in original word (without hyphen)
                  // Add the segment from pos to breakPos
                  val segmentText = cb.text.substring(pos, breakPos)
                  if segmentText.nonEmpty then
                    items += BoxItem(cb.newCharBox(segmentText), idx)
                  // Add penalty (hyphen added if we break here)
                  val hyphenWidth = cb.newCharBox("-").width
                  items += PenaltyItem(hyphenpenalty, true, hyphenWidth, idx)
                  pos = breakPos
                // Add remaining text after last hyphen point
                if pos < cb.text.length then
                  items += BoxItem(cb.newCharBox(cb.text.substring(pos)), idx)
              else
                items += BoxItem(cb, idx)
            case None =>
              items += BoxItem(cb, idx)
        case _ =>
          items += BoxItem(box, idx)

    items.toSeq

  // Expand a text run containing CJK into per-segment boxes joined by breakable, stretchable glue. A break
  // is offered between two characters when CJK.breakableBetween allows it; characters kinsoku keeps together
  // (a closing mark and what precedes it, an opening mark and what follows) stay in one box. The inserted
  // glue has zero natural width — CJK is set solid, with no visible inter-character space — but carries a
  // little stretch and shrink so a line can reach the measure; without any stretch every short CJK line
  // would be infinitely bad and the paragraph would not break. The amounts are deliberately small (a sixth
  // of a character of stretch, a twentieth of shrink) so the breaker is rewarded for packing each line
  // nearly full — the even, solid grid CJK wants — rather than spreading characters loosely; the last line
  // is left ragged by \parfillskip as usual.
  private def appendCJKItems(items: ArrayBuffer[Item], cb: CharBox, idx: Int): Unit =
    val text = cb.text

    // Walk by codepoint so a surrogate pair (an Extension-B ideograph) is never split, keeping each
    // character's substring alongside its codepoint for reassembly and for the kinsoku test.
    val pieces = ArrayBuffer[String]()
    val codes  = ArrayBuffer[Int]()
    var i      = 0
    while i < text.length do
      val cp = text.codePointAt(i)
      val n  = Character.charCount(cp)
      pieces += text.substring(i, i + n)
      codes += cp
      i += n

    val seg = new StringBuilder
    def flush(): Unit =
      if seg.nonEmpty then
        items += BoxItem(cb.newCharBox(seg.toString), idx)
        seg.setLength(0)

    for j <- codes.indices do
      seg ++= pieces(j)
      if j < codes.length - 1 && CJK.breakableBetween(codes(j), codes(j + 1)) then
        val charWidth = cb.newCharBox(pieces(j)).width
        flush()
        items += GlueItem(Glue(0, charWidth / 6.0, charWidth / 20.0), idx)
    flush()
