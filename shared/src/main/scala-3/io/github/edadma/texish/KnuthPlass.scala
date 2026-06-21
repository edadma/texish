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

  // A potential breakpoint with its cost
  case class Breakpoint(
      position: Int,       // index in item list
      line: Int,           // which line number
      totalDemerits: Double,
      totalWidth: Double,
      totalStretch: Double,
      totalShrink: Double,
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

  // Returns lines as sequences of boxes, ready to be added to HBoxBuilder
  def breakParagraph(boxes: Seq[Box], hsize: Double, t: Typesetter): Option[Seq[Seq[Box]]] =
    if boxes.isEmpty then return Some(Seq.empty)

    val tolerance        = t.getNumber("tolerance")
    val hyphenpenalty    = t.getNumber("hyphenpenalty")
    val linepenalty      = t.getNumber("linepenalty")
    val adjdemerits      = t.getNumber("adjdemerits")
    val emergencystretch = t.getNumber("emergencystretch")

    // Per-line measure. \leftskip and \rightskip narrow every line by their natural size and lend it
    // their flexibility; \hangindent narrows the lines selected by \hangafter by |hangindent| (a
    // hanging indent — the basis for list items and quotations). `measure(n)` is the target width for
    // the line whose 0-based number is n; the margins' stretch/shrink join the line's own glue so the
    // breaker weighs badness against the same flexible margins the line will be set with.
    val leftskip   = t.getGlue("leftskip")
    val rightskip  = t.getGlue("rightskip")
    val hangindent = t.getNumber("hangindent")
    val hangafter  = t.getNumber("hangafter").toInt
    val marginNat  = leftskip.naturalSize + rightskip.naturalSize
    val marginStr  = leftskip.stretch + rightskip.stretch
    val marginShr  = leftskip.shrink + rightskip.shrink
    def hung(n: Int): Boolean = if hangafter >= 0 then n >= hangafter else n < -hangafter
    def measure(n: Int): Double =
      hsize - marginNat - (if hangindent != 0 && hung(n) then math.abs(hangindent) else 0.0)

    // Convert boxes to items, expanding hyphenation points
    val items = buildItems(boxes, hyphenpenalty)

    if items.isEmpty then return Some(Seq.empty)

    // Cumulative width/stretch/shrink up to each position
    val cumWidth   = new Array[Double](items.length + 1)
    val cumStretch = new Array[Double](items.length + 1)
    val cumShrink  = new Array[Double](items.length + 1)

    cumWidth(0) = 0
    cumStretch(0) = 0
    cumShrink(0) = 0

    for i <- items.indices do
      items(i) match
        case BoxItem(box, _) =>
          cumWidth(i + 1) = cumWidth(i) + box.width
          cumStretch(i + 1) = cumStretch(i)
          cumShrink(i + 1) = cumShrink(i)
        case GlueItem(glue, _) =>
          cumWidth(i + 1) = cumWidth(i) + glue.naturalSize
          cumStretch(i + 1) = cumStretch(i) + glue.stretch
          cumShrink(i + 1) = cumShrink(i) + glue.shrink
        case PenaltyItem(_, _, w, _) =>
          cumWidth(i + 1) = cumWidth(i) + w
          cumStretch(i + 1) = cumStretch(i)
          cumShrink(i + 1) = cumShrink(i)
        case DiscItem(_, _, noBreak, _, _) =>
          // unbroken, a discretionary occupies its no-break width; breaking trades it for `pre`+`post`
          cumWidth(i + 1) = cumWidth(i) + noBreak.map(_.width).sum
          cumStretch(i + 1) = cumStretch(i)
          cumShrink(i + 1) = cumShrink(i)

    // One Knuth-Plass pass. A line may break at index i (after the active break a) only when its badness is
    // within `effTolerance`; `emergency` is extra per-line stretchability (TeX's \emergencystretch) lent to
    // every line so a paragraph that won't justify within the normal tolerance still breaks at acceptable
    // badness instead of failing. Returns the least-demerit end breakpoint, or None if no chain of legal
    // breaks reaches the paragraph's end.
    def solve(effTolerance: Double, emergency: Double): Option[Breakpoint] =
      // Active breakpoints - start with break at position -1 (before first item)
      var activeBreaks = ArrayBuffer[Breakpoint](
        Breakpoint(
          position = -1,
          line = 0,
          totalDemerits = 0,
          totalWidth = 0,
          totalStretch = 0,
          totalShrink = 0,
          previous = null,
        ),
      )

      // Process each potential break point
      for i <- items.indices do
        val item = items(i)

        // Can we break here?
        val canBreak = item match
          case GlueItem(glue, _) if !glue.nobreak =>
            // Can break before glue if preceded by non-glue
            i > 0 && !items(i - 1).isInstanceOf[GlueItem]
          case PenaltyItem(p, _, _, _) =>
            p < 10000 // Can break at penalty if not infinite
          case DiscItem(_, _, _, p, _) =>
            p < 10000 // a discretionary is a break opportunity unless its penalty forbids it
          case _ => false

        if canBreak then
          val newBreaks = ArrayBuffer[Breakpoint]()

          for a <- activeBreaks do
            // Compute line width from a to i
            val startPos = a.position + 1
            val lineWidth = item match
              case GlueItem(_, _) => cumWidth(i) - cumWidth(math.max(0, startPos))
              case PenaltyItem(_, _, w, _) =>
                cumWidth(i) - cumWidth(math.max(0, startPos)) + w
              case DiscItem(pre, _, _, _, _) =>
                // breaking here ends the line with `pre` instead of the disc's inline material
                cumWidth(i) - cumWidth(math.max(0, startPos)) + pre.map(_.width).sum
              case _ => cumWidth(i + 1) - cumWidth(math.max(0, startPos))

            val lineStretch = cumStretch(i) - cumStretch(math.max(0, startPos)) + marginStr + emergency
            val lineShrink  = cumShrink(i) - cumShrink(math.max(0, startPos)) + marginShr

            val delta   = measure(a.line) - lineWidth
            val badness = computeBadness(delta, lineStretch, lineShrink)

            if badness <= effTolerance then
              val penalty = item match
                case PenaltyItem(p, _, _, _) => p
                case DiscItem(_, _, _, p, _) => p
                case _                       => 0.0

              val demerits = computeDemerits(badness, penalty, linepenalty)

              // Compute fitness class for adjacent line penalty
              val r = if delta > 0 && lineStretch > 0 then delta / lineStretch
              else if delta < 0 && lineShrink > 0 then delta / lineShrink
              else 0.0
              val fitness = fitnessClass(r)

              val totalDem = a.totalDemerits + demerits

              newBreaks += Breakpoint(
                position = i,
                line = a.line + 1,
                totalDemerits = totalDem,
                totalWidth = cumWidth(i + 1),
                totalStretch = cumStretch(i + 1),
                totalShrink = cumShrink(i + 1),
                previous = a,
              )

          // Add new breaks and prune dominated ones
          if newBreaks.nonEmpty then
            activeBreaks ++= newBreaks
            // Keep only the best breakpoint for each (position, line) combination
            activeBreaks = activeBreaks
              .groupBy(b => (b.position, b.line))
              .map(_._2.minBy(_.totalDemerits))
              .to(ArrayBuffer)

      // Find the best final breakpoint
      // Add a virtual break at the end
      val endBreaks = ArrayBuffer[Breakpoint]()
      for a <- activeBreaks do
        val startPos    = a.position + 1
        val lineWidth   = cumWidth(items.length) - cumWidth(math.max(0, startPos))
        val lineStretch = cumStretch(items.length) - cumStretch(math.max(0, startPos)) + marginStr + emergency
        val lineShrink  = cumShrink(items.length) - cumShrink(math.max(0, startPos)) + marginShr
        val delta       = measure(a.line) - lineWidth
        val badness     = computeBadness(delta, lineStretch, lineShrink)

        // Last line is more tolerant (uses parfillskip)
        if badness <= 10000 then
          val demerits = computeDemerits(math.min(badness, effTolerance), 0, linepenalty)
          endBreaks += Breakpoint(
            position = items.length,
            line = a.line + 1,
            totalDemerits = a.totalDemerits + demerits,
            totalWidth = cumWidth(items.length),
            totalStretch = cumStretch(items.length),
            totalShrink = cumShrink(items.length),
            previous = a,
          )

      if endBreaks.isEmpty then None else Some(endBreaks.minBy(_.totalDemerits))

    // First pass at the normal tolerance. If a paragraph can't be justified within it, retry with a relaxed
    // tolerance plus \emergencystretch, distributing the unavoidable looseness evenly the way TeX does rather
    // than dropping to the greedy fallback. Only a genuinely unbreakable paragraph (e.g. a single box wider
    // than the measure) still yields None, leaving the greedy fallback as a true last resort.
    val solution = solve(tolerance, 0.0).orElse(solve(10000.0, emergencystretch))

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
      startIdx = breakPos + 1  // Skip the break item (glue, penalty, or discretionary)

    // Handle the last line (from last break to end)
    if startIdx < items.length || pendingPost.nonEmpty then
      val lineBoxes = ArrayBuffer[Box]()
      lineBoxes ++= pendingPost
      for item <- items.slice(startIdx, items.length) do emitInterior(item, lineBoxes)
      lines += lineBoxes.toSeq

    Some(lines.toSeq)

  private def buildItems(boxes: Seq[Box], hyphenpenalty: Double): Seq[Item] =
    val items = ArrayBuffer[Item]()

    for (box, idx) <- boxes.zipWithIndex do
      box match
        case g: Glue =>
          items += GlueItem(g, idx)
        case d: DiscretionaryBox =>
          items += DiscItem(d.pre, d.post, d.noBreak, hyphenpenalty, idx)
        case cb: CharBox =>
          // Check for hyphenation opportunities
          Hyphenation(cb.text) match
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
