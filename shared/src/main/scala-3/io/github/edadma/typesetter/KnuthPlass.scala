package io.github.edadma.typesetter

import scala.collection.mutable.ArrayBuffer

object KnuthPlass:

  // Items in the paragraph stream
  enum Item:
    case BoxItem(box: Box, index: Int)
    case GlueItem(glue: Glue, index: Int)
    case PenaltyItem(penalty: Double, flagged: Boolean, width: Double, index: Int)

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

    val tolerance     = t.getNumber("tolerance")
    val hyphenpenalty = t.getNumber("hyphenpenalty")
    val linepenalty   = t.getNumber("linepenalty")
    val adjdemerits   = t.getNumber("adjdemerits")

    // Convert boxes to items, expanding hyphenation points
    val items = buildItems(boxes, hyphenpenalty)

    if items.isEmpty then return Some(Seq.empty)

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
            case _ => cumWidth(i + 1) - cumWidth(math.max(0, startPos))

          val lineStretch = cumStretch(i) - cumStretch(math.max(0, startPos))
          val lineShrink  = cumShrink(i) - cumShrink(math.max(0, startPos))

          val delta   = hsize - lineWidth
          val badness = computeBadness(delta, lineStretch, lineShrink)

          if badness <= tolerance then
            val penalty = item match
              case PenaltyItem(p, _, _, _) => p
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
      val lineStretch = cumStretch(items.length) - cumStretch(math.max(0, startPos))
      val lineShrink  = cumShrink(items.length) - cumShrink(math.max(0, startPos))
      val delta       = hsize - lineWidth
      val badness     = computeBadness(delta, lineStretch, lineShrink)

      // Last line is more tolerant (uses parfillskip)
      if badness <= 10000 then
        val demerits = computeDemerits(math.min(badness, tolerance), 0, linepenalty)
        endBreaks += Breakpoint(
          position = items.length,
          line = a.line + 1,
          totalDemerits = a.totalDemerits + demerits,
          totalWidth = cumWidth(items.length),
          totalStretch = cumStretch(items.length),
          totalShrink = cumShrink(items.length),
          previous = a,
        )

    if endBreaks.isEmpty then
      // Fallback: no valid breaks found
      return None

    // Trace back to get break positions in item indices
    var best       = endBreaks.minBy(_.totalDemerits)
    val breakPositions = ArrayBuffer[Int]()

    while best.previous != null do
      breakPositions.prepend(best.position)
      best = best.previous

    // Now build lines from items using break positions
    val lines      = ArrayBuffer[Seq[Box]]()
    var startIdx   = 0

    for breakPos <- breakPositions do
      val lineItems = items.slice(startIdx, breakPos)
      val lineBoxes = ArrayBuffer[Box]()

      for item <- lineItems do
        item match
          case BoxItem(box, _) => lineBoxes += box
          case GlueItem(glue, _) => lineBoxes += glue
          case PenaltyItem(_, flagged, width, _) =>
            // If breaking at a penalty and it's flagged (hyphenation), add the hyphen
            // But only if this is the break point
            ()

      // Check if we broke at a hyphenation point
      if breakPos < items.length then
        items(breakPos) match
          case PenaltyItem(_, true, _, idx) =>
            // Breaking at hyphenation - add hyphen to end of line
            // Find the last CharBox to get font info
            lineBoxes.lastOption match
              case Some(cb: CharBox) => lineBoxes += cb.newCharBox("-")
              case _ => // can't add hyphen without font info
          case GlueItem(_, _) =>
            // Breaking at glue - remove trailing glue from line
            if lineBoxes.nonEmpty && lineBoxes.last.isSpace then
              lineBoxes.remove(lineBoxes.length - 1)
          case _ =>

      lines += lineBoxes.toSeq
      startIdx = breakPos + 1  // Skip the break item (glue or penalty)

    // Handle the last line (from last break to end)
    if startIdx < items.length then
      val lineItems = items.slice(startIdx, items.length)
      val lineBoxes = ArrayBuffer[Box]()

      for item <- lineItems do
        item match
          case BoxItem(box, _) => lineBoxes += box
          case GlueItem(glue, _) => lineBoxes += glue
          case PenaltyItem(_, _, _, _) => () // Don't add penalties to final line

      lines += lineBoxes.toSeq

    Some(lines.toSeq)

  private def buildItems(boxes: Seq[Box], hyphenpenalty: Double): Seq[Item] =
    val items = ArrayBuffer[Item]()

    for (box, idx) <- boxes.zipWithIndex do
      box match
        case g: Glue =>
          items += GlueItem(g, idx)
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
