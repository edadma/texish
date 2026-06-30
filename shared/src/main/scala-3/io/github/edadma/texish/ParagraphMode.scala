package io.github.edadma.texish

import io.github.edadma.texish.opentype.{ArabicShaping, JoiningForm}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class ParagraphMode(val t: Typesetter) extends HorizontalMode:
  def result: Box = ???

  /** Whether the paragraph carries any real content yet, as opposed to nothing or only the first-line indent
    * box (both of which are space). A space added while this is false is a leading space — discarded, so a
    * paragraph opened by \noindent / \indent and followed by a space starts flush rather than one space in. */
  def hasContent: Boolean = boxes.exists(!_.isSpace)

  // Where this paragraph sits in the galley, snapshotted when it is broken: `yStart` is the galley height it opens
  // at and `bls` the baseline-to-baseline distance, so line n occupies the vertical band [yStart+n*bls, +bls]. That
  // band is what the active cutouts are queried against, mapping a figure's vertical extent onto the lines it narrows.
  private def galley: VerticalMode = t.modeStack(1).asInstanceOf[VerticalMode]
  private var yStart = 0.0
  private var bls    = 0.0

  // The cutout bands as resolved at the instant this paragraph is broken — `(top, bottom, side, profile)` each. They
  // are snapshotted once, before any line is contributed, and used for every line's measure and break penalty.
  // Snapshotting matters: contributing a line can trigger a page break that shifts the figure's live position, so a
  // band re-resolved per line would narrow inconsistently. The snapshot also fixes the figure relative to this
  // paragraph's lines, and the two travel together under any later page break, so the narrowing stays aligned with
  // the figure wherever the wrap ends up.
  private var cutBands: Seq[(Double, Double, Side, Double => Double)] = Nil

  // The widest inset a shaped cutout imposes anywhere in the depth range `[a, b]` a line spans. A rectangle is
  // constant, but a curved silhouette bulges within a single line's height, so the profile is sampled across the
  // band and the maximum taken — at line resolution this keeps text clear of the outline without ever crossing it.
  private def maxInset(profile: Double => Double, a: Double, b: Double): Double =
    val steps = 6
    (0 to steps).iterator.map(i => profile(a + (b - a) * i / steps)).max

  /** The (left, right) inset that the galley's figures impose on line `n` of this paragraph. */
  private def cut(n: Int): (Double, Double) =
    val y0 = yStart + n * bls
    val y1 = yStart + (n + 1) * bls
    cutBands.foldLeft((0.0, 0.0)) { case ((l, r), (top, bottom, side, profile)) =>
      if y1 > top && y0 < bottom then
        // the line's band clipped into the cutout, in depth-below-top coordinates the profile is defined in
        val inset = maxInset(profile, math.max(y0, top) - top, math.min(y1, bottom) - top)
        side match
          case Side.Left  => (math.max(l, inset), r)
          case Side.Right => (l, math.max(r, inset))
      else (l, r)
    }

  override def done(): Unit =
    // An empty paragraph contributes nothing and leaves the surrounding state untouched: it sets no lines and,
    // crucially, does not reset \indent / \hangindent. This is what makes \noindent stick across a paragraph
    // break — a heading macro ends with \noindent (opening an empty paragraph), the blank line after the
    // heading closes that empty paragraph, and the real paragraph that follows is still flush left.
    if boxes.nonEmpty then
      val hsize = t.getNumber("hsize")

      // Fix this paragraph's place in the galley so the line breaker and the line setter agree on which lines a
      // figure narrows. The snapshot is taken before any line is contributed, so it is the height the paragraph
      // opens at; the figures take their room out of the per-line measure through `extraInset`.
      yStart = galley.naturalHeight
      bls = t.getGlue("baselineskip").naturalSize
      cutBands = galley.cutouts.flatMap(c => galley.cutoutBand(c).map((top, bottom) => (top, bottom, c.side, c.profile))).toSeq

      // The paragraph's writing direction, decided once: whether to reorder at all, and at which base.
      val base   = baseLevel
      val doBidi = needsBidi(base)

      // Try Knuth-Plass optimal line breaking first
      KnuthPlass.breakParagraph(boxes.toSeq, hsize, t, n => { val (l, r) = cut(n); l + r }) match
        case Some(lines) if lines.nonEmpty =>
          buildLinesFromOptimal(lines, hsize, base, doBidi)
        case _ =>
          // Fall back to greedy algorithm
          buildLinesGreedy(hsize, base, doBidi)

      t.indentParagraph = true
      // \hangindent / \hangafter apply to a single paragraph and revert afterwards, as in TeX;
      // \leftskip / \rightskip persist until the document changes them.
      t.set("hangindent", 0.0)
      t.set("hangafter", 1.0)

    pop

  /** The left and right margin glue for the line whose 0-based number is `n`, from `\leftskip` /
    * `\rightskip`, a `\hangindent` selected by `\hangafter`, and the inset of any figure the line
    * flows around. The box builder still sets the whole line to `hsize`, so these insets push the
    * justified text into the same narrowed measure the breaker chose its breaks against. A positive
    * `\hangindent` indents on the left, a negative one on the right; a left figure adds to the left
    * margin (text moves right past it), a right figure to the right. */
  private def lineMargins(n: Int): (Glue, Glue) =
    val leftskip   = t.getGlue("leftskip")
    val rightskip  = t.getGlue("rightskip")
    val hangindent = t.getNumber("hangindent")
    val hangafter  = t.getNumber("hangafter").toInt
    val hung       = if hangafter >= 0 then n >= hangafter else n < -hangafter
    val hang       = if hangindent != 0 && hung then hangindent else 0.0
    val (cutL, cutR) = cut(n)
    val left       = if hang > 0 then leftskip + hang + cutL else leftskip + cutL
    val right      = if hang < 0 then rightskip + -hang + cutR else rightskip + cutR
    (left, right)

  /** The page-break penalty between two consecutive lines of a paragraph: interlinepenalty everywhere, plus
    * clubpenalty after the first line and widowpenalty before the last, as in TeX.
    */
  private def penaltyBetween(afterFirst: Boolean, beforeLast: Boolean): Int =
    var p = t.getNumber("interlinepenalty").toInt

    if afterFirst then p += t.getNumber("clubpenalty").toInt
    if beforeLast then p += t.getNumber("widowpenalty").toInt
    p

  /** The wrappenalty for the break after line `n`: a page break there would land strictly inside a wrapped figure's
    * band — between the figure's top and bottom — stranding the figure on the shipped page while the text it
    * narrows spilled onto the next. Returning wrappenalty forbids it, so the whole wrap travels together and the
    * page builder breaks above the figure instead. A figure taller than the page is exempt: forbidding every break
    * inside it would leave the page builder nowhere to break, so it is allowed to split rather than hang.
    */
  private def wrapPenalty(n: Int): Int =
    val breakY = yStart + (n + 1) * bls
    val vsize  = t.getNumber("vsize")
    val strands = cutBands.exists { case (top, bottom, _, _) =>
      breakY > top + 1e-6 && breakY < bottom - 1e-6 && bottom - top <= vsize
    }
    if strands then t.getNumber("wrappenalty").toInt else 0

  /** The paragraph's base writing direction, read when it is broken: 0 left-to-right, 1 right-to-left
    * (\rtl / \ltr set the `pardir` parameter). */
  private def baseLevel: Int = if t.getNumber("pardir") == 1.0 then 1 else 0

  /** Whether this paragraph needs bidirectional processing: a right-to-left base, or any right-to-left
    * character somewhere in its content. A pure left-to-right paragraph with no such character — the
    * overwhelmingly common case — skips reordering entirely and lays out exactly as before. */
  private def needsBidi(base: Int): Boolean =
    base == 1 || boxes.exists {
      case c: CharBox => Bidi.hasRtl(c.text)
      case _          => false
    }

  /** Reorder one line from logical into visual order under base direction `base`, in place.
    *
    * Reordering happens at character granularity, not box granularity: a Hebrew word arrives as a single
    * box whose characters are in logical (reading) order, and they must be reversed within the box so the
    * glyphs are drawn left to right in their visual positions. Every backend draws a run of glyphs in the
    * order given, applying no reordering of its own, so texish owns the bidi here and hands each backend
    * text already in visual order.
    *
    * Each character of each word box becomes one item, each interword glue (or other non-text box) one
    * neutral item, giving a one-to-one map between items and the characters of the bidi string. The
    * Unicode algorithm yields the visual order of those items; walking it, runs of characters sharing a
    * font and colour recombine into word boxes (their text now in visual order) and glue and other boxes
    * pass through at their visual positions. The resolution is per line, with the paragraph base as the
    * surrounding direction — exact for a line whose direction the document fixes. */
  private def reorderVisual(content: ArrayBuffer[Box], base: Int): Unit =
    if content.isEmpty then return

    // Flatten to characters, each remembering its source: a word box contributes its characters, and a glue
    // or other box one neutral character. Character index lines up with the bidi string below.
    val chars   = new StringBuilder
    val srcChar = ArrayBuffer.empty[CharBox] // the source word box for a text character, else null
    val srcBox  = ArrayBuffer.empty[Box]     // the box itself for a non-text character, else null
    for b <- content do
      b match
        case c: CharBox =>
          for ch <- c.text do
            chars.append(ch)
            srcChar += c
            srcBox += null
        case _ =>
          chars.append(if b.isSpace then ' ' else '￼') // glue is neutral whitespace; anything else neutral
          srcChar += null
          srcBox += b

    val s      = chars.toString
    val levels = Bidi.resolvedLevels(s, base)

    // Arabic cursive joining is decided here, in memory order, while the logical string is still intact: each
    // character's contextual form (initial/medial/final/isolated) follows from its neighbours, and a form once
    // decided is independent of the visual order the reorder below imposes. The resolved forms ride along into
    // the visual runs so each Arabic CharBox can substitute its shaped glyphs. Characters in the Arabic blocks
    // are all BMP, so a per-code-unit form array lines up with the per-code-unit run text built below. Null
    // when the line has no Arabic, leaving every other script's path untouched.
    val forms: Array[JoiningForm] =
      if ArabicShaping.hasArabic(s) then ArabicShaping.resolveForms(Array.tabulate(s.length)(k => s.charAt(k).toInt))
      else null

    // Group into reorder units. A base character and the non-spacing marks that follow it form one cluster,
    // so reversing a right-to-left run does not strand a vowel point ahead of its consonant; a non-text box
    // is its own unit. Each unit takes the level of its base character.
    val unitChars = ArrayBuffer.empty[ArrayBuffer[Int]]
    val unitLevel = ArrayBuffer.empty[Int]
    var i         = 0
    while i < s.length do
      val isMark = srcChar(i) != null && Bidi.classify(s.charAt(i).toInt) == BidiClass.NSM
      if isMark && unitChars.nonEmpty && srcChar(unitChars.last.head) != null then unitChars.last += i
      else
        unitChars += ArrayBuffer(i)
        unitLevel += levels(i)
      i += 1

    val order = Bidi.reorderByLevels(unitLevel.toArray)

    // Walk the units in visual order, recombining runs of one font and colour into word boxes whose text is
    // now visual. A unit's own characters stay in logical order (base then marks); a right-to-left bracket
    // is swapped for its mirror image (UAX #9 L4).
    val out      = ArrayBuffer.empty[Box]
    var runText  = null: StringBuilder
    var runSrc   = null: CharBox
    var runForms = null: ArrayBuffer[JoiningForm] // resolved joining forms parallel to runText, when Arabic
    def flushRun(): Unit =
      if runText != null then
        out += (if runForms != null then runSrc.newCharBox(runText.toString, runForms.toArray)
                else runSrc.newCharBox(runText.toString))
        runText = null
        runSrc = null
        runForms = null
    for ui <- order do
      val cis   = unitChars(ui)
      val first = cis.head
      if srcChar(first) != null then
        val src = srcChar(first)
        if runText != null && (runSrc.font != src.font || runSrc.color != src.color) then flushRun()
        if runText == null then
          runText = new StringBuilder
          runSrc = src
          if forms != null then runForms = ArrayBuffer.empty
        for ci <- cis do
          val cp = s.charAt(ci).toInt
          runText.append(if Bidi.isRtlLevel(levels(ci)) then Bidi.mirror(cp).toChar else cp.toChar)
          if runForms != null then runForms += forms(ci)
      else
        flushRun()
        out += srcBox(first)
    flushRun()

    content.clear()
    content ++= out

  /** Set one line: reorder its content for a right-to-left or mixed line, then bracket it with the margin
    * glue. \parfillskip — the stretchable glue that lets the last line fall short of full measure — sits
    * at the trailing edge, which is the right under a left-to-right base and the left under a right-to-left
    * base, so the last line rags toward the start of reading either way.
    *
    * The first-line indent pins to the leading edge of reading — the left under a left-to-right base, the
    * right under a right-to-left one. The reorderer would otherwise treat it as neutral whitespace and sweep
    * it to the wrong side, so it is lifted out before the reorder and put back at the leading edge after. */
  private def emitLine(content: ArrayBuffer[Box], lineIdx: Int, isLast: Boolean, base: Int, doBidi: Boolean, hsize: Double): Box =
    val indent =
      if doBidi && content.nonEmpty && (content.head match { case h: HSpaceBox => h.indent; case _ => false }) then
        Some(content.remove(0))
      else None
    if doBidi then reorderVisual(content, base)
    indent.foreach(box => if base == 1 then content += box else content.insert(0, box))
    val hbox                      = new HBoxBuilder(t, hsize)
    val (leftMargin, rightMargin) = lineMargins(lineIdx)
    hbox add leftMargin
    if isLast && base == 1 then hbox add t.getGlue("parfillskip")
    content.foreach(hbox.add)
    if isLast && base == 0 then hbox add t.getGlue("parfillskip")
    hbox add rightMargin
    hbox.result

  private def buildLinesFromOptimal(lines: Seq[Seq[Box]], hsize: Double, base: Int, doBidi: Boolean): Unit =
    var first = true

    for (lineBoxes, lineIdx) <- lines.zipWithIndex do
      val isLast  = lineIdx == lines.length - 1
      // marks and inserts migrate out of the line to the vertical list, where the page builder can see them
      val migrating = lineBoxes.collect { case m: MigratingBox => m }

      // the line's own content, trailing interword space trimmed before the margins are applied
      val content = ArrayBuffer.from(lineBoxes.iterator.filterNot(_.isInstanceOf[MigratingBox]))
      if content.nonEmpty && content.last.isSpace then content.remove(content.length - 1)

      val newLine = emitLine(content, lineIdx, isLast, base, doBidi, hsize)
      t.modeStack(1) add newLine

      if first then
        val vlist = t.modeStack(1).asInstanceOf[VerticalMode]
        if vlist.length > 1 then vlist.insert(vlist.length - 2, t.getGlue("parskip"))
        first = false

      // migrated items go before the interline penalty, so the penalty stays adjacent to the glue it guards
      migrating.foreach(t.modeStack(1).add)

      if !isLast then
        val p = math.max(penaltyBetween(lineIdx == 0, lineIdx == lines.length - 2), wrapPenalty(lineIdx))
        if p != 0 then t.modeStack(1) add Penalty(p)

  // A line may break only at glue — the discardable, flexible interword space. A kern (an HSpaceBox, the box
  // \kern and the inter-letter spacing of a wordmark produce) is also "space" in the isSpace sense: it is
  // blank and not content, so it is discarded at boundaries and ignored by hasContent. But it is rigid and is
  // not a legal breakpoint, so the greedy fallback must skip past it to the real glue, exactly as Knuth-Plass
  // does (to the optimal breaker a kern is a plain rigid box). Without this, the negative kerns inside a
  // wordmark like \TeX read as interword spaces and the greedy breaker splits the logo across a line.
  private def isBreakSpace(b: Box): Boolean = b.isInstanceOf[Glue]

  private def buildLinesGreedy(hsize: Double, base: Int, doBidi: Boolean): Unit =
    var first   = true
    var lineIdx = 0

    while boxes.nonEmpty do
      // the same per-line measure the optimal path uses: \leftskip/\rightskip and a \hangindent
      // narrow the line, and the margins bracket the content so the whole line still sets to hsize
      val (leftMargin, rightMargin) = lineMargins(lineIdx)
      val rightLimit = hsize - rightMargin.naturalSize
      val measure    = rightLimit - leftMargin.naturalSize
      val hbox       = new HBoxBuilder(t, t.getNumber("hsize"))
      val migrating  = scala.collection.mutable.ArrayBuffer[MigratingBox]()
      hbox add leftMargin

      @tailrec
      def line(): Unit =
        if boxes.nonEmpty then
          if boxes.head.isInstanceOf[DiscretionaryBox] then
            // The greedy fallback does not break at author discretionaries; it lays down their unbroken form
            // and carries on. (The optimal path handles the break opportunities.)
            val d = boxes.remove(0).asInstanceOf[DiscretionaryBox]
            boxes.insertAll(0, d.noBreak)
            line()
          else if boxes.head.isInstanceOf[MigratingBox] then
            // marks and inserts migrate out of the line to the vertical list, where the page builder can see them
            migrating += boxes.remove(0).asInstanceOf[MigratingBox]
            line()
          else if hbox.size + boxes.head.width <= rightLimit then
            hbox add boxes.remove(0)
            line()
          else if boxes.head.width > measure then
            println(s"Warning: overflow: ${boxes.head}")
            hbox add boxes.remove(0)
          else
            val sizeBefore = hbox.size
            boxes.head match
              case b: CharBox =>
                b.text.indexOf('-') match
                  case -1 =>
                    Hyphenation(t.hyphenationLanguage, b.text) match
                      case None =>
                      case Some(hyphenation) =>
                        var lastBefore: CharBox = null
                        var lastAfter: String   = null

                        @tailrec
                        def longest(): Unit =
                          if hyphenation.hasNext then
                            val (before, after) = hyphenation.next()
                            val beforeHyphen    = b.newCharBox(before)

                            if hbox.size + beforeHyphen.width <= rightLimit then
                              lastBefore = beforeHyphen
                              lastAfter = after
                              longest()

                        longest()

                        if lastBefore ne null then
                          hbox add lastBefore
                          boxes.remove(0)
                          boxes.insert(0, b.newCharBox(lastAfter))
                    end match
                  case idx =>
                    val beforeHyphen = b.newCharBox(b.text.substring(0, idx + 1))

                    if hbox.size + beforeHyphen.width <= rightLimit then
                      hbox add beforeHyphen
                      boxes.remove(0)
                      boxes.insert(0, b.newCharBox(b.text.substring(idx + 1)))
              case _ =>
            end match

            // A box did not fit and could not be hyphenated onto the line. Back up to the last interword
            // space so a box that *did* fit — an opening "(" before a wide inline \verb, say — is not
            // stranded at the line end before the box that did not; the run since that space moves to the
            // next line, the break TeX would have made. (Reached only on the greedy fallback, when
            // Knuth-Plass found no solution within the tolerance.)
            //
            // The back-up must leave real content on the line, or no box is consumed and the enclosing
            // `while boxes.nonEmpty` spins forever. The opening \leftskip glue reports as a space, so when the
            // first content box already fills the line and the next will not fit, the run scanned back is the
            // whole line and the only "space" before it is that leading margin — backing up past it would
            // empty the line and re-queue the same boxes. In that case keep the content here (an overfull line
            // that makes progress) rather than backing up.
            if hbox.size == sizeBefore && !boxes.head.isSpace then
              val carry = ArrayBuffer[Box]()
              while hbox.nonEmpty && !isBreakSpace(hbox.last) do carry.prepend(hbox.removeLast())
              if hbox.nonEmpty && isBreakSpace(hbox.last) && carry.nonEmpty && hbox.list.dropRight(1).exists(!_.isSpace)
              then
                hbox.removeLast()
                boxes.insertAll(0, carry.toSeq)
              else carry.foreach(hbox.add)

      line()

      if hbox.nonEmpty && hbox.last.isSpace then hbox.removeLast()
      if boxes.nonEmpty && boxes.head.isSpace then boxes.remove(0)

      // Harvest the content the fit loop laid down — everything after the leading \leftskip — and set the
      // line through the shared emitter, so a right-to-left or mixed line is reordered and \parfillskip
      // lands on the correct edge, exactly as on the optimal path.
      val content = ArrayBuffer.from(hbox.list.drop(1))
      val newLine = emitLine(content, lineIdx, boxes.isEmpty, base, doBidi, hsize)

      // a greedy line is final exactly when it exhausted the paragraph's boxes. This penalty guards the break
      // before the line just built — line lineIdx — so the wrap check is for the break after line lineIdx-1.
      if lineIdx > 0 then
        val p = math.max(penaltyBetween(lineIdx == 1, boxes.isEmpty), wrapPenalty(lineIdx - 1))
        if p != 0 then t.modeStack(1) add Penalty(p)

      t.modeStack(1) add newLine

      if first then
        val vlist = t.modeStack(1).asInstanceOf[VerticalMode]

        if vlist.length > 1 then vlist.insert(vlist.length - 2, t.getGlue("parskip"))
        first = false

      migrating.foreach(t.modeStack(1).add)

      lineIdx += 1
    end while
