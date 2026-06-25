package io.github.edadma.texish

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

  /** The (left, right) inset that the galley's figures impose on line `n` of this paragraph. */
  private def cut(n: Int): (Double, Double) =
    galley.insetsAt(yStart + n * bls, yStart + (n + 1) * bls)

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

      // Try Knuth-Plass optimal line breaking first
      KnuthPlass.breakParagraph(boxes.toSeq, hsize, t, n => { val (l, r) = cut(n); l + r }) match
        case Some(lines) if lines.nonEmpty =>
          buildLinesFromOptimal(lines, hsize)
        case _ =>
          // Fall back to greedy algorithm
          buildLinesGreedy(hsize)

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

  private def buildLinesFromOptimal(lines: Seq[Seq[Box]], hsize: Double): Unit =
    var first = true

    for (lineBoxes, lineIdx) <- lines.zipWithIndex do
      val hbox    = new HBoxBuilder(t, hsize)
      val isLast  = lineIdx == lines.length - 1
      // marks and inserts migrate out of the line to the vertical list, where the page builder can see them
      val migrating = lineBoxes.collect { case m: MigratingBox => m }

      // the line's own content, trailing interword space trimmed before the margins are applied
      val content = ArrayBuffer.from(lineBoxes.iterator.filterNot(_.isInstanceOf[MigratingBox]))
      if content.nonEmpty && content.last.isSpace then content.remove(content.length - 1)

      // \leftskip (and a left hanging indent) opens the line; the content, then \parfillskip on the
      // last line, then \rightskip (and a right hanging indent) close it — the whole line set to hsize
      val (leftMargin, rightMargin) = lineMargins(lineIdx)
      hbox add leftMargin
      content.foreach(hbox.add)
      if isLast then hbox add t.getGlue("parfillskip")
      hbox add rightMargin

      val newLine = hbox.result
      t.modeStack(1) add newLine

      if first then
        val vlist = t.modeStack(1).asInstanceOf[VerticalMode]
        if vlist.length > 1 then vlist.insert(vlist.length - 2, t.getGlue("parskip"))
        first = false

      // migrated items go before the interline penalty, so the penalty stays adjacent to the glue it guards
      migrating.foreach(t.modeStack(1).add)

      if !isLast then
        val p = penaltyBetween(lineIdx == 0, lineIdx == lines.length - 2)
        if p != 0 then t.modeStack(1) add Penalty(p)

  private def buildLinesGreedy(hsize: Double): Unit =
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
            if hbox.size == sizeBefore && !boxes.head.isSpace then
              val carry = ArrayBuffer[Box]()
              while hbox.nonEmpty && !hbox.last.isSpace do carry.prepend(hbox.removeLast())
              if hbox.nonEmpty && hbox.last.isSpace && carry.nonEmpty then
                hbox.removeLast()
                boxes.insertAll(0, carry.toSeq)
              else carry.foreach(hbox.add)

      line()

      if hbox.nonEmpty && hbox.last.isSpace then hbox.removeLast()
      if boxes.nonEmpty && boxes.head.isSpace then boxes.remove(0)
      if boxes.isEmpty then hbox add t.getGlue("parfillskip")
      hbox add rightMargin

      val newLine = hbox.result

      // a greedy line is final exactly when it exhausted the paragraph's boxes
      if lineIdx > 0 then
        val p = penaltyBetween(lineIdx == 1, boxes.isEmpty)
        if p != 0 then t.modeStack(1) add Penalty(p)

      t.modeStack(1) add newLine

      if first then
        val vlist = t.modeStack(1).asInstanceOf[VerticalMode]

        if vlist.length > 1 then vlist.insert(vlist.length - 2, t.getGlue("parskip"))
        first = false

      migrating.foreach(t.modeStack(1).add)

      lineIdx += 1
    end while
