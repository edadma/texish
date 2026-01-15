package io.github.edadma.typesetter

import scala.annotation.tailrec

class ParagraphMode(val t: Typesetter) extends HorizontalMode:
  def result: Box = ???

  override def done(): Unit =
    val hsize = t.getNumber("hsize")

    // Try Knuth-Plass optimal line breaking first
    KnuthPlass.breakParagraph(boxes.toSeq, hsize, t) match
      case Some(lines) if lines.nonEmpty =>
        buildLinesFromOptimal(lines, hsize)
      case _ =>
        // Fall back to greedy algorithm
        buildLinesGreedy(hsize)

    t.indentParagraph = true
    pop

  private def buildLinesFromOptimal(lines: Seq[Seq[Box]], hsize: Double): Unit =
    var first = true

    for (lineBoxes, lineIdx) <- lines.zipWithIndex do
      val hbox    = new HBoxBuilder(t, hsize)
      val isLast  = lineIdx == lines.length - 1

      for box <- lineBoxes do
        hbox add box

      // Remove trailing space
      if hbox.nonEmpty && hbox.last.isSpace then hbox.removeLast()

      // Add parfillskip to last line
      if isLast then hbox add t.getGlue("parfillskip")

      val newLine = hbox.result
      t.modeStack(1) add newLine

      if first then
        val vlist = t.modeStack(1).asInstanceOf[VerticalMode]
        if vlist.length > 1 then vlist.insert(vlist.length - 2, t.getGlue("parskip"))
        first = false

  private def buildLinesGreedy(hsize: Double): Unit =
    var first = true

    while boxes.nonEmpty do
      val hbox = new HBoxBuilder(t, t.getNumber("hsize"))

      @tailrec
      def line(): Unit =
        if boxes.nonEmpty then
          if hbox.size + boxes.head.width <= hsize then
            hbox add boxes.remove(0)
            line()
          else if boxes.head.width > hsize then
            println(s"Warning: overflow: ${boxes.head}")
            hbox add boxes.remove(0)
          else
            boxes.head match
              case b: CharBox =>
                b.text.indexOf('-') match
                  case -1 =>
                    Hyphenation(b.text) match
                      case None =>
                      case Some(hyphenation) =>
                        var lastBefore: CharBox = null
                        var lastAfter: String   = null

                        @tailrec
                        def longest(): Unit =
                          if hyphenation.hasNext then
                            val (before, after) = hyphenation.next
                            val beforeHyphen    = b.newCharBox(before)

                            if hbox.size + beforeHyphen.width <= t.getNumber("hsize") then
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

                    if hbox.size + beforeHyphen.width <= t.getNumber("hsize") then
                      hbox add beforeHyphen
                      boxes.remove(0)
                      boxes.insert(0, b.newCharBox(b.text.substring(idx + 1)))
              case _ =>
            end match

      line()

      if hbox.nonEmpty && hbox.last.isSpace then hbox.removeLast()
      if boxes.nonEmpty && boxes.head.isSpace then boxes.remove(0)
      if boxes.isEmpty then hbox add t.getGlue("parfillskip")

      val newLine = hbox.result

      t.modeStack(1) add newLine

      if first then
        val vlist = t.modeStack(1).asInstanceOf[VerticalMode]

        if vlist.length > 1 then vlist.insert(vlist.length - 2, t.getGlue("parskip"))
        first = false
    end while
