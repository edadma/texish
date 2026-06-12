package io.github.edadma.typesetter

import io.github.edadma.typesetter.texish.Value

/** Builds the main vertical list and breaks it into pages.
  *
  * Pages break only at legal breakpoints, following TeX's rules: at a glue whose predecessor is non-discardable, or
  * at a penalty below [[Penalty.Inhibit]]. A penalty at or below [[Penalty.Force]] ships the current page
  * immediately. When a page overflows, the latest legal breakpoint that still fits is chosen (first-fit); the break
  * item and any discardables after it are dropped, so a new page never starts with stray space.
  */
class PageMode(t: Typesetter) extends VBoxBuilder(t):

  private var topskipDone = false

  override def clear(): Unit =
    super.clear()
    topskipDone = false

  override infix def add(box: Box): Unit =
    box match
      case p: Penalty if p.penalty <= Penalty.Force =>
        if nonEmpty then newpage()
      case _ =>
        super.add(box)

        // topskip: when the page's first box arrives, pad above it so the first baseline sits at the same
        // place on every page, however tall that line happens to be; a line taller than topskip gets no pad.
        // Inserted at contribution time so the overflow check below sees the true page size.
        if !topskipDone && !box.isSpace && !box.isInstanceOf[ControlBox] then
          val pad = t.getGlue("topskip").naturalSize - box.ascent

          if pad > 0 then insert(length - 1, VSpaceBox(pad))
          topskipDone = true

        if size > t.getNumber("vsize") then breakPage()

  /** The page is overfull: ship everything before the latest legal breakpoint whose page content still fits, and
    * carry the rest onto the next page. If no legal breakpoint fits, the latest legal one is used anyway (the page
    * ships overfull and is reported by glue setting); with no legal breakpoint at all the material simply
    * accumulates until one arrives.
    */
  private def breakPage(): Unit =
    val vsize = t.getNumber("vsize")

    def legal(i: Int): Boolean =
      boxes(i) match
        case p: Penalty => p.penalty < Penalty.Inhibit
        case g: Glue    => !g.nobreak && i > 0 && !boxes(i - 1).isSpace
        case _          => false

    // sizes(i) is the height of boxes(0 until i), i.e. the page if we break at item i
    val sizes      = boxes.scanLeft(0.0)(_ + measure(_))
    val candidates = (boxes.length - 1) to 1 by -1

    candidates.find(i => legal(i) && sizes(i) <= vsize).orElse(candidates.find(legal)) match
      case None =>
      case Some(i) =>
        val carried = boxes.drop(i).toList

        dropRightInPlace(boxes.length - i)
        newpage()
        // the break item and any discardables after it vanish at the page top; re-adding through add lets
        // material spanning several pages cascade into further breaks
        carried.dropWhile(_.isSpace).foreach(this.add)

  /** The document is ending: ship whatever remains, but only if something does — a document that just ejected
    * its last page must not get a trailing blank one. Shipping goes through newpage (and so getDocument) like
    * every other page, never through whatever mode happens to sit under this one on the stack.
    */
  override def done(): Unit =
    pop

    if nonEmpty then newpage()

  def newpage(): Unit =
    // record this page's marks before shipping, so shipout-time material (running headers) reads them: topmark
    // is the previous page's botmark, and a markless page inherits it for firstmark and botmark too, as in TeX
    val marks = boxes.collect { case m: MarkBox => m.text }
    val top = t.get("botmark") match
      case Some(Value.Text(s)) => s
      case _                   => ""

    t.set("topmark", top)
    t.set("firstmark", marks.headOption.getOrElse(top))
    t.set("botmark", marks.lastOption.getOrElse(top))

    t.getDocument add result
    clear()

  override def result: Box =
    // ragged bottoms trade vertical justification for never stretching the page's own glue: the fil soaks up
    // all the slack, so content stays at its natural spacing and short pages end quietly
    if t.getNumber("raggedbottom") != 0 then super.add(FilGlue)

    wrap(buildTo(t.getNumber("vsize")))
