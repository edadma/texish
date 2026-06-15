package io.github.edadma.texish

import io.github.edadma.texish.parser.Value

/** Builds the main vertical list and breaks it into pages.
  *
  * Pages break only at legal breakpoints, following TeX's rules: at a glue whose predecessor is non-discardable, or
  * at a penalty below [[Penalty.Inhibit]]. A penalty at or below [[Penalty.Force]] ships the current page
  * immediately. When a page overflows, the latest legal breakpoint that still fits is chosen (first-fit); the break
  * item and any discardables after it are dropped, so a new page never starts with stray space.
  *
  * Footnotes arrive as zero-size [[InsertBox]] items whose content nonetheless occupies page height: every fitting
  * decision counts an insert's content (plus the separator above the footnote block, once) against the page, and
  * shipout moves the content to the foot of the page below the separator rule.
  */
class PageMode(t: Typesetter) extends VBoxBuilder(t):

  private val FootnoteRuleHeight = 0.4
  private val FootnoteRuleWidth  = 2 * t.in
  private val FootnoteRuleGap    = 2.6 // breathing room between the rule and the first footnote, as in TeX

  private var topskipDone = false

  /** Height of the separator the shipout places above the footnote block: the footnotesep space, the rule, and
    * the gap below the rule.
    */
  private def separatorSize: Double =
    t.getGlue("footnotesep").naturalSize + FootnoteRuleHeight + FootnoteRuleGap

  /** Height the top floats among `items` add to a page once shipped: the floats' own heights, the floatsep glue
    * between each adjacent pair, and the textfloatsep glue below the whole block before the body. Zero when no
    * float is present.
    */
  private def floatAreaSize(items: collection.Seq[Box]): Double =
    val floats = items.collect { case f: FloatBox => f.content.height }

    if floats.isEmpty then 0
    else floats.sum + (floats.length - 1) * t.getGlue("floatsep").naturalSize + t.getGlue("textfloatsep").naturalSize

  /** Page height the given items will occupy once shipped: their own heights, the footnote content carried by any
    * inserts among them (plus the footnote separator, once), and the top-float area (floats plus their spacing).
    */
  private def shippedSize(items: collection.Seq[Box]): Double =
    val notes = items.collect { case ins: InsertBox => ins.content.height }

    items.map(measure).sum + (if notes.isEmpty then 0 else notes.sum + separatorSize) + floatAreaSize(items)

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

        if shippedSize(boxes) > t.getNumber("vsize") then breakPage()

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

    // sizes(i) is the shipped height of boxes(0 until i), i.e. the page if we break at item i — including the
    // footnote content of any inserts and the top floats in the prefix, by the same rule the overflow check in
    // add uses
    val heights = boxes.scanLeft(0.0)(_ + measure(_))
    val notes = boxes.scanLeft(0.0)((acc, b) =>
      acc + (b match
        case ins: InsertBox => ins.content.height
        case _              => 0.0))

    def sizes(i: Int): Double =
      heights(i) + (if notes(i) > 0 then notes(i) + separatorSize else 0) + floatAreaSize(boxes.take(i))

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
    // the floats and inserts come out of the body: float content reappears at the top of the page above a
    // textfloatsep space, insert content at the foot below the footnotesep space and the separator rule. The body
    // is built short by exactly the height the two areas take, so the page is still exactly vsize tall.
    val floats = boxes.collect { case f: FloatBox => f.content }.toList
    val notes  = boxes.collect { case ins: InsertBox => ins.content }.toList

    boxes.filterInPlace(b => !b.isInstanceOf[FloatBox] && !b.isInstanceOf[InsertBox])

    // ragged bottoms trade vertical justification for never stretching the page's own glue: the fil soaks up
    // all the slack, so content stays at its natural spacing and short pages end quietly
    if t.getNumber("raggedbottom") != 0 then super.add(FilGlue)

    val top: List[Box] =
      if floats.isEmpty then Nil
      else
        val sep = VSpaceBox(t.getGlue("floatsep").naturalSize)

        floats.head :: floats.tail.flatMap(f => List(sep, f)) ::: List(VSpaceBox(t.getGlue("textfloatsep").naturalSize))

    val topHeight = top.map(_.height).sum

    if notes.isEmpty then wrap(top ++ buildTo(t.getNumber("vsize") - topHeight))
    else
      val body = buildTo(t.getNumber("vsize") - topHeight - notes.map(_.height).sum - separatorSize)
      val foot = VSpaceBox(t.getGlue("footnotesep").naturalSize)
        :: RuleBox(t, FootnoteRuleWidth, FootnoteRuleHeight, 0)
        :: VSpaceBox(FootnoteRuleGap) :: notes

      wrap(top ++ body ++ foot)
