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

  /** Height a stack of float blocks adds to a page once shipped: the floats' own heights, the floatsep glue between
    * each adjacent pair, and the textfloatsep glue separating the whole block from the body. Zero when empty.
    */
  private def floatStackSize(heights: collection.Seq[Double]): Double =
    if heights.isEmpty then 0
    else heights.sum + (heights.length - 1) * t.getGlue("floatsep").naturalSize + t.getGlue("textfloatsep").naturalSize

  /** Height the floats among `items` add to a page once shipped: the top-float stack plus the bottom-float stack,
    * each with its own separators.
    */
  private def floatAreaSize(items: collection.Seq[Box]): Double =
    floatStackSize(items.collect { case f: FloatBox if f.edgeTop => f.content.height }) +
      floatStackSize(items.collect { case f: FloatBox if !f.edgeTop => f.content.height })

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

      // a float that prefers "here" placement stays inline at this point when the current page can still hold its
      // block; otherwise it falls back to its next listed edge (and then floats like any top/bottom insert). With
      // no further preference it floats to the top, so a here-only float that does not fit is never lost — the
      // plain-TeX \midinsert rule, "leave it here if it fits, else turn it into a \topinsert".
      case f: FloatBox if f.placement.headOption.contains('h') =>
        if shippedSize(boxes) + f.content.height <= t.getNumber("vsize") then add(f.content)
        else
          val rest = f.placement.tail
          add(new FloatBox(f.content, if rest.isEmpty then List('t') else rest))

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

    // The break is chosen by cost, as in TeX, rather than by taking the latest legal break that happens to fit.
    // The cost of breaking at item i is the page's badness there plus the penalty at the break: badness measures
    // how hard the page glue must work to reach vsize — stretching when the content is short, shrinking when it
    // overruns — and a positive penalty makes a discouraged break less attractive even when it fits. A page that
    // cannot reach vsize because it would overrun beyond the available shrink is infeasible. The least-cost
    // feasible break wins; ties go to the later break, so a page still carries as much as it can. On a rigid text
    // page (interline glue has no stretch) every fitting break is equally bad, so ties alone pick the latest one
    // that fits — exactly the old first-fit — and the cost model only changes the outcome where glue can flex or a
    // penalty intervenes.
    val glueStretch = boxes.scanLeft(0.0)((a, b) => a + (b match { case g: Glue if g.stretchOrder == 0 => g.stretch; case _ => 0.0 }))
    val glueShrink  = boxes.scanLeft(0.0)((a, b) => a + (b match { case g: Glue if g.shrinkOrder == 0 => g.shrink; case _ => 0.0 }))
    val canFill     = boxes.scanLeft(false)((a, b) => a || (b match { case g: Glue => g.stretchOrder >= 1; case _ => false }))

    val Infeasible = Double.PositiveInfinity

    def cost(i: Int): Double =
      val natural = sizes(i)
      val badness =
        if natural <= vsize then
          val short = vsize - natural
          if canFill(i) || short <= 1e-6 then 0.0
          else if glueStretch(i) <= 1e-6 then 10000.0
          else math.min(10000.0, 100.0 * math.pow(short / glueStretch(i), 3))
        else
          val over = natural - vsize
          if over <= glueShrink(i) + 1e-6 && glueShrink(i) > 1e-6 then
            math.min(10000.0, 100.0 * math.pow(over / glueShrink(i), 3))
          else Infeasible
      val penalty = boxes(i) match { case p: Penalty => p.penalty.toDouble; case _ => 0.0 }
      if badness == Infeasible then Infeasible else badness + penalty

    // descending, so the first occurrence of the minimum cost is the latest break among ties
    val legalDescending = ((boxes.length - 1) to 1 by -1).filter(legal)

    legalDescending.filter(cost(_) < Infeasible).minByOption(cost).orElse(legalDescending.headOption) match
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

    if nonEmpty then
      // end of document, as plain TeX's \bye does with \vfill\eject: the final page is under-full, so cap it
      // with an infinite fil to soak up the slack and keep its content at the top — otherwise a flush-bottom
      // page (raggedbottom == 0) would justify, stretching the small interparagraph glue down the whole page.
      // When raggedbottom is set, result already appends this fil to every page, so don't add a second one.
      if t.getNumber("raggedbottom") == 0 then super.add(FilGlue)
      newpage()

  def newpage(): Unit =
    // a cutout whose figure ships with this page is finished; one whose figure is carried to the next page is not
    // among these boxes and survives, its band re-derived from the figure's position there. No coordinate rebasing
    // is needed: cutout tops are resolved from the live galley, not stored absolutely (see Cutout).
    cutouts.filterInPlace(c => !boxes.exists(_ eq c.anchor))

    // record this page's marks before shipping, so shipout-time material (running headers) reads them: topmark
    // is the previous page's botmark, and a markless page inherits it for firstmark and botmark too, as in TeX
    val marks = boxes.collect { case m: MarkBox => m.text }
    val top = t.get("botmark") match
      case Some(Value.Text(s)) => s
      case _                   => ""

    t.set("topmark", top)
    t.set("firstmark", marks.headOption.getOrElse(top))
    t.set("botmark", marks.lastOption.getOrElse(top))

    // cross-references resolve here too: a label or a contents entry learns its folio from the page it ships on.
    // pageno is still the shipping page's number at this point (DocumentMode advances it after the add). The walk
    // descends through the page's boxes: a label or \addcontentsline inside a float or footnote (e.g. the
    // \caption/\label of a figure, which ride a \centerline hbox inside the float's vbox rather than migrating to
    // the page's vertical list) ships on this page too and must learn the same folio. The same holds for a wrapped
    // figure's caption, which rides inside the OverlayBox the wrap anchors at this point.
    val folio = t.getNumber("pageno").toInt

    def resolve(box: Box): Unit = box match
      case l: LabelBox    => t.references.setPage(l.name, folio)
      case e: TocMarkBox  => t.references.recordList(e.list, e.level, e.number, e.title, folio)
      case f: FloatBox    => resolve(f.content)
      case ins: InsertBox => resolve(ins.content)
      case o: OverlayBox  => resolve(o.content)
      case v: VerticalBox => v.boxes.foreach(resolve)
      case h: HBox        => h.boxes.foreach(resolve)
      case _              =>

    boxes.foreach(resolve)

    t.getDocument add result
    clear()

  override def result: Box =
    // the floats and inserts come out of the body and reappear around it. The page is assembled top to bottom as
    // top floats, body, footnotes, bottom floats — the order TeX/LaTeX use: top floats head the page above a
    // textfloatsep space, footnotes sit at the foot of the text below the separator rule, and bottom floats sink
    // below them. The body is built short by exactly the height all three areas take, so the page stays vsize tall.
    val topFloats = boxes.collect { case f: FloatBox if f.edgeTop => f.content }.toList
    val botFloats = boxes.collect { case f: FloatBox if !f.edgeTop => f.content }.toList
    val notes     = boxes.collect { case ins: InsertBox => ins.content }.toList

    boxes.filterInPlace(b => !b.isInstanceOf[FloatBox] && !b.isInstanceOf[InsertBox])

    // ragged bottoms trade vertical justification for never stretching the page's own glue: the fil soaks up
    // all the slack, so content stays at its natural spacing and short pages end quietly
    if t.getNumber("raggedbottom") != 0 then super.add(FilGlue)

    // a float stack with its blocks separated by floatsep and the textfloatsep that holds it off the body — placed
    // before the body for a top stack, after it for a bottom stack
    def floatStack(floats: List[Box], top: Boolean): List[Box] =
      if floats.isEmpty then Nil
      else
        val sep     = VSpaceBox(t.getGlue("floatsep").naturalSize)
        val stacked = floats.head :: floats.tail.flatMap(f => List(sep, f))

        if top then stacked :+ VSpaceBox(t.getGlue("textfloatsep").naturalSize)
        else VSpaceBox(t.getGlue("textfloatsep").naturalSize) :: stacked

    val top    = floatStack(topFloats, top = true)
    val bottom = floatStack(botFloats, top = false)
    val around = top.map(_.height).sum + bottom.map(_.height).sum

    if notes.isEmpty then wrap(top ++ buildTo(t.getNumber("vsize") - around) ++ bottom)
    else
      val body = buildTo(t.getNumber("vsize") - around - notes.map(_.height).sum - separatorSize)
      val foot = VSpaceBox(t.getGlue("footnotesep").naturalSize)
        :: RuleBox(t, FootnoteRuleWidth, FootnoteRuleHeight, 0)
        :: VSpaceBox(FootnoteRuleGap) :: notes

      wrap(top ++ body ++ foot ++ bottom)
