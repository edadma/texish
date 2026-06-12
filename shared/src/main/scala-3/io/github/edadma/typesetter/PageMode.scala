package io.github.edadma.typesetter

/** Builds the main vertical list and breaks it into pages.
  *
  * Pages break only at legal breakpoints, following TeX's rules: at a glue whose predecessor is non-discardable, or
  * at a penalty below [[Penalty.Inhibit]]. A penalty at or below [[Penalty.Force]] ships the current page
  * immediately. When a page overflows, the latest legal breakpoint that still fits is chosen (first-fit); the break
  * item and any discardables after it are dropped, so a new page never starts with stray space.
  */
class PageMode(t: Typesetter) extends VBoxBuilder(t):

  override infix def add(box: Box): Unit =
    box match
      case p: Penalty if p.penalty <= Penalty.Force =>
        if nonEmpty then newpage()
      case _ =>
        super.add(box)

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
    t.getDocument add result
    clear()

  override def result: Box = wrap(buildTo(t.getNumber("vsize")))
