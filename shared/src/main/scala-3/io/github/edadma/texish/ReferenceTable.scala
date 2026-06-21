package io.github.edadma.texish

import scala.collection.mutable

/** A resolved cross-reference: the text a `\ref` prints (a section or figure number, captured where the label was
  * declared) and the folio of the page the label landed on, filled in when that page ships.
  */
case class RefEntry(text: String, page: Int)

/** One line of a table of contents: how deep the entry sits, the number it shows, its title, and the page it
  * starts on. Collected as sectioning commands ship and replayed by `\tableofcontents`.
  */
case class TocEntry(level: Int, number: String, title: String, page: Int)

/** The cross-reference store shared across the passes of one document run.
  *
  * Forward references — a `\ref` to a later `\label`, or a `\tableofcontents` to sections that follow it — cannot
  * be resolved while the document is read once top to bottom: the number or page is not known yet. So a document is
  * typeset more than once over a single table. Each pass *collects* into the pending side — a label declares its
  * text the moment it is read and gets its page when the page ships — while `\ref`/`\pageref`/`\tableofcontents`
  * *read* the resolved side, which is the previous pass's finished collection. [[commit]] promotes pending to
  * resolved between passes and reports whether anything moved, so the driver keeps running passes until the
  * references stop changing (a table of contents that grows a page shifts every folio after it, so one extra pass
  * is sometimes not enough — exactly the reason LaTeX asks you to "rerun").
  */
class ReferenceTable:
  private var resolvedRefs: Map[String, RefEntry] = Map.empty
  private var resolvedToc: Vector[TocEntry]       = Vector.empty

  // insertion-ordered so a stable run produces byte-identical output across passes
  private val pendingRefs = mutable.LinkedHashMap[String, RefEntry]()
  private val pendingToc  = mutable.ArrayBuffer[TocEntry]()

  /** Record a label's reference text the moment it is declared. Its page is unknown until the page ships, so it
    * starts at zero and is filled in by [[setPage]]; a backward `\ref` in the same pass already sees the text here.
    */
  def declare(name: String, text: String): Unit =
    pendingRefs(name) = RefEntry(text, pendingRefs.get(name).map(_.page).getOrElse(0))

  /** Set a label's page once the page carrying its [[LabelBox]] is shipped. */
  def setPage(name: String, page: Int): Unit =
    val e = pendingRefs.getOrElse(name, RefEntry("", 0))
    pendingRefs(name) = e.copy(page = page)

  /** Append a table-of-contents line as its sectioning command ships. */
  def recordToc(level: Int, number: String, title: String, page: Int): Unit =
    pendingToc += TocEntry(level, number, title, page)

  /** The reference text for `\ref`: the current pass's value if the label is already declared (a backward
    * reference), otherwise the value the previous pass resolved (a forward reference).
    */
  def refText(name: String): Option[String] =
    pendingRefs.get(name).map(_.text).filter(_.nonEmpty).orElse(resolvedRefs.get(name).map(_.text))

  /** The page for `\pageref`: a real (non-zero) folio from either side, preferring the resolved value because a
    * label declared earlier this pass may sit on a page that has not shipped yet.
    */
  def refPage(name: String): Option[Int] =
    resolvedRefs.get(name).map(_.page).filter(_ > 0).orElse(pendingRefs.get(name).map(_.page).filter(_ > 0))

  /** The table of contents `\tableofcontents` replays: the previous pass's collection, in document order. */
  def toc: Vector[TocEntry] = resolvedToc

  /** Promote this pass's collection to the resolved set the next pass will read, and report whether it differs
    * from what was resolved before — i.e. whether another pass could still change the output.
    */
  def commit(): Boolean =
    val newRefs = pendingRefs.toMap
    val newToc  = pendingToc.toVector
    val changed = newRefs != resolvedRefs || newToc != resolvedToc

    resolvedRefs = newRefs
    resolvedToc = newToc
    pendingRefs.clear()
    pendingToc.clear()
    changed

/** An invisible point in the vertical list that binds a label name to the page it lands on. Like a [[MarkBox]] it
  * migrates out of a paragraph to the enclosing vertical list and draws nothing; when its page ships, the page
  * builder records the label's folio in the [[ReferenceTable]].
  */
class LabelBox(val name: String) extends MigratingBox:
  val isSpace: Boolean = false

  override def toString: String = s"LabelBox($name)"

/** An invisible point that records a table-of-contents line for the page it lands on. Emitted by a sectioning
  * command so the entry's page is the page the heading actually starts.
  */
class TocMarkBox(val level: Int, val number: String, val title: String) extends MigratingBox:
  val isSpace: Boolean = false

  override def toString: String = s"TocMarkBox($level, $number, $title)"
