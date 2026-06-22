package io.github.edadma.texish

import scala.collection.mutable

/** A resolved cross-reference, captured where the label was declared: the text a `\ref` prints (a section or figure
  * number) and the folio of the page the label landed on, filled in when that page ships. `kind` is the display word
  * a `\autoref` prefixes — "Section", "Figure" — and `name` is the title a `\nameref` prints; both are empty for a
  * bare `\label` declared with no surrounding sectioning or caption command to supply them.
  */
case class RefEntry(text: String, page: Int, kind: String = "", name: String = "")

/** One line of a contents list: how deep the entry sits, the number it shows, its title, and the page it starts on.
  * Collected as sectioning and caption commands ship and replayed by `\tableofcontents` / `\listoffigures` /
  * `\listoftables`, each of which reads its own named list.
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
  private var resolvedRefs: Map[String, RefEntry]          = Map.empty
  private var resolvedLists: Map[String, Vector[TocEntry]] = Map.empty

  // insertion-ordered so a stable run produces byte-identical output across passes
  private val pendingRefs  = mutable.LinkedHashMap[String, RefEntry]()
  private val pendingLists = mutable.LinkedHashMap[String, mutable.ArrayBuffer[TocEntry]]()

  /** Record a label's reference text — and, when a sectioning or caption command supplies them, the display word
    * `\autoref` prefixes and the title `\nameref` prints — the moment the label is declared. Its page is unknown
    * until the page ships, so it starts at zero and is filled in by [[setPage]]; a backward `\ref` in the same pass
    * already sees the text here.
    */
  def declare(name: String, text: String, kind: String = "", refName: String = ""): Unit =
    val page = pendingRefs.get(name).map(_.page).getOrElse(0)
    pendingRefs(name) = RefEntry(text, page, kind, refName)

  /** Set a label's page once the page carrying its [[LabelBox]] is shipped. */
  def setPage(name: String, page: Int): Unit =
    val e = pendingRefs.getOrElse(name, RefEntry("", 0))
    pendingRefs(name) = e.copy(page = page)

  /** Append a line to a named contents list as its sectioning or caption command ships — `"toc"` for the table of
    * contents, `"lof"` for the list of figures, `"lot"` for the list of tables.
    */
  def recordList(list: String, level: Int, number: String, title: String, page: Int): Unit =
    pendingLists.getOrElseUpdate(list, mutable.ArrayBuffer[TocEntry]()) += TocEntry(level, number, title, page)

  /** Append a table-of-contents line: the common case of [[recordList]] into the `"toc"` list. */
  def recordToc(level: Int, number: String, title: String, page: Int): Unit =
    recordList("toc", level, number, title, page)

  /** The reference text for `\ref`: the current pass's value if the label is already declared (a backward
    * reference), otherwise the value the previous pass resolved (a forward reference).
    */
  def refText(name: String): Option[String] =
    pendingRefs.get(name).map(_.text).filter(_.nonEmpty).orElse(resolvedRefs.get(name).map(_.text))

  /** The display word `\autoref` prefixes ("Section", "Figure"), resolved like [[refText]] across passes. Empty for
    * a label declared with no sectioning/caption context, in which case `\autoref` falls back to a bare number.
    */
  def refKind(name: String): Option[String] =
    pendingRefs.get(name).map(_.kind).filter(_.nonEmpty).orElse(resolvedRefs.get(name).map(_.kind).filter(_.nonEmpty))

  /** The title `\nameref` prints, resolved like [[refText]] across passes. */
  def refName(name: String): Option[String] =
    pendingRefs.get(name).map(_.name).filter(_.nonEmpty).orElse(resolvedRefs.get(name).map(_.name).filter(_.nonEmpty))

  /** The page for `\pageref`: a real (non-zero) folio from either side, preferring the resolved value because a
    * label declared earlier this pass may sit on a page that has not shipped yet.
    */
  def refPage(name: String): Option[Int] =
    resolvedRefs.get(name).map(_.page).filter(_ > 0).orElse(pendingRefs.get(name).map(_.page).filter(_ > 0))

  /** A named contents list as its `\tableofcontents` / `\listoffigures` / `\listoftables` replays it: the previous
    * pass's collection, in document order. An unseen list is empty.
    */
  def list(name: String): Vector[TocEntry] = resolvedLists.getOrElse(name, Vector.empty)

  /** The table of contents `\tableofcontents` replays: the previous pass's `"toc"` collection, in document order. */
  def toc: Vector[TocEntry] = list("toc")

  /** Promote this pass's collection to the resolved set the next pass will read, and report whether it differs
    * from what was resolved before — i.e. whether another pass could still change the output.
    */
  def commit(): Boolean =
    val newRefs  = pendingRefs.toMap
    val newLists = pendingLists.map((k, v) => k -> v.toVector).toMap
    val changed  = newRefs != resolvedRefs || newLists != resolvedLists

    resolvedRefs = newRefs
    resolvedLists = newLists
    pendingRefs.clear()
    pendingLists.clear()
    changed

/** An invisible point in the vertical list that binds a label name to the page it lands on. Like a [[MarkBox]] it
  * migrates out of a paragraph to the enclosing vertical list and draws nothing; when its page ships, the page
  * builder records the label's folio in the [[ReferenceTable]].
  */
class LabelBox(val name: String) extends MigratingBox:
  val isSpace: Boolean = false

  override def toString: String = s"LabelBox($name)"

/** An invisible point that records a contents line for the page it lands on, in the list named by `list` ("toc",
  * "lof", "lot"). Emitted by a sectioning or caption command so the entry's page is the page its heading or float
  * actually starts.
  */
class TocMarkBox(val list: String, val level: Int, val number: String, val title: String) extends MigratingBox:
  val isSpace: Boolean = false

  override def toString: String = s"TocMarkBox($list, $level, $number, $title)"
