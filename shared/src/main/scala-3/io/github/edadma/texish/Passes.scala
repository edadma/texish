package io.github.edadma.texish

/** Drives the repeated typesetting passes a document needs to resolve its forward cross-references.
  *
  * A `\ref` to a later `\label`, or a `\tableofcontents` to the sections that follow it, cannot be filled in while
  * the document is read once top to bottom — so the document is set more than once over a single shared
  * [[ReferenceTable]], each pass reading what the previous one resolved. A document with no cross-references
  * settles after one pass; a table of contents that grows a page can shift every folio after it, so more than two
  * passes are occasionally needed (LaTeX's "rerun to get cross-references right"). The cap bounds the rare run that
  * never quite settles.
  */
object Passes:

  /** Typeset a document until its cross-references stop moving, returning the typesetter whose output is final.
    *
    * `make` builds and configures a fresh typesetter (paper size, dpi, output target) for each pass; `run` feeds it
    * the document — register primitives, process the source, end. Each pass shares the one reference table, so a
    * forward reference resolved on one pass is visible on the next. Intermediate typesetters are discarded; the
    * returned one is the caller's to read from and destroy.
    */
  def untilStable(maxPasses: Int = 4)(make: () => Typesetter)(run: Typesetter => Unit): Typesetter =
    require(maxPasses >= 1, "a document needs at least one pass")

    val refs = new ReferenceTable

    var current = make()
    current.references = refs
    run(current)

    var n = 1

    while refs.commit() && n < maxPasses do
      current.discard()
      current = make()
      current.references = refs
      run(current)
      n += 1

    current
