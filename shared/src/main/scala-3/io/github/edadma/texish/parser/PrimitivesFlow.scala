package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.*

/** Flow primitives: paragraph and page control (\newpage, \indent, \vfil, \eject), the cross-reference family
  * (\label / \ref / \pageref / \autoref / \nameref), the named contents lists (\tableofcontents and friends),
  * \footnote, and the floating inserts (\topinsert / \midinsert / \botinsert). */
private[parser] def registerFlowPrimitives(proc: Processor, handler: TypesetterHandler): Unit =
  val t = handler.typesetter

  // Simple commands (0 args)
  proc.registerPrimitive("newpage", SimplePrimitive(() => t.newpage()))
  proc.registerPrimitive("noindent", SimplePrimitive(() => t.noindent))
  proc.registerPrimitive("indent", SimplePrimitive(() => t.indent))

  // \leavevmode begins a paragraph if one is not open, respecting the running \indent state — TeX's macro of the
  // same name, made a primitive here. Use it before inline material that would otherwise strand on the vertical
  // list, chiefly a box (a paragraph led by a superscript or an \hbox) where a plain character would have left
  // vertical mode on its own. Horizontal glue and shifted boxes leave vertical mode themselves (see leaveVmode).
  proc.registerPrimitive("leavevmode", SimplePrimitive(() => t.leaveVmode))

  // \strut adds an invisible box one baselineskip tall (and zero wide) to the current line, so the line keeps a
  // regular height and depth no matter its glyphs — TeX's \strut. In vertical mode it begins a paragraph first.
  proc.registerPrimitive("strut", SimplePrimitive(() => { t.leaveVmode; t.add(t.strut) }))

  // \par ends the current paragraph — the explicit form of the blank line that ordinarily breaks a
  // paragraph in the source. It ends the open paragraph (the same break \eject and the vertical glue
  // commands trigger) so a following run starts a fresh one; where no paragraph is open it does nothing.
  // Useful inside a macro, or anywhere a literal blank line would be awkward, as in TeX.
  proc.registerPrimitive("par", SimplePrimitive(() => t.paragraph()))

  // Paragraph writing direction. \rtl sets a right-to-left base — the paragraph flushes right and its
  // words read right to left, with embedded Latin and numbers handled by the bidirectional algorithm —
  // and \ltr restores the default left-to-right base. Like \leftskip, the setting persists until changed.
  proc.registerPrimitive("rtl", SimplePrimitive(() => t.set("pardir", 1.0)))
  proc.registerPrimitive("ltr", SimplePrimitive(() => t.set("pardir", 0.0)))
  proc.registerPrimitive("cr", SimplePrimitive(() => t.op("newLine")))
  proc.registerPrimitive("hfil", SimplePrimitive(() => { t.leaveVmode; t.fil }))
  proc.registerPrimitive("hfill", SimplePrimitive(() => { t.leaveVmode; t.fill }))
  proc.registerPrimitive("hss", SimplePrimitive(() => { t.leaveVmode; t.add(InfGlue) }))

  // \ldots / \dots — a text ellipsis (the … glyph) or, in math, low dots. The math font carries no … glyph
  // (unlike the centred \cdots, which keeps its own), so the low dots are built from three period glyphs spaced a
  // little apart and set as one inner atom. \ldots is always low; \dots is context-sensitive like amsmath's — it
  // sets the centred \cdots when an operator or relation follows (a + \dots + z) and low dots otherwise (a, \dots, z).
  def addLowDots(m: MathMode): Unit =
    def dot = m.mathFont.glyphBox('.'.toInt)
    def gap = HSpaceBox(m.mathFont.size * 2.0 / 18.0)
    m.addNode(MathAtom(MathClass.Inner, HBox(Vector(dot, gap, dot, gap, dot))))

  // Whether the next input token is a binary operator or a relation — the case amsmath's \dots sets with centred
  // dots. The classification reuses the math symbol table: the first character of a text run, or a whole
  // control-sequence symbol, is looked up and its atom class inspected.
  def nextIsBinOrRel(proc: Processor, m: MathMode): Boolean =
    def binRel(node: Option[MathNode]): Boolean = node match
      case Some(a: MathAtom) => a.cls == MathClass.Bin || a.cls == MathClass.Rel
      case _                 => false
    proc.skipSpaces() // a space is insignificant in math, so look past it to the next real token
    proc.hasMoreTokens && (proc.peekToken() match
      case Token.Text(s, _) =>
        binRel(s.dropWhile(_.isWhitespace).headOption.flatMap(c => MathSymbols.charNode(m.mathFont, c.toInt)))
      case Token.ControlSeq(name, _) => binRel(MathSymbols.commandNode(m.mathFont, name))
      case _                         => false)

  proc.registerPrimitive(
    "ldots",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case m: MathMode => addLowDots(m)
          case _           => handler.text("…")
    },
  )
  proc.registerPrimitive(
    "dots",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case m: MathMode => if nextIsBinOrRel(proc, m) then m.addCommand("cdots") else addLowDots(m)
          case _           => handler.text("…")
    },
  )

  // The vertical glue commands end an open paragraph first, as in TeX: \vfill issued mid-paragraph would
  // otherwise add its glue to the paragraph itself, where it sets as horizontal space inside the last line and
  // the vertical list never sees it.
  proc.registerPrimitive(
    "vfil",
    SimplePrimitive(() => {
      t.paragraph()
      t.fil
    }),
  )
  proc.registerPrimitive(
    "vfill",
    SimplePrimitive(() => {
      t.paragraph()
      t.fill
    }),
  )
  proc.registerPrimitive(
    "vss",
    SimplePrimitive(() => {
      t.paragraph()
      t.add(InfGlue)
    }),
  )

  // \nobreak forbids a page break at this point; \eject ends the paragraph and forces one (\vfill\eject fills
  // the rest of the page first). Both are penalties under the hood — see \penalty.
  proc.registerPrimitive("nobreak", SimplePrimitive(() => t.add(Penalty(Penalty.Inhibit))))
  proc.registerPrimitive(
    "eject",
    SimplePrimitive(() => {
      t.paragraph()
      t.add(Penalty(Penalty.Force))
    }),
  )

  // mark - 1 arg: label this point in the document; as pages ship, topmark/firstmark/botmark track which
  // labels each page covers, so a running header can show e.g. "\the\firstmark — \the\botmark"
  proc.registerPrimitive(
    "mark",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.add(MarkBox(Value.display(evalArg(proc, pos))))
    },
  )

  // submark - 1 arg: the same, in a second, independent stream, read back as topsubmark/firstsubmark/botsubmark.
  // Two streams are what a two-sided running head needs: a book marks the chapter with \mark and the section with
  // \submark, and its head then names the chapter on the verso and the section on the recto, as LaTeX's
  // \leftmark and \rightmark do. Neither stream disturbs the other, so a chapter clears the section showing under
  // it by issuing an empty \submark itself.
  proc.registerPrimitive(
    "submark",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.add(MarkBox(Value.display(evalArg(proc, pos)), sub = true))
    },
  )

  // \label - 1 arg: bind a name to the current reference point so a later \ref/\pageref/\autoref/\nameref can name
  // its number, page, kind and title. The reference text is whatever `currentlabel` holds right now — a sectioning
  // command sets it to the section number, just as LaTeX's \refstepcounter sets \@currentlabel — captured here so a
  // counter that steps afterwards does not change it. `currentlabeltype` (the display word \autoref prefixes, e.g.
  // "Section") and `currentlabelname` (the title \nameref prints) are captured the same way, each empty when no
  // surrounding command set it. The page is unknown until shipout, so an invisible LabelBox rides the vertical list
  // to the page the label lands on (see ReferenceTable / PageMode).
  proc.registerPrimitive(
    "label",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        def varText(name: String): String = t.get(name) match
          case Some(v) => Value.display(v)
          case None    => ""

        val name = Value.display(evalArg(proc, pos))

        t.references.declare(name, varText("currentlabel"), varText("currentlabeltype"), varText("currentlabelname"))
        t.add(new LabelBox(name))
    },
  )

  // \ref - 1 arg: print the reference text bound to a label (a section/figure number). Forward references resolve
  // on a later pass; until then, and for a name that was never labelled, it prints "??" — LaTeX's placeholder and
  // the cue to rerun.
  proc.registerPrimitive(
    "ref",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = Value.display(evalArg(proc, pos))
        val text = t.references.refText(name).getOrElse("??")

        proc.setResult(Value.Text(text))
        proc.handler.text(text)
    },
  )

  // \pageref - 1 arg: print the folio of the page a label sits on. Like \ref it shows "??" until a pass has shipped
  // the page that carries the label.
  proc.registerPrimitive(
    "pageref",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = Value.display(evalArg(proc, pos))
        val text = t.references.refPage(name).map(_.toString).getOrElse("??")

        proc.setResult(Value.Text(text))
        proc.handler.text(text)
    },
  )

  // \eqref - 1 arg: print a label's reference text wrapped in parentheses, the form amsmath uses for equation
  // numbers — "(3.2)". Like \ref it shows "(??)" until a forward reference resolves on a later pass.
  proc.registerPrimitive(
    "eqref",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = Value.display(evalArg(proc, pos))
        val text = s"(${t.references.refText(name).getOrElse("??")})"

        proc.setResult(Value.Text(text))
        proc.handler.text(text)
    },
  )

  // \autoref - 1 arg: print a label's reference text with its kind word in front — "Section 3.2", "Figure 1". The
  // kind and number print as one box, so they never split across a line (LaTeX ties them with a non-breaking space).
  // The kind is the `currentlabeltype` captured at \label; when none was set (a bare \label) it falls back to a
  // plain \ref. The number resolves across passes like \ref.
  proc.registerPrimitive(
    "autoref",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = Value.display(evalArg(proc, pos))
        val text = t.references.refText(name).getOrElse("??")
        val out = t.references.refKind(name) match
          case Some(kind) => s"$kind $text"
          case None       => text

        proc.setResult(Value.Text(out))
        proc.handler.text(out)
    },
  )

  // \nameref - 1 arg: print the title bound to a label (the section heading or caption text), not its number. Uses
  // the `currentlabelname` captured at \label, and shows "??" when there is none.
  proc.registerPrimitive(
    "nameref",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = Value.display(evalArg(proc, pos))
        val text = t.references.refName(name).getOrElse("??")

        proc.setResult(Value.Text(text))
        proc.handler.text(text)
    },
  )

  // \addcontentsline - 4 args (list, level, number, title): note this point for a named contents list ("toc", "lof",
  // "lot"). Like a label its page is learned at shipout, via an invisible TocMarkBox; a sectioning or caption command
  // issues one so the entry's folio is the page its heading or float starts on.
  proc.registerPrimitive(
    "addcontentsline",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val list   = Value.display(evalArg(proc, pos))
        val level  = argInt(proc, pos)
        val number = Value.display(evalArg(proc, pos))
        val title  = Value.display(evalArg(proc, pos))

        t.add(new TocMarkBox(list, level, number, title))
    },
  )

  // \tocentry - 3 args (level, number, title): note this point for the table of contents — \addcontentsline into the
  // "toc" list. A sectioning command issues one so the entry's folio is the page its heading starts on.
  proc.registerPrimitive(
    "tocentry",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val level  = argInt(proc, pos)
        val number = Value.display(evalArg(proc, pos))
        val title  = Value.display(evalArg(proc, pos))

        t.add(new TocMarkBox("toc", level, number, title))
    },
  )

  // \tableofcontents / \listoffigures / \listoftables - 0 args: replay the entries collected for one named list on
  // the previous pass. The engine owns the collection and the iteration; the document language owns the look, through
  // a format macro it must define — \tocformat / \lofformat / \lotformat, each called once per entry as
  // macro{level}{number}{title}{page}. On the first pass a list is empty, so this emits nothing and the contents
  // simply appear once the document has been set through.
  def contentsListPrimitive(list: String, formatMacro: String): Primitive =
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val entries = t.references.list(list)

        if entries.nonEmpty then
          val src = entries
            .map(e => s"\\$formatMacro{${e.level}}{${e.number}}{${e.title}}{${e.page}}")
            .mkString

          proc.processContent(src)
    }

  proc.registerPrimitive("tableofcontents", contentsListPrimitive("toc", "tocformat"))
  proc.registerPrimitive("listoffigures", contentsListPrimitive("lof", "lofformat"))
  proc.registerPrimitive("listoftables", contentsListPrimitive("lot", "lotformat"))

  // \contentslist - 2 args (list, format-macro): the general form of \tableofcontents. Replays the entries collected
  // for any named list — the name passed to \addcontentsline — through a format macro named at the call site, invoked
  // once per entry as macro{level}{number}{title}{page}, exactly as \tableofcontents drives \tocformat. A document
  // that keeps several independent contents lists (a separate table of contents per language, say) files into each
  // with \addcontentsline and replays each with its own \contentslist. Like the built-in lists it emits nothing on
  // the first pass, when no entries have been collected yet.
  proc.registerPrimitive(
    "contentslist",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val list        = Value.display(evalArg(proc, pos))
        val formatMacro = Value.display(evalArg(proc, pos))
        val entries     = t.references.list(list)

        if entries.nonEmpty then
          val src = entries
            .map(e => s"\\$formatMacro{${e.level}}{${e.number}}{${e.title}}{${e.page}}")
            .mkString

          proc.processContent(src)
    },
  )

  // \indexlist - 2 args (list, format-macro): the index form of \contentslist. Where \contentslist replays a named
  // list line by line in document order, an index wants the same entries gathered: one line per distinct term,
  // alphabetically, carrying every page the term was written on. So the entries collected for the named list are
  // grouped by title, sorted, and their pages deduplicated and put in ascending order, then the format macro is
  // called once per term as macro{level}{number}{title}{pages} — the same four-argument shape \contentslist uses, so
  // one format macro can serve either — with `pages` the comma-joined folio list and `number` whatever the first
  // entry of the term carried (empty, for an \index that files no number).
  //
  // The sort is by the term folded to lower case, with the term itself breaking ties, so "Zebra" files under Z
  // rather than ahead of "apple" as a raw code-point sort would put it, and two terms differing only in case keep a
  // stable order between them. Like every contents list it reads the previous pass's collection, so an index emits
  // nothing on the first pass.
  proc.registerPrimitive(
    "indexlist",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val list        = Value.display(evalArg(proc, pos))
        val formatMacro = Value.display(evalArg(proc, pos))
        val entries     = t.references.list(list)

        if entries.nonEmpty then
          val terms = entries
            .groupBy(_.title)
            .toVector
            .sortBy((title, _) => (title.toLowerCase, title))

          val src = terms
            .map { (title, es) =>
              val pages = es.map(_.page).distinct.sorted.mkString(", ")
              val level = es.head.level
              val num   = es.head.number

              s"\\$formatMacro{$level}{$num}{$title}{$pages}"
            }
            .mkString

          proc.processContent(src)
    },
  )

  // \marginalnote - 3 args (offset, width, body): set the body in the margin beside the line it was written in. The
  // body is typeset immediately into a vertical box of the given width and rides the vertical list as a zero-size
  // item (see MarginalBox), so the line it annotates is set, and the page broken, exactly as if the note were not
  // there. `offset` is measured from the left edge of the text block — \hsize plus a gap puts the note in the right
  // margin, a negative offset puts it in the left — and an optional `rise:` lifts the note above the point where it
  // lands, a rise of one baseline levelling its first line with the line it annotates. The document language's
  // \marginpar is the everyday form, which is where the outer-margin choice of a two-sided document is made.
  proc.registerPrimitive(
    "marginalnote",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val opts   = proc.readOptionalParams(pos)
        val rise   = opts.get("rise").flatMap(points).getOrElse(0.0)
        val offset = points(evalArg(proc, pos)).getOrElse(0.0)
        val width  = points(evalArg(proc, pos)).getOrElse(0.0)
        val body   = proc.readArgument(pos)

        // typeset now, in its own scope and its own measure, exactly as a float's body is: the note is detached, so
        // neither its font and spacing changes nor a \noindent inside it may flow back into the running text
        val savedIndent = t.indentParagraph
        val savedHsize  = t.getNumber("hsize")

        t.enter()
        t.set("hsize", width)
        t.vbox()
        proc.processTokenList(body)
        t.paragraph()

        val content = t.mode.exit

        t.exit()
        t.set("hsize", savedHsize)
        t.indentParagraph = savedIndent

        if content ne null then t.add(new MarginalBox(content, offset, rise))
    },
  )

  // footnote - an optional [marker] then 1 body arg: a raised marker in the running text, with the body typeset at
  // the foot of whatever page the marker lands on. With no [marker] the note auto-numbers (a running counter in
  // `footnoteno`); with an explicit [marker] that content is the caller instead — a letter, a symbol, an italic
  // sigil — and the counter is left alone, so a document can follow a translation's own scheme (Louis Segond's
  // per-passage italic letters, say) rather than one global sequence. The body is typeset immediately, at
  // footnotesize, into a block that rides the vertical list as a zero-size insert (see InsertBox); the page builder
  // counts its height against the page and moves it below the separator rule at shipout.
  proc.registerPrimitive(
    "footnote",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val markerToks = readBracketTokens(proc)
        val body       = proc.readArgument(pos)
        val auto       = markerToks.isEmpty
        val n          = if auto then t.getNumber("footnoteno").toInt + 1 else 0

        if auto then t.set("footnoteno", n.toDouble)

        val textFont = t.currentFont
        val noteFont = t.makeFont(textFont.typeface, textFont.size * t.getNumber("footnotescale"), textFont.style)

        // Build the marker: the running number when auto, otherwise the given content typeset in the note font.
        // The foot entry opens with the same marker, so `label` reproduces it in the note's own paragraph.
        def markerBox(): Box =
          t.enter()
          t.currentFont = noteFont
          t.hbox(null)
          markerToks match
            case Some(toks) => proc.processTokenList(toks)
            case None       => t.add(t.charBox(n.toString))
          val b = t.mode.exit
          t.exit()
          b

        t.leaveVmode add ShiftBox(markerBox(), -textFont.size / 3)

        // the body is typeset now, into its own vertical box at the footnote size, behind a "N." prefix; the
        // scope brackets the font switch and its dependent spacing so the surrounding text resumes unaffected
        t.enter()
        t.currentFont = noteFont
        t.set("baselineskip", Glue(noteFont.size * t.getNumber("footnoteleading")))
        t.set("spaceskip", interwordGlue(noteFont.space))
        t.set("xspaceskip", xinterwordGlue(noteFont.space))
        // The note is set on its own measure, flush at the foot, whatever the indentation of the line the marker
        // fell on — a marker inside a poetry line, a list item or a quotation must not carry that block's
        // leftskip / indent into the note. The scope above restores them for the surrounding text.
        t.set("leftskip", ZeroGlue)
        t.set("rightskip", ZeroGlue)
        t.set("parindent", 0.0)
        t.set("hangindent", 0.0)
        // Colour is context too: a marker that fell inside coloured text (red words of Jesus, say) must not tint
        // the note at the foot. Reset to the document's own pen — not a fixed black, which would be invisible on
        // a document set in light ink — so the note reads in body colour whatever coloured span the marker sat
        // in. The scope restores the surrounding colour for the text after the marker.
        t.currentColor = t.documentColor
        // A note set solid has no room for a tall glyph: an accented capital on a wrapped line would trip the
        // \lineskip floor and open the line above it, just as it did between notes. Disable the floor inside the
        // note so every line sits exactly a footnote-leading below the last, the accent tucking into the line above.
        t.set("lineskiplimit", -1.0e6)
        t.vbox()
        // the foot entry opens with the same marker as the in-text mark: "N." when auto-numbered, else the caller
        t.noindent
        markerToks match
          case Some(toks) => proc.processTokenList(toks)
          case None       => t.add(t.charBox(s"$n."))
        t.add(t.getGlue("spaceskip"))
        // a footstrut holds the first line to a full footnote-baselineskip height, so its baseline sits the same
        // distance below the separator rule (and below the note before it) whatever the note's opening glyphs are
        t.add(t.strut)
        proc.processTokenList(body)
        t.paragraph()

        val note = t.mode.exit

        t.exit()

        // The note carries its own leading so consecutive footnotes stack a footnote-baselineskip apart, tighter
        // than the body — the same footnoteleading used within a note, not the body's 1.2.
        if note ne null then t.start add InsertBox(note, noteFont.size * t.getNumber("footnoteleading"))
    },
  )

  // topinsert / midinsert / botinsert - an optional [htb] placement spec then 1 body arg: a block (a figure, a
  // table) that detaches from the running text and floats to a page edge. The body is typeset immediately into its
  // own vertical box, which rides the vertical list as a zero-size float (see FloatBox); the page builder counts its
  // height against the page and, at shipout, lifts a top float above the body or sinks a bottom float below the
  // footnotes. The spec (any of h=here, t=top, b=bottom, order significant) overrides the command's default: a
  // here-preferring float stays inline where it sits if the page can still hold it, otherwise falls to its next edge.
  def floatPrimitive(default: String): Primitive =
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val spec = readPlacementSpec(proc).filter(_.nonEmpty).getOrElse(default)
        val body = proc.readArgument(pos)

        // the body is typeset now, into its own vertical box; the scope brackets any font or spacing changes the
        // body makes so the surrounding text resumes unaffected. The indent flag is bracketed too: a float is
        // detached, so a \noindent inside it (a caption opens with one) must not flow out and flush the paragraph
        // that follows the float in the running text.
        val savedIndent = t.indentParagraph
        t.enter()
        t.vbox()
        proc.processTokenList(body)
        t.paragraph()

        val content = t.mode.exit

        t.exit()
        t.indentParagraph = savedIndent

        // contributed to the current list directly: between paragraphs that is the vertical list (a float is a
        // zero-size control item, so no interline glue attaches), and inside a paragraph it rides the line and
        // migrates out to the vertical list with the other migrating items
        if content ne null then t.add(new FloatBox(content, spec.toList))
    }

  proc.registerPrimitive("topinsert", floatPrimitive("t"))
  proc.registerPrimitive("midinsert", floatPrimitive("ht"))
  proc.registerPrimitive("botinsert", floatPrimitive("b"))
