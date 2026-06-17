package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.path.Path
import io.github.edadma.texish.*

/** Register the standard typesetting primitives (\newpage, \hbox, \font, \bold, ...) with a processor.
  *
  * These are the language-level bindings to the typesetting API — the builtin vocabulary any document language gets
  * for free. Applications register their own primitives on top.
  */
def registerTypesettingPrimitives(proc: Processor, handler: TypesetterHandler): Unit =
  val t = handler.typesetter

  // Simple commands (0 args)
  proc.registerPrimitive("newpage", SimplePrimitive(() => t.newpage()))
  proc.registerPrimitive("noindent", SimplePrimitive(() => t.noindent))
  proc.registerPrimitive("indent", SimplePrimitive(() => t.indent))
  proc.registerPrimitive("cr", SimplePrimitive(() => t.op("newLine")))
  proc.registerPrimitive("hfil", SimplePrimitive(() => t.fil))
  proc.registerPrimitive("hfill", SimplePrimitive(() => t.fill))
  proc.registerPrimitive("hss", SimplePrimitive(() => t.add(InfGlue)))

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

  // footnote - 1 body arg: a raised marker number in the running text, with the body typeset at the foot of
  // whatever page the marker lands on. The body is typeset immediately, at footnotesize, into a block that rides
  // the vertical list as a zero-size insert (see InsertBox); the page builder counts its height against the page
  // and moves it below the separator rule at shipout, so reference and footnote always share a page.
  proc.registerPrimitive(
    "footnote",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val body = proc.readArgument(pos)
        val n    = t.getNumber("footnoteno").toInt + 1

        t.set("footnoteno", n.toDouble)

        val textFont = t.currentFont
        val noteFont = t.makeFont(textFont.typeface, textFont.size * t.getNumber("footnotesize"), textFont.style)

        // the marker: the footnote number in the smaller footnote font, raised a third of an em
        t.currentFont = noteFont

        val marker = t.charBox(n.toString)

        t.currentFont = textFont
        t.start add ShiftBox(marker, -textFont.size / 3)

        // the body is typeset now, into its own vertical box at the footnote size, behind a "N." prefix; the
        // scope brackets the font switch and its dependent spacing so the surrounding text resumes unaffected
        t.enter()
        t.currentFont = noteFont
        t.set("baselineskip", Glue(noteFont.size * 1.2))
        t.set("spaceskip", Glue(noteFont.space, 1))
        t.set("xspaceskip", Glue(noteFont.space * 1.5, 1))
        t.vbox()
        t.noindent add t.charBox(s"$n.")
        t.add(t.getGlue("spaceskip"))
        proc.processTokenList(body)
        t.paragraph()

        val note = t.mode.exit

        t.exit()

        if note ne null then t.start add InsertBox(note)
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
        // body makes so the surrounding text resumes unaffected
        t.enter()
        t.vbox()
        proc.processTokenList(body)
        t.paragraph()

        val content = t.mode.exit

        t.exit()

        // contributed to the current list directly: between paragraphs that is the vertical list (a float is a
        // zero-size control item, so no interline glue attaches), and inside a paragraph it rides the line and
        // migrates out to the vertical list with the other migrating items
        if content ne null then t.add(new FloatBox(content, spec.toList))
    }

  proc.registerPrimitive("topinsert", floatPrimitive("t"))
  proc.registerPrimitive("midinsert", floatPrimitive("ht"))
  proc.registerPrimitive("botinsert", floatPrimitive("b"))

  // penalty - 1 numeric arg: how undesirable a page break is here (10000 forbids, -10000 forces)
  proc.registerPrimitive(
    "penalty",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        evalArg(proc, pos) match
          case Value.Num(n) => t.add(Penalty(n.toInt))
          case _            => handler.error("\\penalty expects a number", pos)
    },
  )

  // loadhyphenation - 2 braced args: language name and path to pattern file
  proc.registerPrimitive(
    "loadhyphenation",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val lang = evalArg(proc, pos)
        val path = evalArg(proc, pos)
        (lang, path) match
          case (Value.Text(l), Value.Text(p)) => Hyphenation.loadPatterns(l, p)
          case _                              => handler.error("\\loadhyphenation expects {language}{path}", pos)
    },
  )

  // language - 1 braced arg: switch active hyphenation language
  proc.registerPrimitive(
    "language",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val arg = evalArg(proc, pos)
        arg match
          case Value.Text(lang) => Hyphenation.setLanguage(lang)
          case _                => handler.error("\\language expects a language name", pos)
    },
  )

  // typeface - 1 braced arg
  proc.registerPrimitive(
    "typeface",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val arg = evalArg(proc, pos)
        arg match
          case Value.Text(typeface) =>
            val font = t.typeface(typeface)
            // Update spaceskip based on new font's space width (typesetter doesn't do this automatically)
            t.set("spaceskip", Glue(font.space, 1))
            t.set("xspaceskip", Glue(font.space * 1.5, 1))
          case _ => handler.error("\\typeface expects a typeface name", pos)
    },
  )

  // font - 3 braced args
  proc.registerPrimitive(
    "font",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val typeface = evalArg(proc, pos)
        val size     = evalArg(proc, pos)
        val style    = evalArg(proc, pos)
        (typeface, size, style) match
          case (Value.Text(tf), Value.Num(sz), Value.Text(st)) =>
            val font = t.selectFont(tf, sz.toDouble, st.split("\\s+").toSet)
            // Update spaceskip based on new font's space width (typesetter doesn't do this automatically)
            t.set("spaceskip", Glue(font.space, 1))
            t.set("xspaceskip", Glue(font.space * 1.5, 1))
          case _ => handler.error("\\font expects <typeface> <size> <style>", pos)
    },
  )

  // image - 1 arg
  proc.registerPrimitive(
    "image",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val arg = evalArg(proc, pos)
        arg match
          case Value.Text(path) => t.image(resolveImagePath(proc, path))
          case _                => handler.error("\\image expects a path", pos)
    },
  )

  // includegraphics [width=…,height=…,scale=…] {path} — place a raster image, sized by an optional LaTeX-style
  // key=value list. width and height are lengths (a dimension like 200pt, or a factor times \linewidth /
  // \textwidth, the current line width); giving only one scales the other to keep the aspect ratio. scale
  // multiplies the natural size. With no options the image is placed at its natural pixel size.
  proc.registerPrimitive(
    "includegraphics",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val (width, height, scale) = parseGraphicsOptions(proc, t, proc.readOptionalArg(pos), pos)
        evalArg(proc, pos) match
          case Value.Text(path) => t.image(resolveImagePath(proc, path), width, height, scale)
          case _                => handler.error("\\includegraphics expects a path", pos)
    },
  )

  // href {uri} {text} - a hyperlink: typeset the display text (blue, like hyperref's coloured links) and wrap
  // it so that, in PDF output, the drawn region becomes a clickable annotation pointing at uri. The uri is read
  // verbatim — it may contain // (otherwise a comment), ~, % and other specials — while the text is ordinary
  // markup. On a backend without annotations the text just draws blue.
  proc.registerPrimitive(
    "href",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val uri  = proc.readRawArgument(pos)
        val body = proc.readArgument(pos)
        handler.flushPendingSpace()
        t.hbox(null)
        t.enter()
        t.currentColor = Color("blue")
        proc.processTokenList(body)
        t.exit()
        val box = t.mode.exit
        if box ne null then handler.addBox(new LinkBox(box, uri))
    },
  )

  // url {uri} - typeset the URL itself, in the monospaced face and blue, as a link to itself. The argument is
  // read verbatim and set literally (no ligatures, no re-tokenizing), so the address appears exactly as written.
  proc.registerPrimitive(
    "url",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val uri = proc.readRawArgument(pos)
        handler.flushPendingSpace()
        t.hbox(null)
        t.enter()
        val font = selectMatchedMono(t)
        t.set("spaceskip", Glue(font.space, 1))
        t.set("xspaceskip", Glue(font.space * 1.5, 1))
        t.currentColor = Color("blue")
        t.add(t.charBox(uri))
        t.exit()
        val box = t.mode.exit
        if box ne null then handler.addBox(new LinkBox(box, uri))
    },
  )

  // color name - set the pen colour for the text, rules and glyphs that follow in the current group; the colour
  // reverts at the group's close, exactly as \font does (both are saved on enter and restored on exit). The name
  // is a CSS colour word (blue, darkred, …) or a #RRGGBB hex code.
  proc.registerPrimitive(
    "color",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        evalArg(proc, pos) match
          case Value.Text(name) => t.currentColor = Color(name)
          case _                => handler.error("\\color expects a colour name or #RRGGBB code", pos)
    },
  )

  // textcolor name body - set the pen colour for just its body, which is typeset in its own group so the colour
  // reverts immediately after. \textcolor{blue}{link} is the local form of \color.
  proc.registerPrimitive(
    "textcolor",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        evalArg(proc, pos) match
          case Value.Text(name) =>
            val body = proc.readArgument(pos)
            t.enter()
            t.currentColor = Color(name)
            proc.processTokenList(body)
            t.exit()
          case _ => handler.error("\\textcolor expects a colour name or #RRGGBB code", pos)
    },
  )

  // vskip - glue spec: dimension with optional plus/minus continuation, braced glue, or glue variable; like the
  // other vertical glue commands it ends an open paragraph first
  proc.registerPrimitive(
    "vskip",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val argPos = argumentPos(proc, pos)
        glueArg(proc, pos) match
          case Some(g) =>
            t.paragraph()
            t.add(g)
          case None => handler.error("\\vskip expects a dimension or glue", argPos)
    },
  )

  // hskip - glue spec: dimension with optional plus/minus continuation, braced glue, or glue variable
  proc.registerPrimitive(
    "hskip",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val argPos = argumentPos(proc, pos)
        glueArg(proc, pos) match
          case Some(g) => t.add(g)
          case None    => handler.error("\\hskip expects a dimension or glue", argPos)
    },
  )

  // hrule - 0 args but optional params. With no width: it runs to the width of the box it lands in (the table
  // width inside an \halign's \noalign; otherwise the hsize fallback). With width: that exact width.
  proc.registerPrimitive(
    "hrule",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val opts          = proc.readOptionalParams(pos)
        val explicitWidth = opts.get("width").flatMap(points)
        val ascent        = opts.get("ascent").flatMap(points).getOrElse(3.0)
        val descent       = opts.get("descent").flatMap(points).getOrElse(0.0)
        t.add(
          new RuleBox(t, explicitWidth.getOrElse(t.getNumber("hsize")), ascent, descent, t.currentColor,
            running = explicitWidth.isEmpty),
        )
    },
  )

  // vrule - 0 args but optional params. With no height/depth: a vertical rule that runs to the height of the line
  // it sits in (so a \vrule in a table cell spans that cell). With height:/depth:: a rule of exactly that size.
  // width: sets the rule's thickness (default 0.4pt, as in TeX).
  proc.registerPrimitive(
    "vrule",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val opts      = proc.readOptionalParams(pos)
        val thickness = opts.get("width").flatMap(points).getOrElse(0.4)
        val height    = opts.get("height").flatMap(points)
        val depth     = opts.get("depth").flatMap(points)
        if height.isDefined || depth.isDefined then
          t.add(new RuleBox(t, thickness, height.getOrElse(0.0), depth.getOrElse(0.0)))
        else t.add(new VRule(t, thickness))
    },
  )

  // hbox / vbox / vtop - typeset a braced body into a horizontal box, a vertical box, or a vertical box whose
  // reference point is the first line's baseline (\vtop). An optional `to:` sets the final size and `spread:`
  // adds to the natural size; glue stretches or shrinks to fill. The box is added to the current list.
  proc.registerPrimitive(
    "hbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        buildBox(proc, t, vertical = false, top = false, pos) match
          case b: Box => t.add(b)
          case null   =>
    },
  )
  proc.registerPrimitive(
    "vbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        buildBox(proc, t, vertical = true, top = false, pos) match
          case b: Box => t.add(b)
          case null   =>
    },
  )
  proc.registerPrimitive(
    "vtop",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        buildBox(proc, t, vertical = true, top = true, pos) match
          case b: Box => t.add(b)
          case null   =>
    },
  )

  // setbox name \hbox{...} (or \vbox / \vtop) - typeset a box now and save it in a register under `name`, for
  // later measurement (\wd / \ht / \dp) and placement (\box / \copy). Like \set, the assignment is local to the
  // current group. The box's contents are typeset at this point, not when the register is later used.
  proc.registerPrimitive(
    "setbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = proc.readIdentifier(pos)
        readBoxArg(proc, t, pos) match
          case b: Box => proc.handler.set(name, Value.Native(b))
          case null   => handler.error("\\setbox expects a box (\\hbox, \\vbox, or \\vtop)", argumentPos(proc, pos))
    },
  )

  // box name - place the saved box into the current list and empty the register (the box is "used up", as in
  // TeX). copy name - place the box but leave the register intact for reuse. Boxes are immutable, so the copy
  // shares the same instance.
  proc.registerPrimitive(
    "box",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = proc.readIdentifier(pos)
        t.add(boxRegister(proc, handler, name, "box", pos))
        proc.handler.set(name, Value.Undefined)
    },
  )
  proc.registerPrimitive(
    "copy",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = proc.readIdentifier(pos)
        t.add(boxRegister(proc, handler, name, "copy", pos))
    },
  )

  // unhbox name / unvbox name - splice the saved box's contents directly into the current list (rather than
  // nesting the box itself), then empty the register. unhbox requires an \hbox register, unvbox a \vbox.
  proc.registerPrimitive(
    "unhbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = proc.readIdentifier(pos)
        boxRegister(proc, handler, name, "unhbox", pos) match
          case hb: HBox => hb.boxes.foreach(t.add)
          case _        => handler.error(s"\\unhbox: '$name' is not an \\hbox", pos)
        proc.handler.set(name, Value.Undefined)
    },
  )
  proc.registerPrimitive(
    "unvbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = proc.readIdentifier(pos)
        boxRegister(proc, handler, name, "unvbox", pos) match
          case vb: VerticalBox => vb.boxes.foreach(t.add)
          case _               => handler.error(s"\\unvbox: '$name' is not a \\vbox or \\vtop", pos)
        proc.handler.set(name, Value.Undefined)
    },
  )

  // wd / ht / dp name - the width, height (above the baseline), and depth (below the baseline) of a saved box,
  // each a dimension. They feed any primitive that takes a dimension — \kern\wd title (a rigid space as wide as
  // the box), \hbox to:{\wd ref}{...} (match another box's width) — and, stored first with \set, into \calc:
  // \set h {\ht a} then \calc{h + 2}. ht maps to the box's ascent and dp to its descent, matching TeX's
  // reference-point split of the total height.
  proc.registerPrimitive(
    "wd",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = proc.readIdentifier(pos)
        proc.setResult(Value.Dimen(boxRegister(proc, handler, name, "wd", pos).width))
    },
  )
  proc.registerPrimitive(
    "ht",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = proc.readIdentifier(pos)
        proc.setResult(Value.Dimen(boxRegister(proc, handler, name, "ht", pos).ascent))
    },
  )
  proc.registerPrimitive(
    "dp",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = proc.readIdentifier(pos)
        proc.setResult(Value.Dimen(boxRegister(proc, handler, name, "dp", pos).descent))
    },
  )

  // kern - a rigid horizontal space of the given dimension (may be negative), e.g. \kern-.1667em
  proc.registerPrimitive(
    "kern",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val argPos = argumentPos(proc, pos)
        points(proc.evalArgumentExpr(pos)) match
          case Some(d) => t.add(HSpaceBox(d))
          case None    => handler.error("\\kern expects a dimension", argPos)
    },
  )

  // lower / raise - shift the following box (an \hbox or \vbox) down / up by a dimension, e.g.
  // \lower.5ex\hbox{E}. The box keeps its own width and height; only where it draws moves.
  proc.registerPrimitive(
    "lower",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val argPos = argumentPos(proc, pos)
        points(proc.evalArgumentExpr(pos)) match
          case Some(d) =>
            readBoxArg(proc, t, pos) match
              case b: Box => t.add(ShiftBox(b, d))
              case null   => handler.error("\\lower expects a box (\\hbox or \\vbox)", argumentPos(proc, pos))
          case None => handler.error("\\lower expects a dimension", argPos)
    },
  )
  proc.registerPrimitive(
    "raise",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val argPos = argumentPos(proc, pos)
        points(proc.evalArgumentExpr(pos)) match
          case Some(d) =>
            readBoxArg(proc, t, pos) match
              case b: Box => t.add(ShiftBox(b, -d))
              case null   => handler.error("\\raise expects a box (\\hbox or \\vbox)", argumentPos(proc, pos))
          case None => handler.error("\\raise expects a dimension", argPos)
    },
  )

  registerMathPrimitives(proc, handler)

  // bold - 1 body arg
  proc.registerPrimitive(
    "bold",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val body = proc.readArgument(pos)
        t.bold()
        proc.processTokenList(body) // scoping happens automatically from { } tokens
        t.nobold()
    },
  )

  // italic - 1 body arg
  proc.registerPrimitive(
    "italic",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val body = proc.readArgument(pos)
        t.italic()
        proc.processTokenList(body) // scoping happens automatically from { } tokens
        t.noitalic()
    },
  )

  // smallcaps - 1 body arg
  proc.registerPrimitive(
    "smallcaps",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val body = proc.readArgument(pos)
        t.smallcaps()
        proc.processTokenList(body) // scoping happens automatically from { } tokens
        t.nosmallcaps()
    },
  )

  // texttt - 1 body arg: set its content in the monospaced face (JetBrains Mono), for inline code, file names,
  // URLs and the like. The mono size is matched to the surrounding font by x-height (see selectMatchedMono), so
  // the code does not loom larger than the body text. The switch is scoped, so the surrounding text resumes in
  // the body font afterwards; the interword space is widened to the mono font's fixed space for the duration.
  proc.registerPrimitive(
    "texttt",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val body = proc.readArgument(pos)
        t.enter()
        val font = selectMatchedMono(t)
        t.set("spaceskip", Glue(font.space, 1))
        t.set("xspaceskip", Glue(font.space * 1.5, 1))
        proc.processTokenList(body)
        t.exit()
    },
  )

  // underline - 1 body arg (wraps content in underline)
  proc.registerPrimitive(
    "underline",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val body = proc.readArgument(pos)
        // Create an hbox to capture the content
        handler.flushPendingSpace()
        t.hbox(null)
        proc.processTokenList(body) // scoping happens automatically from { } tokens
        val box = t.mode.exit
        if box ne null then handler.addBox(new UnderlineBox(t, box))
    },
  )

  // Running headers and footers: if the document defines a headline or footline macro, each shipped page builds
  // an hbox to hsize from its body at shipout time — pageno is already set to the shipping page's number, so
  // \the\pageno in the macro is always current. The hbox is built on a temporary mode pushed over whatever is
  // being typeset and popped with exit (not done), so the box never lands in the page being broken; the handler's
  // pending-newline state is isolated the same way, so a newline pending in the document doesn't put a stray
  // space at the front of the header.
  t.pageDecorator = () =>
    def line(name: String): Box | Null =
      t.get(name) match
        case Some(Value.Macro(_, body, _)) =>
          handler.isolated {
            t.hbox(t.getNumber("hsize"))
            proc.processTokenList(body)
            t.mode.exit
          }
        case _ => null

    (line("headline"), line("footline"))

  // Active characters

  // $ toggles math mode, as in TeX. A single $ delimits inline math; a doubled $$ delimits a display, set on
  // its own centred line. A second $ immediately following is recognised here and consumed, so the doubled
  // delimiter reads as one token to the rest of the machinery — opening or closing a display either way.
  proc.registerActive(
    '$',
    new Active {
      def execute(proc: Processor, c: Char, pos: CharReader): Unit =
        val display = proc.peekToken() match
          case Token.Active('$', _) => proc.nextToken(); true
          case _                    => false

        handler.toggleMath(display)
    },
  )

  // ^ and _ attach a superscript / subscript to the preceding math atom. They matter only inside $…$; in
  // ordinary text they stand for themselves, so a document that uses ^ or _ in prose is unaffected.
  proc.registerActive(
    '^',
    new Active {
      def execute(proc: Processor, c: Char, pos: CharReader): Unit = handler.mathScript(proc, superscript = true, pos)
    },
  )
  proc.registerActive(
    '_',
    new Active {
      def execute(proc: Processor, c: Char, pos: CharReader): Unit = handler.mathScript(proc, superscript = false, pos)
    },
  )

  // # and & are alignment-active only when an \halign is the current mode: # marks the template placeholder,
  // & separates columns. Anywhere else (ordinary prose, an \hbox, a picture) they are literal characters, so a
  // document can write "AT&T" or "C#" without escaping. \# and \& force the literal even inside a table.
  proc.registerActive(
    '#',
    new Active {
      def execute(proc: Processor, c: Char, pos: CharReader): Unit =
        t.mode match
          case _: HAlignMode => t.op("placeholder")
          case _             => handler.text(c.toString)
    },
  )

  proc.registerActive(
    '&',
    new Active {
      def execute(proc: Processor, c: Char, pos: CharReader): Unit =
        t.mode match
          case _: HAlignMode => t.op("newColumn")
          case _             => handler.text(c.toString)
    },
  )

  registerPictureGraphicsPrimitives(proc, handler)

// Helper class for simple 0-arg commands
class SimplePrimitive(action: () => Any) extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit = action()

// Helper to evaluate an argument and get its value
private[parser] def evalArg(proc: Processor, pos: CharReader): Value =
  proc.evalArgumentExpr(pos)

// Build the TeX (or TeXish) logo in the current text font: a T, an E lowered half an x-height and kerned
// A dimension value in big points: Dimen carries its own unit; a bare number means points
private[parser] def points(v: Value): Option[Double] = v match
  case Value.Dimen(p) => Some(p.toDouble)
  case Value.Num(n)   => Some(n.toDouble)
  case _              => None

// Resolve an image path the way \use resolves a module: an absolute path is used as given, a relative one is
// taken relative to the directory of the document being processed, so \includegraphics{frog.jpg} finds the
// image beside the source file rather than relative to the working directory.
private[parser] def resolveImagePath(proc: Processor, path: String): String =
  val p = Path(path)
  if p.isAbsolute then path else (Path(proc.currentDir) / p).toString

// Select the monospaced face at a size whose x-height matches the surrounding font, and return it as the new
// current font. A mono font set at the body's nominal point size looks oversized because its x-height is larger
// per em; scaling by the x-height ratio makes inline code sit at the visual size of the text around it.
private[parser] def selectMatchedMono(t: Typesetter): Font =
  val base       = t.currentFont
  val monoAtBase = t.makeFont("mono", base.size, Set.empty)
  val scale      = if monoAtBase.xHeight > 0 then base.xHeight / monoAtBase.xHeight else 1.0
  t.selectFont("mono", base.size * scale, Set.empty)

// Parse \includegraphics's optional [width=…,height=…,scale=…] list. The bracket tokens are flattened back to
// their source text (a captured \linewidth survives as the literal "\linewidth"), split on commas into
// key=value entries, and each value resolved as a length or a scale factor. An absent list yields all-None.
private[parser] def parseGraphicsOptions(
    proc: Processor,
    t: Typesetter,
    opts: Option[Vector[Token]],
    pos: CharReader,
): (Option[Double], Option[Double], Option[Double]) =
  opts match
    case None => (None, None, None)
    case Some(tokens) =>
      val raw = tokens.map {
        case Token.Text(s, _)       => s
        case Token.ControlSeq(n, _) => "\\" + n
        case Token.Space(_, _)      => " "
        case Token.Newline(_)       => " "
        case _                      => ""
      }.mkString
      var width: Option[Double]  = None
      var height: Option[Double] = None
      var scale: Option[Double]  = None
      for entry <- raw.split(",") if entry.trim.nonEmpty do
        val eq = entry.indexOf('=')
        if eq < 0 then proc.handler.error(s"\\includegraphics option '${entry.trim}' must be key=value", pos)
        val key   = entry.substring(0, eq).trim
        val value = entry.substring(eq + 1).trim
        key match
          case "width"  => width = Some(resolveGraphicsLength(proc, t, value, pos))
          case "height" => height = Some(resolveGraphicsLength(proc, t, value, pos))
          case "scale" =>
            scale = Some(value.toDoubleOption.getOrElse(proc.handler.error(s"\\includegraphics: scale '$value' is not a number", pos)))
          case other => proc.handler.error(s"\\includegraphics: unknown option '$other'", pos)
      (width, height, scale)

// Resolve an \includegraphics length: a factor times \linewidth or \textwidth (both the current line width,
// the bare command meaning factor 1), or an ordinary dimension like 200pt / 5cm / 0.5in.
private[parser] def resolveGraphicsLength(proc: Processor, t: Typesetter, s: String, pos: CharReader): Double =
  def factorTimesLineWidth(suffix: String): Double =
    val f = s.dropRight(suffix.length).trim
    val factor =
      if f.isEmpty then 1.0
      else f.toDoubleOption.getOrElse(proc.handler.error(s"\\includegraphics: '$f' is not a number", pos))
    factor * t.getNumber("hsize")

  if s.endsWith("\\linewidth") then factorTimesLineWidth("\\linewidth")
  else if s.endsWith("\\textwidth") then factorTimesLineWidth("\\textwidth")
  else
    parseDimension(s, proc.handler.fontUnit) match
      case Some(Value.Dimen(p)) => p
      case _                    => proc.handler.error(s"\\includegraphics: '$s' is not a length", pos)

// Build an \hbox or \vbox whose command token has already been consumed: read its optional `to:` target
// and braced body, typeset the body into a fresh builder, and return the finished box *without* adding it
// to the current list. Shared by the \hbox / \vbox / \vtop / \setbox primitives and \lower / \raise. `vertical`
// selects a vertical builder, `top` makes it a \vtop (reference point on the first line). The optional `to:` sets
// the final size and `spread:` adds to the natural size; at most one may be given.
private[parser] def buildBox(proc: Processor, t: Typesetter, vertical: Boolean, top: Boolean, pos: CharReader): Box | Null =
  val opts = proc.readOptionalParams(pos)
  val body = proc.readArgument(pos)
  // build Double | Null directly — boxing through java.lang.Double would unbox null to 0.0
  val toVal: Double | Null = opts.get("to").flatMap(points) match
    case Some(d) => d
    case None    => null
  val spreadVal: Double | Null = opts.get("spread").flatMap(points) match
    case Some(d) => d
    case None    => null
  if toVal != null && spreadVal != null then proc.handler.error("a box takes either to: or spread:, not both", pos)
  if vertical then (if top then t.vtop(toVal, spreadVal) else t.vbox(toVal, spreadVal)) else t.hbox(toVal, spreadVal)
  proc.processTokenList(body) // scoping happens automatically from { } tokens
  t.paragraph() // close any paragraph the body opened in vertical mode, so exit sees the box builder itself
  t.mode.exit

// Read the <box> that follows \lower / \raise / \setbox — the next \hbox, \vbox, or \vtop, built and returned
// without adding it to the current list, so the caller can wrap, shift, or store it. Returns null for
// anything that is not a box command.
private[parser] def readBoxArg(proc: Processor, t: Typesetter, pos: CharReader): Box | Null =
  proc.skipSpaces()
  if !proc.hasMoreTokens then null
  else
    proc.peekToken() match
      case Token.ControlSeq(name, _) if name == "hbox" || name == "vbox" || name == "vtop" =>
        proc.nextToken() // consume the box command
        buildBox(proc, t, vertical = name != "hbox", top = name == "vtop", pos)
      case _ => null

// Fetch a box stored in a register by \setbox. Errors (rather than returning a sentinel) when the register
// is empty or holds a non-box, so a misused box command points at the offending name.
private[parser] def boxRegister(proc: Processor, handler: TypesetterHandler, name: String, cmd: String, pos: CharReader): Box =
  proc.handler.get(name) match
    case Value.Native(b: Box) => b
    case Value.Undefined      => handler.error(s"\\$cmd: box register '$name' is empty", pos)
    case _                    => handler.error(s"\\$cmd: '$name' is not a box", pos)

// Resolve a glue argument: a braced glue spec ({12pt plus 2pt}), a glue-valued variable, or a bare dimension
// optionally continued by `plus`/`minus` keywords in the token stream (\vskip 12pt plus 2pt minus 1fil)
// The position of the next argument token (after any intervening spaces), or the command's own
// position when nothing follows. An "expects an argument" error reports this, so the caret points
// at the offending argument rather than at the start of the command.
private[parser] def argumentPos(proc: Processor, command: CharReader): CharReader =
  proc.skipSpaces()
  if proc.hasMoreTokens then Token.pos(proc.peekToken()) else command

private[parser] def glueArg(proc: Processor, pos: CharReader): Option[Glue] =
  proc.evalArgumentExpr(pos) match
    case Value.Glue(n, st, sh, sto, sho) => Some(Glue(n, st, sh, sto, sho))
    case Value.Native(g: Glue)           => Some(g)
    case Value.Dimen(p)                  => Some(glueContinuation(proc, p, pos))
    case Value.Num(n)                    => Some(glueContinuation(proc, n, pos))
    case _                               => None

private[parser] def glueContinuation(proc: Processor, natural: Double, pos: CharReader): Glue =
  proc.readGlueContinuation(natural, pos) match
    case Value.Glue(n, st, sh, sto, sho) => Glue(n, st, sh, sto, sho)
    case _                               => Glue(natural)

// Read an optional [htb] placement specifier following a float command, in the style of \sqrt's [degree]. Brackets
// are ordinary text, so the opening '[' may begin a text token and the closing ']' may sit mid-token; this scans
// across tokens, splitting text runs at the brackets and pushing back any tail after ']'. The result keeps only the
// placement letters h/t/b, in order. Returns None when no '[' run follows, leaving the stream untouched.
private[parser] def readPlacementSpec(proc: Processor): Option[String] =
  proc.skipSpaces()
  proc.peekToken() match
    case Token.Text(s, sp) if s.startsWith("[") =>
      proc.nextToken()
      val out    = new StringBuilder
      var closed = false

      def takeText(str: String, p: CharReader): Unit =
        val idx = str.indexOf(']')
        if idx < 0 then out ++= str
        else
          out ++= str.substring(0, idx)
          val after = str.substring(idx + 1)
          if after.nonEmpty then proc.pushBack(Vector(Token.Text(after, p)))
          closed = true

      takeText(s.substring(1), sp)
      while !closed && proc.hasMoreTokens do
        proc.nextToken() match
          case Token.Text(str, p) => takeText(str, p)
          case Token.EOF(_)       => closed = true
          case _                  => ()
      Some(out.toString.filter("htb".contains(_)))
    case _ => None
