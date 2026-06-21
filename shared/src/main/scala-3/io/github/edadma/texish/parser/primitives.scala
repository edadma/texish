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

  // Expose whether this backend can render images, so a document can choose an inline bitmap (\defbitmap) when it
  // can and a drawn fallback when it cannot (e.g. the JS layer).
  t.set("imagessupported", if t.imagesSupported then 1.0 else 0.0)

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

  // fontsize - 1 braced arg: change only the type size, keeping the current typeface and shape/series. Unlike
  // \font, which re-selects all three, this is the size knob the LaTeX-style size declarations (\small, \large,
  // \Large …) are built on, so a size change inside bold or italic text keeps that style. selectFont also resets
  // \baselineskip to TeX's 1.2 × size default leading; a document that wants a different leading sets it after.
  proc.registerPrimitive(
    "fontsize",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        evalArg(proc, pos) match
          case Value.Num(sz) =>
            val cur  = t.currentFont
            val font = t.selectFont(cur.typeface, sz.toDouble, cur.style)
            t.set("spaceskip", Glue(font.space, 1))
            t.set("xspaceskip", Glue(font.space * 1.5, 1))
          case _ => handler.error("\\fontsize expects a number", pos)
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

  // defbitmap {name} {width} {height} {depth} {base64} — define an inline raster image from data embedded in the
  // source, so a package can carry a glyph (a clef, a logo) with no external file. The pixels are alpha only,
  // black where opaque: `depth` bits per pixel (1 for crisp line art, 2/4/8 for antialiased grey levels), packed
  // MSB-first row-major and base64-encoded. The image is built once and stored under name for \usebitmap. On a
  // backend without image support it is silently skipped (the name stays undefined), so a document can fall back.
  proc.registerPrimitive(
    "defbitmap",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name  = Value.display(evalArg(proc, pos))
        val width  = argInt(proc, pos)
        val height = argInt(proc, pos)
        val depth  = argInt(proc, pos)
        // the data is read verbatim — base64 is not markup, and an expression read would drop characters and
        // could trip over // (a comment) or other specials, as a URL does in \href
        val data = proc.readRawArgument(pos)
        if t.imagesSupported then
          val argb   = unpackBitmapAlpha(base64Decode(data), width, height, depth)
          val handle = t.createImage(width, height, argb)
          proc.handler.set(name, Value.Native(InlineBitmap(handle, width, height)))
    },
  )

  // usebitmap [width:… height:…] {name} — place a bitmap defined by \defbitmap, sized like \includegraphics
  // (one of width/height keeps the aspect ratio). An undefined name places nothing, so an image-less backend
  // simply shows whatever the document drew instead.
  proc.registerPrimitive(
    "usebitmap",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val opts = proc.readOptionalParams(pos)
        val reqW = opts.get("width").flatMap(points)
        val reqH = opts.get("height").flatMap(points)
        val name = Value.display(evalArg(proc, pos))
        proc.handler.get(name) match
          case Value.Native(b: InlineBitmap) =>
            t.add(new HandleImageBox(t, b.handle, b.width, b.height, reqW, reqH, None))
          case _ =>
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
        val font = t.mono()
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

  // \discretionary{pre}{post}{no-break} — the author's break point. If the line breaker breaks here, `pre` ends
  // the line and `post` opens the next; otherwise `no-break` shows in place. Compound words, breakable URLs, and
  // respelled hyphenations all reduce to this.
  proc.registerPrimitive(
    "discretionary",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val pre     = typesetGroupBoxes(proc, t, pos)
        val post    = typesetGroupBoxes(proc, t, pos)
        val noBreak = typesetGroupBoxes(proc, t, pos)
        t.add(new DiscretionaryBox(pre, post, noBreak))
    },
  )

  // \softhyphen — a discretionary hyphen: a break point that, if taken, ends the line with a hyphen in the
  // current font and nothing on the next line. It is \discretionary{-}{}{} with the hyphen drawn from the active
  // face. (TeX spells this \-, but texish reserves \- for subtraction, so the command is named in full.)
  proc.registerPrimitive(
    "softhyphen",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.add(new DiscretionaryBox(Seq(new CharBox(t, "-")), Seq.empty, Seq.empty))
    },
  )

  // Leaders fill a flexible space with repeated copies of a box instead of leaving it blank — TeX's \leaders
  // (\cleaders, \xleaders) family. Each reads the unit box (the next \hbox/\vbox) then a glue spec giving the
  // space to fill (usually a fil-stretch glue so it spans whatever slack the line has). \leaders aligns copies to
  // a page-anchored grid, \cleaders centres them, \xleaders spreads the leftover space between them.
  def leaderPrimitive(kind: LeaderKind): Primitive =
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        readBoxArg(proc, t, pos) match
          case b: Box =>
            val argPos = argumentPos(proc, pos)
            glueArg(proc, pos) match
              case Some(g) =>
                t.add(new LeaderGlue(b, kind, g.naturalSize, g.stretch, g.shrink, g.stretchOrder, g.shrinkOrder))
              case None => handler.error("leaders expect a glue after the unit box", argPos)
          case null =>
            handler.error("leaders expect a box (\\hbox or \\vbox) followed by a glue", argumentPos(proc, pos))
    }
  proc.registerPrimitive("leaders", leaderPrimitive(LeaderKind.Aligned))
  proc.registerPrimitive("cleaders", leaderPrimitive(LeaderKind.Centered))
  proc.registerPrimitive("xleaders", leaderPrimitive(LeaderKind.Expanded))

  // \dotfill — centred dot leaders that fill the line, the staple of a table of contents (`entry \dotfill page`).
  // The unit is a period followed by a gap, so the dots are evenly spaced; the glue is 0pt plus 1fil.
  proc.registerPrimitive(
    "dotfill",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val unit = new HBox(Seq(new CharBox(t, "."), HSpaceBox(t.currentFont.size * 0.4)))
        t.add(new LeaderGlue(unit, LeaderKind.Centered, 0, 1, 0, 1))
    },
  )

  // \hrulefill — a thin rule that stretches to fill the line (a continuous leader). Useful for form fields and
  // signature lines. The rule is 0.4pt thick (TeX's default) and is re-sized to whatever width the glue takes.
  proc.registerPrimitive(
    "hrulefill",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.add(new LeaderGlue(new RuleBox(t, 0, 0.4, 0), LeaderKind.Aligned, 0, 1, 0, 1))
    },
  )

  // The TeX math-spacing commands: \, thin (3mu), \: medium (4mu), \; thick (5mu), \! negative thin (-3mu).
  // A mu is 1/18 em and em is the current font size, so these scale with the font. Rigid horizontal spaces that
  // work in math or text — the manual spaces you reach for between a coefficient and a species, around an
  // operator, and so on.
  def mathSpace(mu: Double): Primitive =
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit = t.add(HSpaceBox(t.currentFont.size * mu / 18))
    }
  proc.registerPrimitive(",", mathSpace(3))
  proc.registerPrimitive(":", mathSpace(4))
  proc.registerPrimitive(";", mathSpace(5))
  proc.registerPrimitive("!", mathSpace(-3))

  // \quad is a 1em space and \qquad a 2em space (18mu and 36mu, since a mu is 1/18 em) — the wide manual gaps
  // TeX uses to set formulas or examples apart on a line.
  proc.registerPrimitive("quad", mathSpace(18))
  proc.registerPrimitive("qquad", mathSpace(36))

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

  // slanted - 1 body arg: set its content in the slanted (oblique) shape, the upright body face sheared rather
  // than the separately-drawn italic. The shape axis: \slanted and \italic both flip it, so this is the
  // text-mode partner of \italic for a face that has a slanted cut (Latin Modern Roman does).
  proc.registerPrimitive(
    "slanted",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val body = proc.readArgument(pos)
        t.slanted()
        proc.processTokenList(body) // scoping happens automatically from { } tokens
        t.noslanted()
    },
  )

  // Font-shape and -series declarations (LaTeX \itshape / \bfseries / …). Unlike \italic{…} and \bold{…}, which
  // wrap an argument, these flip the current font for the *rest of the enclosing group* and take no argument. Font
  // state is saved on every group open and restored on close (Typesetter.enter/exit), so a declaration reverts at
  // the closing brace — or at an environment's \end, which closes a group too. That is what lets an environment set
  // its whole body in one shape: `\newenvironment thm {\bfseries Theorem.\ \itshape}{}` leaves the body italic and
  // the heading bold, both reverting at \end. The on-switches add a style; the resets remove one (\upshape clears the
  // shape axis — italic and small caps — and \mdseries clears the bold weight); \normalfont returns to the plain face.
  def declarePrimitive(name: String, switch: Typesetter => Unit): Unit =
    proc.registerPrimitive(
      name,
      new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit = switch(t)
      },
    )

  declarePrimitive("itshape", _.italic())
  declarePrimitive("slshape", _.slanted())
  declarePrimitive("bfseries", _.bold())
  declarePrimitive("scshape", _.smallcaps())
  declarePrimitive("upshape", _.removeStyle("italic", "slanted", "smallcaps"))
  declarePrimitive("mdseries", _.nobold())
  declarePrimitive("normalfont", t => { t.removeStyle("italic", "slanted", "bold", "smallcaps"); t.serif() })

  // Family-role declarations (LaTeX \rmfamily / \sffamily / \ttfamily). These flip the family-role axis for the
  // rest of the group, selecting the roman, sans-serif or typewriter member of the current super-family while
  // keeping the weight and slope — so \bfseries\ttfamily is bold typewriter when that cut exists.
  declarePrimitive("rmfamily", _.serif())
  declarePrimitive("sffamily", _.sans())
  declarePrimitive("ttfamily", _.mono())

  // \texttt / \textsf / \textrm - 1 body arg: set the content in the typewriter, sans-serif or roman member of the
  // current super-family, at the current size and keeping the weight and slope. The switch is scoped, so the text
  // resumes in the previous font afterwards; the interword space is reset to the chosen cut's own space (the mono
  // face is fixed-width, the others proportional). \texttt is the common case — inline code, file names, URLs.
  def roleText(switch: Typesetter => Font): Primitive =
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val body = proc.readArgument(pos)
        t.enter()
        val font = switch(t)
        t.set("spaceskip", Glue(font.space, 1))
        t.set("xspaceskip", Glue(font.space * 1.5, 1))
        proc.processTokenList(body)
        t.exit()
    }
  proc.registerPrimitive("texttt", roleText(_.mono()))
  proc.registerPrimitive("textsf", roleText(_.sans()))
  proc.registerPrimitive("textrm", roleText(_.serif()))

  // \glyphwidth{typeface}{size}{codepoint} - the width of one glyph's inked image, in points, at the given size:
  // the distance from the glyph's origin to the right edge of its ink (x-bearing + ink width). Returns a number
  // for \set / \calc, so a drawing can size itself to a real glyph rather than a guessed constant — the music
  // package reads a notehead's width this way to seat a stem on its right edge whatever the music font.
  proc.registerPrimitive(
    "glyphwidth",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val face = Value.display(proc.evalArgumentExpr(pos))
        val size = num1(proc, pos)
        val cp   = num1(proc, pos).toInt
        val font = t.makeFont(face, size, Set.empty[String])
        val rf   = font.renderFont.asInstanceOf[t.RenderFont]
        val ext  = t.glyphExtents(rf, t.glyphIndex(rf, cp))
        val w    = ext.xBearing + ext.width
        proc.setResult(Value.Num(w))
        proc.handler.text(w.toString)
    },
  )

  // textsub / textsup - 1 body arg: a text subscript or superscript. The body is set in a smaller version of
  // the current font — scriptScale of its size — and its box is shifted below (sub) or above (sup) the baseline.
  // The size is derived from the current font, so a script adapts to the body size rather than a fixed point
  // size: H\textsub{2}O, the 1\textsup{st}, a chemical formula's atom counts. A box, so it stays attached to the
  // preceding character with no interword space.
  def scriptPrimitive(drop: Boolean): Primitive =
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val body = proc.readArgument(pos)
        val base = t.currentFont
        t.enter()
        t.currentFont = t.makeFont(base.typeface, base.size * scriptScale, base.style)
        t.hbox(null)
        proc.processTokenList(body)
        val box = t.mode.exit
        t.exit()
        if box ne null then
          val shift = if drop then base.size * subDrop else -(base.size * supRise)
          handler.addBox(new ShiftBox(box, shift))
    }
  proc.registerPrimitive("textsub", scriptPrimitive(drop = true))
  proc.registerPrimitive("textsup", scriptPrimitive(drop = false))

  // \verb<delim>…<delim> - inline verbatim. The character right after \verb is the delimiter; everything up to its
  // next occurrence is set literally in the typewriter face, with no comment, escape, active-character or macro
  // processing — so \verb|a_b//c| reproduces those characters exactly. Like LaTeX's \verb, it must be written on the
  // input directly (its text is read raw from the tokenizer), not produced by a macro.
  proc.registerPrimitive(
    "verb",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val s = proc.readVerb(pos)
        handler.flushPendingSpace()
        val saved = t.currentFont
        t.mono()
        val box = new CharBox(t, s, t.currentFont, t.currentColor)
        t.currentFont = saved
        handler.addBox(box)
    },
  )

  // \code[language]{raw body} - typeset a code listing. The body is read raw (so backslashes, braces and // all
  // survive), set in the JetBrains Mono code face, one source line per output line. With a [language] it is
  // syntax-highlighted using that language's bundled TextMate grammar and the current theme (\set codetheme);
  // without one it is plain. The braces in the body must balance — for code that does not, use the code
  // environment (\begin{code} … \end{code}), which reads to \end{code} and takes its language from \set codelang.
  // Like \verb, the body must be written on the input directly (read raw), not produced by a macro.
  proc.registerPrimitive(
    "code",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val lang = proc.readOptionalRawBracket().getOrElse("")
        val body = proc.readRawArgument(pos)
        handler.placeCode(lang, body, pos)
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

  // fbox / framebox - typeset the body and draw a rectangular frame around it, with \fboxsep of padding between
  // the content and the rule and a rule \fboxrule thick (LaTeX defaults 3pt / 0.4pt). \framebox is the same here
  // (its optional [width][pos] sizing is not supported). The frame colour is the current pen colour.
  def frameBoxPrimitive: Primitive =
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        captureHBox(proc, t, handler, pos) match
          case b: Box =>
            val sep  = numVarOr(t, "fboxsep", 3.0)
            val rule = numVarOr(t, "fboxrule", 0.4)
            handler.addBox(new FrameBox(b, sep, rule, t.currentColor, null))
          case null =>
    }
  proc.registerPrimitive("fbox", frameBoxPrimitive)
  proc.registerPrimitive("framebox", frameBoxPrimitive)

  // colorbox color body - fill the body's background (padded by \fboxsep) with the named colour, no frame.
  proc.registerPrimitive(
    "colorbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        evalArg(proc, pos) match
          case Value.Text(name) =>
            captureHBox(proc, t, handler, pos) match
              case b: Box => handler.addBox(new FrameBox(b, numVarOr(t, "fboxsep", 3.0), 0.0, null, Color(name)))
              case null   =>
          case _ => handler.error("\\colorbox expects a colour name or #RRGGBB code", pos)
    },
  )

  // fcolorbox framecolor backgroundcolor body - draw a frame in the first colour around a body filled with the
  // second, padded and ruled like \fbox.
  proc.registerPrimitive(
    "fcolorbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        (evalArg(proc, pos), evalArg(proc, pos)) match
          case (Value.Text(frame), Value.Text(bg)) =>
            captureHBox(proc, t, handler, pos) match
              case b: Box =>
                handler.addBox(new FrameBox(b, numVarOr(t, "fboxsep", 3.0), numVarOr(t, "fboxrule", 0.4), Color(frame), Color(bg)))
              case null =>
          case _ => handler.error("\\fcolorbox expects two colour names or #RRGGBB codes", pos)
    },
  )

  // rotatebox angle body - rotate the body counter-clockwise by `angle` degrees about its left baseline (LaTeX's
  // default origin), reserving the bounding box of the rotated result.
  proc.registerPrimitive(
    "rotatebox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val argPos = argumentPos(proc, pos)
        Value.number(evalArg(proc, pos)) match
          case Some(deg) =>
            captureHBox(proc, t, handler, pos) match
              case b: Box =>
                val rad = deg * math.Pi / 180.0
                val c   = math.cos(rad)
                val s   = math.sin(rad)
                // Device y is down, so a visually counter-clockwise turn is a clockwise matrix in device space:
                // (x,y) -> (x cos + y sin, -x sin + y cos); the backend gets the matching rotate(-rad).
                handler.addBox(new TransformBox(b, c, s, -s, c, _.rotate(-rad)))
              case null =>
          case None => handler.error("\\rotatebox expects an angle in degrees", argPos)
    },
  )

  // scalebox factor [yfactor] body - scale the body horizontally by `factor` and vertically by `yfactor`
  // (defaulting to `factor`, so a single argument scales uniformly).
  proc.registerPrimitive(
    "scalebox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val argPos = argumentPos(proc, pos)
        Value.number(evalArg(proc, pos)) match
          case Some(sx) =>
            val sy = readOptionalNumber(proc).getOrElse(sx)
            captureHBox(proc, t, handler, pos) match
              case b: Box => handler.addBox(new TransformBox(b, sx, 0, 0, sy, _.scale(sx, sy)))
              case null   =>
          case None => handler.error("\\scalebox expects a scale factor", argPos)
    },
  )

  // reflectbox body - mirror the body left-to-right (a horizontal scale of -1), the common case of \scalebox.
  proc.registerPrimitive(
    "reflectbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        captureHBox(proc, t, handler, pos) match
          case b: Box => handler.addBox(new TransformBox(b, -1, 0, 0, 1, _.scale(-1, 1)))
          case null   =>
    },
  )

  // resizebox width height body - scale the body to the given width and height. A `!` for either dimension keeps
  // the aspect ratio set by the other, so \resizebox{2in}{!}{...} scales to 2in wide without distortion.
  proc.registerPrimitive(
    "resizebox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val wOpt = resizeDim(proc, pos)
        val hOpt = resizeDim(proc, pos)
        captureHBox(proc, t, handler, pos) match
          case b: Box =>
            val natW = b.width
            val natH = b.height
            val (sx, sy) = (wOpt, hOpt) match
              case (Some(w), Some(h)) => (if natW > 0 then w / natW else 1.0, if natH > 0 then h / natH else 1.0)
              case (Some(w), None)    => val f = if natW > 0 then w / natW else 1.0; (f, f)
              case (None, Some(h))    => val f = if natH > 0 then h / natH else 1.0; (f, f)
              case (None, None)       => (1.0, 1.0)
            handler.addBox(new TransformBox(b, sx, 0, 0, sy, _.scale(sx, sy)))
          case null =>
    },
  )

  // raisebox lift body - raise the body by `lift` (lower it when negative), adjusting the box's reported height
  // and depth to its new position (unlike \raise / \lower, which leave the metrics alone).
  proc.registerPrimitive(
    "raisebox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val argPos = argumentPos(proc, pos)
        points(proc.evalArgumentExpr(pos)) match
          case Some(lift) =>
            captureHBox(proc, t, handler, pos) match
              case b: Box => handler.addBox(new RaiseBox(b, lift))
              case null   =>
          case None => handler.error("\\raisebox expects a dimension", argPos)
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

// An inline bitmap built by \defbitmap: the backend image handle (opaque) plus its pixel size, stored as a
// Value.Native so \usebitmap can place it.
private[parser] case class InlineBitmap(handle: Any, width: Int, height: Int)

// Read an argument and coerce it to an integer (for \defbitmap's width / height / depth).
private[parser] def argInt(proc: Processor, pos: CharReader): Int =
  Value.number(evalArg(proc, pos)).map(_.toInt).getOrElse(0)

// Decode base64 to bytes, ignoring any whitespace and padding. A small self-contained decoder (no java.util),
// so it compiles on every backend. Both the standard (`+/`) and URL-safe (`-_`) alphabets are accepted; embedded
// data uses the URL-safe form so it never contains `//`, which would otherwise be read as a comment.
private[parser] def base64Decode(s: String): Array[Byte] =
  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  val lookup   = Array.fill(128)(-1)
  for i <- alphabet.indices do lookup(alphabet(i).toInt) = i
  lookup('-'.toInt) = 62
  lookup('_'.toInt) = 63
  val out    = scala.collection.mutable.ArrayBuffer[Byte]()
  var buffer = 0
  var bits   = 0
  for c <- s do
    val v = if c.toInt < 128 then lookup(c.toInt) else -1
    if v >= 0 then
      buffer = (buffer << 6) | v
      bits += 6
      if bits >= 8 then
        bits -= 8
        out += ((buffer >> bits) & 0xff).toByte
  out.toArray

// Unpack a packed alpha bitmap into straight ARGB pixels. Each pixel is `depth` bits (MSB-first, row-major),
// giving an alpha level from 0 (transparent) to full (opaque); the colour is black, so the value is the glyph's
// coverage. 1-bit is on/off; 2/4/8-bit carry antialiased grey levels.
private[parser] def unpackBitmapAlpha(bytes: Array[Byte], width: Int, height: Int, depth: Int): Array[Int] =
  val n    = width * height
  val argb = new Array[Int](n)
  val maxv = (1 << depth) - 1
  var bitpos = 0
  var i      = 0
  while i < n do
    var v = 0
    var d = 0
    while d < depth do
      val byteIdx = bitpos >> 3
      val bit     = if byteIdx < bytes.length then (bytes(byteIdx) >> (7 - (bitpos & 7))) & 1 else 0
      v = (v << 1) | bit
      bitpos += 1
      d += 1
    val alpha = if maxv == 0 then 0 else v * 255 / maxv
    argb(i) = alpha << 24
    i += 1
  argb

// Build the TeX (or TeXish) logo in the current text font: a T, an E lowered half an x-height and kerned
// A dimension value in big points: Dimen carries its own unit; a bare number means points
private[parser] def points(v: Value): Option[Double] = v match
  case Value.Dimen(p) => Some(p.toDouble)
  case Value.Num(n)   => Some(n.toDouble)
  case _              => None

// Text script geometry (\textsub / \textsup): the body is set at scriptScale of the current font size and
// its box shifted by a fraction of that size below (sub) or above (sup) the baseline.
private val scriptScale = 0.7
private val subDrop     = 0.18
private val supRise     = 0.42

// Resolve an image path the way \use resolves a module: an absolute path is used as given, a relative one is
// taken relative to the directory of the document being processed, so \includegraphics{frog.jpg} finds the
// image beside the source file rather than relative to the working directory.
private[parser] def resolveImagePath(proc: Processor, path: String): String =
  val p = Path(path)
  if p.isAbsolute then path else (Path(proc.currentDir) / p).toString

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

// Typeset a braced `{…}` argument as a single LR-mode hbox and return it (null when empty) — the way the
// wrapping boxes (\fbox, \colorbox, \rotatebox, \scalebox, \resizebox, \raisebox, …) capture their content,
// matching \underline. The body is read first, then the pending interword space is flushed before the hbox is
// pushed, so a source newline before the command keeps its space.
private[parser] def captureHBox(proc: Processor, t: Typesetter, handler: TypesetterHandler, pos: CharReader): Box | Null =
  val body = proc.readArgument(pos)
  handler.flushPendingSpace()
  t.hbox(null)
  proc.processTokenList(body)
  t.mode.exit

// Read a numeric variable in points, falling back to `default` when it is unset — used for \fboxsep / \fboxrule,
// which a document may set with \set but which have LaTeX-default values otherwise.
private[parser] def numVarOr(t: Typesetter, name: String, default: Double): Double =
  t.get(name).flatMap(points).getOrElse(default)

// Read an optional bracketed number ([0.5], [-1]) following \scalebox, in the style of readPlacementSpec. Returns
// None (leaving the stream untouched) when no '[' run follows.
private[parser] def readOptionalNumber(proc: Processor): Option[Double] =
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
      out.toString.trim.toDoubleOption
    case _ => None

// Read a \resizebox dimension argument: a braced dimension, or `!` to mean "keep the aspect ratio set by the
// other dimension" (returned as None).
private[parser] def resizeDim(proc: Processor, pos: CharReader): Option[Double] =
  evalArg(proc, pos) match
    case Value.Text(s) if s.trim == "!" => None
    case v                              => points(v)

// Typeset a braced `{…}` argument as horizontal material and return its boxes — used for the three parts of a
// \discretionary. The content is set in a throwaway \hbox so it goes through the normal text path (font, kerning,
// ligatures); the resulting boxes are handed back individually so the line breaker can place them on either side
// of a break. An empty group yields no boxes.
private[parser] def typesetGroupBoxes(proc: Processor, t: Typesetter, pos: CharReader): Seq[Box] =
  val body = proc.readArgument(pos)
  t.hbox()
  proc.processTokenList(body)
  t.mode.exit match
    case null    => Seq.empty
    case h: HBox => h.boxes
    case b: Box  => Seq(b)

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
