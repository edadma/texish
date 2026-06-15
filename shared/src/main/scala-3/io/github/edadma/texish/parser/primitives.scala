package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.{
  Anchor,
  Box,
  Color,
  Glue,
  GlyphBox,
  HBox,
  HSpaceBox,
  Hyphenation,
  InfGlue,
  InsertBox,
  LineCap,
  LineJoin,
  MarkBox,
  MathAtom,
  MathClass,
  MathDelimiters,
  MathMode,
  MathStyle,
  Penalty,
  PictureMode,
  RuleBox,
  ShiftBox,
  Typesetter,
  UnderlineBox,
  VerticalBox,
}

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
          case Value.Text(path) => t.image(path)
          case _                => handler.error("\\image expects a path", pos)
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

  // hrule - 0 args but optional params
  proc.registerPrimitive(
    "hrule",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val opts    = proc.readOptionalParams(pos)
        val width   = opts.get("width").flatMap(points).getOrElse(t.getNumber("hsize"))
        val ascent  = opts.get("ascent").flatMap(points).getOrElse(3.0)
        val descent = opts.get("descent").flatMap(points).getOrElse(0.0)
        t.add(RuleBox(t, width, ascent, descent))
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

  // frac - 2 body args: a fraction numerator over denominator. Math-mode only; the numerator and denominator
  // are each typeset by a nested math mode one style smaller (so an inline fraction sets its parts at script
  // size), then stacked over a rule centered on the math axis. The result enters the list as an Inner atom.
  proc.registerPrimitive(
    "frac",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case parent: MathMode =>
            val numTokens   = proc.readArgument(pos)
            val denomTokens = proc.readArgument(pos)
            val numBox      = handler.mathSubFormula(proc, parent.style.num, numTokens)
            val denomBox    = handler.mathSubFormula(proc, parent.style.denom, denomTokens)

            if (numBox ne null) && (denomBox ne null) then
              parent.addNode(MathAtom(MathClass.Inner, parent.makeFraction(numBox, denomBox)))
          case _ => handler.error("\\frac is only allowed in math mode", pos)
    },
  )

  // sqrt - an optional [degree] then 1 body arg: a square root, or a higher root when the degree is given
  // (\sqrt[3]{x} is a cube root). Math-mode only; the radicand is typeset by a nested math mode in the cramped
  // current style and the degree, if any, in scriptscript; a surd glyph tall enough to span the radicand is
  // set on the left with a vinculum across the top, the degree tucked into its kink. Enters as an Ord atom.
  proc.registerPrimitive(
    "sqrt",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case parent: MathMode =>
            val degreeTokens = readOptionalDegree(proc)
            val radicand     = handler.mathSubFormula(proc, parent.style.cramp, proc.readArgument(pos))
            val degree: Option[Box] = degreeTokens match
              case Some(toks) =>
                handler.mathSubFormula(proc, parent.style.rootDegree, toks) match
                  case b: Box => Some(b)
                  case null   => None
              case None => None

            if radicand ne null then parent.addNode(MathAtom(MathClass.Ord, parent.makeRadical(radicand, degree)))
          case _ => handler.error("\\sqrt is only allowed in math mode", pos)
    },
  )

  // left - opens a stretchy delimited sub-formula. Math-mode only; reads the opening delimiter, collects the
  // body up to the matching \right, reads the closing delimiter, then sizes both fences to span the body about
  // the math axis. Either delimiter may be `.` (drawn as nothing). The whole thing enters the list as an Inner
  // atom, so it gets the spacing of a parenthesized subexpression.
  proc.registerPrimitive(
    "left",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case parent: MathMode =>
            val leftDelim  = readDelimiter(proc, handler, pos)
            val body       = proc.collectDelimitedBody(pos)
            val rightDelim = readDelimiter(proc, handler, pos)
            val inner      = handler.mathSubFormula(proc, parent.style, body)

            if inner ne null then
              parent.addNode(MathAtom(MathClass.Inner, parent.makeDelimited(leftDelim, inner, rightDelim)))
          case _ => handler.error("\\left is only allowed in math mode", pos)
    },
  )

  // right - only meaningful as the close of a \left group, which consumes it directly; standing alone it is an
  // error (an unmatched \right).
  proc.registerPrimitive(
    "right",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit = handler.error("\\right without matching \\left", pos)
    },
  )

  // limits / nolimits - force a large operator's scripts above/below (limits) or to the side. Math-mode only;
  // must follow an operator. Inline math defaults to side-set scripts, so \limits is how a \sum or \prod gets
  // its bounds stacked over and under (and its glyph enlarged) without display mode.
  proc.registerPrimitive(
    "limits",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case parent: MathMode => parent.setLimits(true)
          case _                => handler.error("\\limits is only allowed in math mode", pos)
    },
  )
  proc.registerPrimitive(
    "nolimits",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case parent: MathMode => parent.setLimits(false)
          case _                => handler.error("\\nolimits is only allowed in math mode", pos)
    },
  )

  // over / atop - infix fraction operators. Math-mode only; everything in the current group before the
  // operator is the numerator and everything after is the denominator, each set one style smaller (script
  // size inline, text size in display), exactly as in plain TeX. \over draws the fraction rule; \atop stacks
  // the operands with no rule. They are scoped by braces — {a+b \over c+d} — because a {…} in math is its own
  // sub-formula; without braces the operator takes the whole formula, as a display \over usually does.
  proc.registerPrimitive(
    "over",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case parent: MathMode => parent.setFraction(bar = true)
          case _                => handler.error("\\over is only allowed in math mode", pos)
    },
  )
  proc.registerPrimitive(
    "atop",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case parent: MathMode => parent.setFraction(bar = false)
          case _                => handler.error("\\atop is only allowed in math mode", pos)
    },
  )

  // eqno - 1 body arg: an equation number for the surrounding display. Display-math only; the number is
  // typeset by a nested math mode at text size and flushed to the right margin on the display line. As in
  // plain TeX, the material is set in math (so "(3.1)" sets its parens and digits as math symbols).
  proc.registerPrimitive(
    "eqno",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case parent: MathMode if parent.style.isDisplay =>
            val box = handler.mathSubFormula(proc, MathStyle.Text, proc.readArgument(pos))

            if box ne null then parent.eqno = Some(box)
          case _ => handler.error("\\eqno is only allowed in display math", pos)
    },
  )

  // Math accents: each sets an accent glyph over its single argument's nucleus. Math-mode only; the nucleus
  // is typeset by a nested math mode in the cramped current style, then the accent is centred over it. The
  // wide forms (\widehat, \widetilde) grow a horizontal variant to span a multi-character nucleus. Enters as
  // an Ord atom.
  val mathAccents: Map[String, (Int, Boolean)] = Map(
    "hat"      -> (0x0302, false), "widehat"   -> (0x0302, true),
    "tilde"    -> (0x0303, false), "widetilde" -> (0x0303, true),
    "check"    -> (0x030C, false), "breve"     -> (0x0306, false),
    "acute"    -> (0x0301, false), "grave"     -> (0x0300, false),
    "dot"      -> (0x0307, false), "ddot"      -> (0x0308, false),
    "bar"      -> (0x0304, false), "vec"       -> (0x20D7, false),
    "mathring" -> (0x030A, false),
  )

  for (name, (codepoint, wide)) <- mathAccents do
    proc.registerPrimitive(
      name,
      new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit =
          t.mode match
            case parent: MathMode =>
              val nucleus = handler.mathSubFormula(proc, parent.style.cramp, proc.readArgument(pos))

              if nucleus ne null then
                parent.addNode(MathAtom(MathClass.Ord, parent.makeAccent(codepoint, nucleus, wide)))
            case _ => handler.error(s"\\$name is only allowed in math mode", pos)
      },
    )

  // matrix and its bracketed forms - 1 body arg: a grid of math cells, & between columns and \cr (or \\)
  // between rows. Math-mode only; each cell is typeset by a nested math mode in the array's cell style (text
  // style, so a matrix in a display does not enlarge its entries), the cells are aligned into columns and
  // baseline-spaced rows centred on the math axis, and \pmatrix/\bmatrix/\cases wrap the array in stretchy
  // fences sized to span it. \cases sets its columns flush left under a single left brace. Enters as an Inner
  // atom, so a matrix gets the spacing of a parenthesised subexpression.
  def matrixPrimitive(left: Option[Int], right: Option[Int], leftAlign: Boolean): Primitive =
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case parent: MathMode =>
            val body = stripOuterBraces(proc.readArgument(pos))
            val rows = splitMatrixBody(body).map(_.map { cellTokens =>
              handler.mathSubFormula(proc, parent.cellStyle, cellTokens) match
                case b: Box => b
                case null   => HBox(Vector.empty)
            })
            val array = parent.makeMatrix(rows, leftAlign)
            val box   = if left.isEmpty && right.isEmpty then array else parent.makeDelimited(left, array, right)

            parent.addNode(MathAtom(MathClass.Inner, box))
          case _ => handler.error("\\matrix is only allowed in math mode", pos)
    }

  proc.registerPrimitive("matrix", matrixPrimitive(None, None, leftAlign = false))
  proc.registerPrimitive("pmatrix", matrixPrimitive(Some(0x28), Some(0x29), leftAlign = false))
  proc.registerPrimitive("bmatrix", matrixPrimitive(Some(0x5B), Some(0x5D), leftAlign = false))
  proc.registerPrimitive("cases", matrixPrimitive(Some(0x7B), None, leftAlign = true))

  // noalign - 1 body arg (no scoping - it's inline content in table)
  proc.registerPrimitive(
    "noalign",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val body = proc.readArgument(pos)
        t.op("noalign-begin")
        proc.processTokenList(body)
        t.op("noalign-end")
    },
  )

  // halign - 1 body arg (no scoping - table inherits outer context)
  proc.registerPrimitive(
    "halign",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val body = proc.readArgument(pos)
        t.halign
        proc.processTokenList(body)
        t.done()
    },
  )

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

  // underline - 1 body arg (wraps content in underline)
  proc.registerPrimitive(
    "underline",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val body = proc.readArgument(pos)
        // Create an hbox to capture the content
        t.hbox(null)
        proc.processTokenList(body) // scoping happens automatically from { } tokens
        val box = t.mode.exit
        if box ne null then handler.addBox(new UnderlineBox(t, box))
    },
  )

  // TeX / TeXish - the engine's logos, set in the current text font through the glyph seam so the letters are
  // placed by their own advances (no string-shaping in between) and the E nestles between the T and X. \TeXish
  // is the same logo with "ish" trailing — the name is the pun.
  proc.registerPrimitive("TeX", SimplePrimitive(() => handler.addBox(texLogo(t, ish = false))))
  proc.registerPrimitive("TeXish", SimplePrimitive(() => handler.addBox(texLogo(t, ish = true))))

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

  proc.registerActive(
    '#',
    new Active {
      def execute(proc: Processor, c: Char, pos: CharReader): Unit =
        t.op("placeholder")
    },
  )

  proc.registerActive(
    '&',
    new Active {
      def execute(proc: Processor, c: Char, pos: CharReader): Unit =
        t.op("newColumn")
    },
  )

  registerPictureGraphicsPrimitives(proc, handler)

// The vector-graphics vocabulary: `\picture` opens a drawing, the rest of these are only meaningful inside one.
// `\picture` mirrors `\hbox` — it pushes a PictureMode, processes its body (the drawing commands fill a display
// list), and on done() drops the resulting PictureBox into the surrounding text. Each drawing command guards
// that a PictureMode is on top and calls the matching collector method; coordinates parse through `readNumbers`,
// which evaluates each whitespace-separated piece of a `{x y …}` group as an expression (so a literal `2in`, a
// variable `\the\x`, and a computed `\*{\the\i}{14}` are all valid coordinates).
def registerPictureGraphicsPrimitives(proc: Processor, handler: TypesetterHandler): Unit =
  val t = handler.typesetter

  // \picture - optional width:/height: params + 1 body arg, like \hbox to:. Pushes a PictureMode, processes the
  // body, and lets Mode.done() add the PictureBox to the surrounding list.
  proc.registerPrimitive(
    "picture",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val opts   = proc.readOptionalParams(pos)
        val width  = opts.get("width").flatMap(points).getOrElse(0.0)
        val height = opts.get("height").flatMap(points).getOrElse(0.0)
        val body   = proc.readArgument(pos)
        t.push(new PictureMode(t, width, height))
        proc.processTokenList(body)
        t.mode.done()
    },
  )

  // \coordinate{name}{coord} - name a point for later reference as (name). Stored as a two-element numeric
  // sequence in the document scope, so it reads back through the same variable machinery everything else uses.
  picturePrimitive(
    proc,
    handler,
    "coordinate",
    (_, p) =>
      val name = Value.display(proc.evalArgumentExpr(p))
      val c    = readNumbers(proc, p)
      if c.length < 2 then handler.error(s"\\coordinate '$name' expects a point", p)
      t.set(name, Value.Seq(Vector(Value.Num(c(0)), Value.Num(c(1))))),
  )

  // \xof{coord} / \yof{coord} - the x or y of a coordinate, as a number for use in expressions. These let a
  // package compute with points (a bond's perpendicular offset, a midpoint) in the document language itself.
  proc.registerPrimitive(
    "xof",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val c = readNumbers(proc, pos)
        proc.setResult(Value.Num(if c.nonEmpty then c.head else 0.0))
    },
  )
  proc.registerPrimitive(
    "yof",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val c = readNumbers(proc, pos)
        proc.setResult(Value.Num(if c.length > 1 then c(1) else 0.0))
    },
  )

  // State: colours are picture-mode state baked into each shape's paint; widths/dashes/caps/joins become ops.
  picturePrimitive(proc, handler, "stroke", (pm, p) => pm.setStroke(readColorArg(proc, p)))
  picturePrimitive(proc, handler, "fill", (pm, p) => pm.setFill(readColorArg(proc, p)))
  picturePrimitive(proc, handler, "nostroke", (pm, _) => pm.noStroke())
  picturePrimitive(proc, handler, "nofill", (pm, _) => pm.noFill())
  picturePrimitive(proc, handler, "linewidth", (pm, p) => pm.setLineWidth(num1(proc, p)))
  picturePrimitive(
    proc,
    handler,
    "linecap",
    (pm, p) =>
      val s = Value.display(proc.evalArgumentExpr(p))
      LineCap.fromString(s) match
        case Some(c) => pm.setLineCap(c)
        case None    => handler.error(s"unknown line cap '$s' (expected butt, round, or square)", p),
  )
  picturePrimitive(
    proc,
    handler,
    "linejoin",
    (pm, p) =>
      val s = Value.display(proc.evalArgumentExpr(p))
      LineJoin.fromString(s) match
        case Some(j) => pm.setLineJoin(j)
        case None    => handler.error(s"unknown line join '$s' (expected miter, round, or bevel)", p),
  )
  picturePrimitive(proc, handler, "dash", (pm, p) => pm.setDash(readNumbers(proc, p), 0))

  // \linetype - named stroke-style presets over \dash. Dotted pairs well with \linecap{round} to render as dots.
  picturePrimitive(
    proc,
    handler,
    "linetype",
    (pm, p) =>
      Value.display(proc.evalArgumentExpr(p)) match
        case "solid"   => pm.setDash(Vector.empty, 0)
        case "dashed"  => pm.setDash(Vector(4.0, 4.0), 0)
        case "dotted"  => pm.setDash(Vector(1.0, 3.0), 0)
        case "dashdot" => pm.setDash(Vector(4.0, 2.0, 1.0, 2.0), 0)
        case other     => handler.error(s"unknown line type '$other' (solid, dashed, dotted, dashdot)", p),
  )

  // Transforms: \rotate is in degrees, counter-clockwise in the picture's y-up space.
  picturePrimitive(proc, handler, "translate", (pm, p) => { val c = readNumbers(proc, p); pm.translate(c(0), c(1)) })
  picturePrimitive(proc, handler, "scale", (pm, p) => { val c = readNumbers(proc, p); pm.scale(c(0), c(1)) })
  picturePrimitive(proc, handler, "rotate", (pm, p) => pm.rotate(math.toRadians(num1(proc, p))))

  // Shapes lower to a path plus one paint with the current colours.
  picturePrimitive(proc, handler, "line", (pm, p) => { val c = readNumbers(proc, p); pm.line(c(0), c(1), c(2), c(3)) })
  picturePrimitive(proc, handler, "rect", (pm, p) => { val c = readNumbers(proc, p); pm.rect(c(0), c(1), c(2), c(3)) })
  picturePrimitive(proc, handler, "circle", (pm, p) => { val c = readNumbers(proc, p); pm.circle(c(0), c(1), c(2)) })
  picturePrimitive(
    proc,
    handler,
    "ellipse",
    (pm, p) => { val c = readNumbers(proc, p); pm.ellipse(c(0), c(1), c(2), c(3)) },
  )
  picturePrimitive(proc, handler, "polygon", (pm, p) => pm.polygon(coordPairs(readNumbers(proc, p))))
  picturePrimitive(proc, handler, "polyline", (pm, p) => pm.polyline(coordPairs(readNumbers(proc, p))))
  picturePrimitive(
    proc,
    handler,
    "arc",
    (pm, p) =>
      val c = readNumbers(proc, p)
      pm.arcShape(c(0), c(1), c(2), math.toRadians(c(3)), math.toRadians(c(4)), negative = false),
  )
  picturePrimitive(
    proc,
    handler,
    "arcn",
    (pm, p) =>
      val c = readNumbers(proc, p)
      pm.arcShape(c(0), c(1), c(2), math.toRadians(c(3)), math.toRadians(c(4)), negative = true),
  )

  // Freeform path: \path{ \moveto \lineto \curveto \close } builds a path with these segment commands and paints
  // it with the current state. The segment commands are themselves picture-only.
  picturePrimitive(
    proc,
    handler,
    "path",
    (pm, p) =>
      pm.newPath()
      proc.processTokenList(proc.readArgument(p))
      pm.paint(),
  )
  picturePrimitive(proc, handler, "moveto", (pm, p) => { val c = readNumbers(proc, p); pm.moveTo(c(0), c(1)) })
  picturePrimitive(proc, handler, "lineto", (pm, p) => { val c = readNumbers(proc, p); pm.lineTo(c(0), c(1)) })
  picturePrimitive(
    proc,
    handler,
    "curveto",
    (pm, p) => { val c = readNumbers(proc, p); pm.curveTo(c(0), c(1), c(2), c(3), c(4), c(5)) },
  )
  picturePrimitive(proc, handler, "close", (pm, _) => pm.close())

  // Grouping and clipping: \group save/restores the whole graphics state; \clip intersects the clip with a path
  // built the same way \path's body is.
  picturePrimitive(
    proc,
    handler,
    "group",
    (pm, p) =>
      val body = proc.readArgument(p)
      pm.groupBegin()
      proc.processTokenList(body)
      pm.groupEnd(),
  )
  picturePrimitive(
    proc,
    handler,
    "clip",
    (pm, p) =>
      pm.newPath()
      proc.processTokenList(proc.readArgument(p))
      pm.clip(),
  )

  // \at[anchor:]{x y}{content} - place fully typeset content (text, math) at a coordinate. The content is set in
  // its own horizontal box, exactly as \lower/\raise read a box, then placed with its anchor on the point.
  proc.registerPrimitive(
    "at",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val pm     = requirePicture(t, handler, "at", pos)
        val anchor = readAnchor(proc, pos, Anchor.Center)
        val c      = readNumbers(proc, pos)
        val body   = proc.readArgument(pos)
        t.hbox(null)
        proc.processTokenList(body)
        t.mode.exit match
          case b: Box => pm.place(b, anchor, c(0), c(1))
          case null   =>
    },
  )

  // \glyph[anchor:]{x y}{codepoint} - place one glyph (a marker, arrowhead, charge sign) by codepoint, drawn in
  // the current fill colour. Defaults to a baseline anchor, the natural attach point for a glyph.
  proc.registerPrimitive(
    "glyph",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val pm     = requirePicture(t, handler, "glyph", pos)
        val anchor = readAnchor(proc, pos, Anchor.Baseline)
        val c      = readNumbers(proc, pos)
        val cp     = num1(proc, pos).toInt
        val rf     = t.currentFont.renderFont.asInstanceOf[t.RenderFont]
        val color  = pm.fillColor.orElse(pm.strokeColor).getOrElse(Color("black"))
        pm.place(new GlyphBox(t, t.glyphIndex(rf, cp), t.currentFont, color), anchor, c(0), c(1))
    },
  )

// Register a picture-only command: it guards that a PictureMode is on top (else a clear error) and runs `body`
// with that mode. Keeps the many shape/state primitives to one line each.
private def picturePrimitive(
    proc: Processor,
    handler: TypesetterHandler,
    name: String,
    body: (PictureMode, CharReader) => Unit,
): Unit =
  proc.registerPrimitive(
    name,
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        body(requirePicture(handler.typesetter, handler, name, pos), pos)
    },
  )

private def requirePicture(t: Typesetter, handler: TypesetterHandler, name: String, pos: CharReader): PictureMode =
  t.mode match
    case pm: PictureMode => pm
    case _               => handler.error(s"\\$name is only allowed inside \\picture", pos)

// Read a coordinate group as a flat list of points (in the engine's point space). Each whitespace-separated
// piece is either a parenthesised coordinate — Cartesian `(x,y)`, polar `(a:r)`, or a named `(name)`, each
// contributing two numbers (see `Coord`) — or a bare scalar expression contributing one (a literal `2in`, a
// variable `\the\x`, a computed `\*{a}{b}` or `\calc{…}`). So `\line{(0,0) (60:1in)}` and `\line{0 0 36 62}`
// produce the same flat stream the shape primitives consume, and the two notations interoperate.
private def readNumbers(proc: Processor, pos: CharReader): Vector[Double] =
  splitTopLevel(stripOuterBraces(proc.readArgument(pos))).flatMap { chunk =>
    val text = coordText(chunk)
    if Coord.looksLikeCoord(text) then
      val (x, y) = Coord.parse(text, varResolver(proc), proc.handler.fontUnit, namedResolver(proc))
      Vector(x, y)
    else Vector(points(proc.evalExpr(chunk, pos)).getOrElse(0.0))
  }

// Reconstruct a chunk's raw text for the coordinate parser: a control sequence contributes its bare name (so a
// variable `\R` reads as the identifier `R`) and an active character its symbol, matching how `\calc` flattens.
private def coordText(tokens: Vector[Token]): String =
  tokens.map {
    case Token.Text(s, _)       => s
    case Token.Space(s, _)      => s
    case Token.Newline(_)       => " "
    case Token.ControlSeq(n, _) => n
    case Token.Active(c, _)     => c.toString
    case _                      => ""
  }.mkString

// Resolve a bare identifier in a coordinate component expression to a document variable's number.
private def varResolver(proc: Processor): String => Option[Double] = name =>
  proc.handler.get(name) match
    case Value.Num(n)   => Some(n)
    case Value.Dimen(p) => Some(p)
    case _              => None

// Resolve a `(name)` reference to a point stored by \coordinate (a two-element numeric sequence).
private def namedResolver(proc: Processor): String => Option[(Double, Double)] = name =>
  proc.handler.get(name) match
    case Value.Seq(Vector(Value.Num(x), Value.Num(y))) => Some((x, y))
    case _                                             => None

// Read a single-number group, e.g. \linewidth{2pt} or \rotate{30}.
private def num1(proc: Processor, pos: CharReader): Double =
  points(proc.evalArgumentExpr(pos)).getOrElse(0.0)

// Split a coordinate group into its whitespace-separated pieces, keeping each piece intact across a brace group
// (`\*{a}{b}`) and across a parenthesised coordinate (`(60:1in)`, even with an internal space like `(2, 3)`).
// Brace depth is tracked from the group tokens; parenthesis depth from the characters of text tokens outside any
// braces, since parentheses are ordinary text. A whitespace token splits only when both depths are zero.
private def splitTopLevel(tokens: Vector[Token]): Vector[Vector[Token]] =
  val chunks = Vector.newBuilder[Vector[Token]]
  var cur    = Vector.newBuilder[Token]
  var depth  = 0
  var parens = 0
  var any    = false

  def flush(): Unit =
    if any then chunks += cur.result()
    cur = Vector.newBuilder[Token]
    any = false

  for tok <- tokens do
    tok match
      case Token.BeginGroup(_)                                       => depth += 1; cur += tok; any = true
      case Token.EndGroup(_)                                         => depth -= 1; cur += tok; any = true
      case (_: Token.Space | _: Token.Newline) if depth == 0 && parens == 0 => flush()
      case t @ Token.Text(s, _) =>
        if depth == 0 then parens += s.count(_ == '(') - s.count(_ == ')')
        cur += t; any = true
      case t => cur += t; any = true

  flush()
  chunks.result()

private def coordPairs(ns: Vector[Double]): Vector[(Double, Double)] =
  ns.grouped(2).collect { case Vector(x, y) => (x, y) }.toVector

// Evaluate a color argument: a named color (steelblue) or an #rrggbb hex code.
private def readColorArg(proc: Processor, pos: CharReader): Color =
  Color(Value.display(proc.evalArgumentExpr(pos)))

// Read an optional anchor:NAME parameter before a placement's coordinates.
private def readAnchor(proc: Processor, pos: CharReader, default: Anchor): Anchor =
  proc.readOptionalParams(pos).get("anchor").flatMap(v => Anchor.fromString(Value.display(v))).getOrElse(default)

// Helper class for simple 0-arg commands
class SimplePrimitive(action: () => Any) extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit = action()

// Helper to evaluate an argument and get its value
private def evalArg(proc: Processor, pos: CharReader): Value =
  proc.evalArgumentExpr(pos)

// Build the TeX (or TeXish) logo in the current text font: a T, an E lowered half an x-height and kerned
// back under the T, then an X kerned back under the E. The three capitals ride the glyph seam (placed by
// their own advances) so no string-shaping creeps between them; the kern fractions are the classic plain-TeX
// values. TeXish adds "ish" trailing, set smaller and raised — echoing the lifted "A" of the LaTeX logo.
private def texLogo(t: Typesetter, ish: Boolean): Box =
  val font = t.currentFont
  val rf   = font.renderFont.asInstanceOf[t.RenderFont]
  val em   = font.size
  val ex   = font.xHeight

  def glyph(c: Char): Box = new GlyphBox(t, t.glyphIndex(rf, c.toInt), font, t.currentColor)

  val pieces = Vector.newBuilder[Box]
  pieces += glyph('T')
  pieces += HSpaceBox(-0.1667 * em)
  pieces += new ShiftBox(glyph('E'), 0.5 * ex)
  pieces += HSpaceBox(-0.125 * em)
  pieces += glyph('X')

  if ish then
    // "ish" set smaller, slanted, and raised a little — echoing the lifted "A" of the LaTeX logo. Built in the
    // current typeface's slanted face (falling back to italic, then upright, for a font without one) at 70%
    // size on the string seam, since it is ordinary text; then kerned back so it tucks against the top-right
    // arm of the X, and shifted up so it rides above the baseline of the capitals.
    def face(extra: String) =
      try Some(t.makeFont(font.typeface, font.size * 0.7, font.style + extra))
      catch case _: RuntimeException => None

    val small = face("slanted").orElse(face("italic")).getOrElse(t.makeFont(font.typeface, font.size * 0.7, font.style))
    val saved = t.currentFont

    t.currentFont = small
    val ishBox = t.charBox("ish")
    t.currentFont = saved

    pieces += HSpaceBox(-0.14 * em)
    pieces += new ShiftBox(ishBox, -0.22 * em)

  HBox(pieces.result())

// Read an optional bracketed argument like \sqrt's [degree]. Brackets are ordinary text characters, not
// tokenizer-special, so the opening '[' may share a text token with the degree and the closing ']' may sit
// mid-token; this scans across tokens, splitting text runs at the brackets and pushing back any tail after
// ']'. Returns None when the next token is not a '[' run (no degree present), leaving the stream untouched.
private def readOptionalDegree(proc: Processor): Option[Vector[Token]] =
  proc.skipSpaces()
  proc.peekToken() match
    case Token.Text(s, sp) if s.startsWith("[") =>
      proc.nextToken()
      val out = Vector.newBuilder[Token]

      def takeText(str: String, p: io.github.edadma.char_reader.CharReader): Boolean =
        val idx = str.indexOf(']')
        if idx < 0 then { if str.nonEmpty then out += Token.Text(str, p); false }
        else
          val before = str.substring(0, idx)
          val after  = str.substring(idx + 1)
          if before.nonEmpty then out += Token.Text(before, p)
          if after.nonEmpty then proc.pushBack(Vector(Token.Text(after, p)))
          true

      var closed = takeText(s.substring(1), sp)
      while !closed && proc.hasMoreTokens do
        proc.nextToken() match
          case Token.Text(str, p) => closed = takeText(str, p)
          case Token.EOF(_)       => closed = true
          case other              => out += other
      Some(out.result())
    case _ => None

// Split a math-array body into rows of cells. At brace depth zero the column-separator active character `&`
// ends a cell and `\cr` or `\\` ends a row; nested {…} groups pass through verbatim so a braced cell is not
// split. A trailing row that holds nothing but the empty cell left by a final `\cr` is dropped, so a body
// that ends in a row separator does not add a spurious blank row. Each cell is the raw token run between
// separators, to be laid out by a nested math mode.
private def splitMatrixBody(body: Vector[Token]): Vector[Vector[Vector[Token]]] =
  val rows  = Vector.newBuilder[Vector[Vector[Token]]]
  var row   = Vector.newBuilder[Vector[Token]]
  var cell  = Vector.newBuilder[Token]
  var depth = 0

  def endCell(): Unit = { row += cell.result(); cell = Vector.newBuilder[Token] }
  def endRow(): Unit  = { endCell(); rows += row.result(); row = Vector.newBuilder[Vector[Token]] }

  for tok <- body do
    tok match
      case Token.BeginGroup(_)                     => depth += 1; cell += tok
      case Token.EndGroup(_)                       => depth -= 1; cell += tok
      case Token.Active('&', _) if depth == 0      => endCell()
      case Token.ControlSeq("cr", _) if depth == 0 => endRow()
      case Token.ControlSeq("\\", _) if depth == 0 => endRow()
      case _                                       => cell += tok

  endRow()

  def cellEmpty(c: Vector[Token]): Boolean = c.forall {
    case _: Token.Space | _: Token.Newline => true
    case _                                 => false
  }

  val all = rows.result()
  if all.nonEmpty && all.last.forall(cellEmpty) then all.init else all

// Read the delimiter that follows \left or \right: a single character (the first of the next text run, with
// the rest pushed back) or a control sequence, resolved through MathDelimiters. `.` and any unrecognized
// delimiter yield None — the null (undrawn) fence.
private def readDelimiter(proc: Processor, handler: TypesetterHandler, pos: CharReader): Option[Int] =
  proc.skipSpaces()
  proc.peekToken() match
    case Token.Text(s, p) if s.nonEmpty =>
      proc.nextToken()
      if s.length > 1 then proc.pushBack(Vector(Token.Text(s.substring(1), p)))
      MathDelimiters.forChar(s.charAt(0))
    case Token.ControlSeq(name, _) =>
      proc.nextToken()
      MathDelimiters.forCommand(name)
    case _ => handler.error("expected a delimiter after \\left or \\right", pos)

// A dimension value in big points: Dimen carries its own unit; a bare number means points
private def points(v: Value): Option[Double] = v match
  case Value.Dimen(p) => Some(p.toDouble)
  case Value.Num(n)   => Some(n.toDouble)
  case _              => None

// Build an \hbox or \vbox whose command token has already been consumed: read its optional `to:` target
// and braced body, typeset the body into a fresh builder, and return the finished box *without* adding it
// to the current list. Shared by the \hbox / \vbox / \vtop / \setbox primitives and \lower / \raise. `vertical`
// selects a vertical builder, `top` makes it a \vtop (reference point on the first line). The optional `to:` sets
// the final size and `spread:` adds to the natural size; at most one may be given.
private def buildBox(proc: Processor, t: Typesetter, vertical: Boolean, top: Boolean, pos: CharReader): Box | Null =
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
private def readBoxArg(proc: Processor, t: Typesetter, pos: CharReader): Box | Null =
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
private def boxRegister(proc: Processor, handler: TypesetterHandler, name: String, cmd: String, pos: CharReader): Box =
  proc.handler.get(name) match
    case Value.Native(b: Box) => b
    case Value.Undefined      => handler.error(s"\\$cmd: box register '$name' is empty", pos)
    case _                    => handler.error(s"\\$cmd: '$name' is not a box", pos)

// Resolve a glue argument: a braced glue spec ({12pt plus 2pt}), a glue-valued variable, or a bare dimension
// optionally continued by `plus`/`minus` keywords in the token stream (\vskip 12pt plus 2pt minus 1fil)
// The position of the next argument token (after any intervening spaces), or the command's own
// position when nothing follows. An "expects an argument" error reports this, so the caret points
// at the offending argument rather than at the start of the command.
private def argumentPos(proc: Processor, command: CharReader): CharReader =
  proc.skipSpaces()
  if proc.hasMoreTokens then Token.pos(proc.peekToken()) else command

private def glueArg(proc: Processor, pos: CharReader): Option[Glue] =
  proc.evalArgumentExpr(pos) match
    case Value.Glue(n, st, sh, sto, sho) => Some(Glue(n, st, sh, sto, sho))
    case Value.Native(g: Glue)           => Some(g)
    case Value.Dimen(p)                  => Some(glueContinuation(proc, p, pos))
    case Value.Num(n)                    => Some(glueContinuation(proc, n, pos))
    case _                               => None

private def glueContinuation(proc: Processor, natural: Double, pos: CharReader): Glue =
  proc.readGlueContinuation(natural, pos) match
    case Value.Glue(n, st, sh, sto, sho) => Glue(n, st, sh, sto, sho)
    case _                               => Glue(natural)
