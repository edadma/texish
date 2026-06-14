package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.{
  Box,
  Glue,
  GlyphBox,
  HBox,
  HSpaceBox,
  Hyphenation,
  InfGlue,
  InsertBox,
  MarkBox,
  MathAtom,
  MathClass,
  MathDelimiters,
  MathMode,
  MathStyle,
  Penalty,
  RuleBox,
  ShiftBox,
  Typesetter,
  UnderlineBox,
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

  // hbox - 1 body arg + optional "to" param
  proc.registerPrimitive(
    "hbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val opts = proc.readOptionalParams(pos)
        val body = proc.readArgument(pos)
        // build Double | Null directly — boxing through java.lang.Double would unbox null to 0.0
        val toVal: Double | Null = opts.get("to").flatMap(points) match
          case Some(d) => d
          case None    => null
        t.hbox(toVal)
        proc.processTokenList(body) // scoping happens automatically from { } tokens
        t.mode.done()
    },
  )

  // vbox - 1 body arg + optional "to" param
  proc.registerPrimitive(
    "vbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val opts = proc.readOptionalParams(pos)
        val body = proc.readArgument(pos)
        // build Double | Null directly — boxing through java.lang.Double would unbox null to 0.0
        val toVal: Double | Null = opts.get("to").flatMap(points) match
          case Some(d) => d
          case None    => null
        t.vbox(toVal)
        proc.processTokenList(body) // scoping happens automatically from { } tokens
        t.mode.done()
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

// Read the <box> that follows \lower / \raise — the next \hbox or \vbox, built and returned
// *without* adding it to the current list, so the caller can wrap it (in a ShiftBox). Returns null
// for anything that is not a box command.
private def readBoxArg(proc: Processor, t: Typesetter, pos: CharReader): Box | Null =
  proc.skipSpaces()
  if !proc.hasMoreTokens then null
  else
    proc.peekToken() match
      case Token.ControlSeq(name, _) if name == "hbox" || name == "vbox" =>
        proc.nextToken() // consume the box command
        val opts = proc.readOptionalParams(pos)
        val body = proc.readArgument(pos)
        val toVal: Double | Null = opts.get("to").flatMap(points) match
          case Some(d) => d
          case None    => null
        if name == "hbox" then t.hbox(toVal) else t.vbox(toVal)
        proc.processTokenList(body)
        t.mode.exit
      case _ => null

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
