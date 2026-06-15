package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
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
// back under the T, then an X kerned back under the E. The three capitals ride the glyph seam (placed by
// their own advances) so no string-shaping creeps between them; the kern fractions are the classic plain-TeX
// values. TeXish adds "ish" trailing, set smaller and raised — echoing the lifted "A" of the LaTeX logo.
private[parser] def texLogo(t: Typesetter, ish: Boolean): Box =
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

// A dimension value in big points: Dimen carries its own unit; a bare number means points
private[parser] def points(v: Value): Option[Double] = v match
  case Value.Dimen(p) => Some(p.toDouble)
  case Value.Num(n)   => Some(n.toDouble)
  case _              => None

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
