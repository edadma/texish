package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.*
import io.github.edadma.qr.{Ecc, QrCode}

/** Box-construction primitives: vertical and horizontal glue (\vskip / \hskip) and rules (\hrule / \vrule),
  * the box builders (\hbox / \vbox / \vtop / \mbox / \makebox / \parbox and the \minipage pair), length
  * variables (\newlength / \setlength / \addtolength), the phantom family, balanced \columns, and the
  * \geometry page-frame setup. */
private[parser] def registerBoxPrimitives(proc: Processor, handler: TypesetterHandler): Unit =
  val t = handler.typesetter

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

  // hskip - glue spec: dimension with optional plus/minus continuation, braced glue, or glue variable. Horizontal
  // glue, so in vertical mode it begins a paragraph (leaveVmode) rather than joining the vertical list — the
  // counterpart of \vskip, which ends a paragraph.
  proc.registerPrimitive(
    "hskip",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val argPos = argumentPos(proc, pos)
        glueArg(proc, pos) match
          case Some(g) => t.leaveVmode; t.add(g)
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

  // mbox - 1 body arg: an unbreakable horizontal box of its content's natural width, the LaTeX name for a bare
  // \hbox. Inside a paragraph it keeps its content on one line; nothing inside it can break.
  proc.registerPrimitive(
    "mbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        captureHBox(proc, t, handler, pos) match
          case b: Box => t.add(b)
          case null   =>
    },
  )

  // makebox - optional [width] then [pos], then 1 body arg: an hbox of an explicit width with its content aligned
  // inside. With no [width] it is exactly \mbox (natural width). [pos] is l (flush left), r (flush right), c
  // (centred, the default) or s (stretch the content's own glue to fill); the width is a dimension or a factor of
  // \linewidth. Built by padding the content with fil glue on the empty side(s) inside an hbox set to the width.
  proc.registerPrimitive(
    "makebox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val widthSpec = proc.readOptionalArg(pos).map(tokensToSource).map(_.trim).filter(_.nonEmpty)
        val alignSpec = proc.readOptionalArg(pos).map(tokensToSource).map(_.trim).filter(_.nonEmpty)
        val body      = proc.readArgument(pos)

        handler.flushPendingSpace()

        widthSpec match
          case None =>
            t.hbox(null)
            proc.processTokenList(body)
          case Some(ws) =>
            val w     = resolveLength(proc, t, ws, pos, "\\makebox")
            val align = alignSpec.map(_.charAt(0)).getOrElse('c')

            t.hbox(w)
            if align == 'c' || align == 'r' then t.fil
            proc.processTokenList(body)
            if align == 'c' || align == 'l' then t.fil

        t.mode.exit match
          case b: Box => t.add(b)
          case null   =>
    },
  )

  // parbox - optional [pos], [height] and [inner-pos], then {width} and {body}: typeset the body — one paragraph or
  // several — in a vertical box of the given width, so a block of text sits in the running line like a single box.
  // [pos] aligns the box on the surrounding baseline: t (first line's baseline), b (last line's baseline), or c
  // (vertical centre, the default). The width is a dimension or a factor of \linewidth. With the full LaTeX
  // signature, [height] fixes the box height and [inner-pos] places the content within it — t (top), c (centre), b
  // (bottom), or s (stretch the content's own glue) — defaulting to [pos]; the box then stays that tall regardless
  // of the text (which overflows if it does not fit). \minipage is the environment form, over the same machinery.
  proc.registerPrimitive(
    "parbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val align     = proc.readOptionalArg(pos).map(tokensToSource).map(_.trim).filter(_.nonEmpty).map(_.charAt(0)).getOrElse('c')
        val heightRaw = proc.readOptionalArg(pos).map(tokensToSource).map(_.trim).filter(_.nonEmpty)
        val inner     = proc.readOptionalArg(pos).map(tokensToSource).map(_.trim).filter(_.nonEmpty).map(_.charAt(0)).getOrElse(align)
        val widthRaw  = tokensToSource(proc.readArgument(pos))
        val body      = proc.readArgument(pos)
        val w         = resolveLength(proc, t, widthRaw, pos, "\\parbox")
        val height    = heightRaw.map(h => resolveLength(proc, t, h, pos, "\\parbox"))

        handler.flushPendingSpace()
        val saved = t.getNumber("hsize")
        t.set("hsize", w)
        openFixedVbox(t, align, height, inner)
        proc.processTokenList(body)
        t.paragraph()
        closeFixedVbox(t, height, inner)
        val box = t.mode.exit
        t.set("hsize", saved)
        // the body is self-contained: a source newline at its end left the pending-newline state set, and it must
        // not leak out to the surrounding paragraph (where it would turn the next newline into a spurious break)
        handler.flushPendingSpace()

        box match
          case vb: VerticalBox => t.add(alignParbox(vb, align))
          case _               =>
    },
  )

  // \minipage's begin/end pair: \beginminipage [pos] [height] [inner-pos] {width} opens a vertical box of the given
  // width and routes the environment body into it; \endminipage closes it and contributes the finished box, aligned
  // by [pos] like \parbox. It takes the same optional [height] / [inner-pos] as \parbox for a fixed-height box. The
  // saved state rides a stack so minipages can nest. Wired to \begin{minipage}/\end{minipage} by the document
  // package.
  val minipageStack = scala.collection.mutable.Stack[(Double, Char, Option[Double], Char)]() // (saved hsize, align, height, inner)

  proc.registerPrimitive(
    "beginminipage",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val align     = proc.readOptionalArg(pos).map(tokensToSource).map(_.trim).filter(_.nonEmpty).map(_.charAt(0)).getOrElse('c')
        val heightRaw = proc.readOptionalArg(pos).map(tokensToSource).map(_.trim).filter(_.nonEmpty)
        val inner     = proc.readOptionalArg(pos).map(tokensToSource).map(_.trim).filter(_.nonEmpty).map(_.charAt(0)).getOrElse(align)
        val widthRaw  = tokensToSource(proc.readArgument(pos))
        val w         = resolveLength(proc, t, widthRaw, pos, "\\minipage")
        val height    = heightRaw.map(h => resolveLength(proc, t, h, pos, "\\minipage"))

        minipageStack.push((t.getNumber("hsize"), align, height, inner))
        handler.flushPendingSpace()
        t.set("hsize", w)
        openFixedVbox(t, align, height, inner)
    },
  )

  proc.registerPrimitive(
    "endminipage",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        if minipageStack.isEmpty then handler.error("\\endminipage without a matching \\beginminipage", pos)
        else
          val (savedHsize, align, height, inner) = minipageStack.pop()

          t.paragraph()
          closeFixedVbox(t, height, inner)
          val box = t.mode.exit
          t.set("hsize", savedHsize)
          // clear the pending-newline state the body's final newline left, so it does not leak a spurious
          // paragraph break into the text after the minipage (as it does for \parbox)
          handler.flushPendingSpace()

          box match
            case vb: VerticalBox => t.add(alignParbox(vb, align))
            case _               =>
    },
  )

  // newlength / setlength / addtolength - a length is an ordinary dimension variable; these are the LaTeX names for
  // declaring and adjusting one. \newlength{name} initialises it to 0pt, \setlength{name}{dimen} assigns it, and
  // \addtolength{name}{dimen} adds to it. The name is a plain variable (LaTeX's leading backslash is not needed),
  // so the length is read back as \name or with \the\name and used in \calc like any \set variable. Each honours a
  // leading \global the same way \set does.
  def lengthAssign(proc: Processor, name: String, value: Value): Unit =
    val global = proc.handler.globalAssign
    proc.handler.globalAssign = false
    if global then proc.handler.setGlobal(name, value) else proc.handler.set(name, value)

  proc.registerPrimitive(
    "newlength",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        lengthAssign(proc, Value.display(evalArg(proc, pos)), Value.Dimen(0))
    },
  )
  proc.registerPrimitive(
    "setlength",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = Value.display(evalArg(proc, pos))
        lengthAssign(proc, name, proc.evalArgumentExpr(pos))
    },
  )
  proc.registerPrimitive(
    "addtolength",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = Value.display(evalArg(proc, pos))
        val inc  = points(proc.evalArgumentExpr(pos)).getOrElse(handler.error("\\addtolength expects a dimension", pos))
        val cur  = points(proc.handler.get(name)).getOrElse(0.0)
        lengthAssign(proc, name, Value.Dimen(cur + inc))
    },
  )

  // phantom / hphantom / vphantom / smash - 1 body arg: reserve the size of the argument along selected axes
  // without its full ink. \phantom leaves an invisible box the exact size of its argument; \hphantom keeps only
  // its width; \vphantom only its height and depth (the classic way to make a row of fractions share a baseline,
  // or align \sqrt signs); \smash draws the argument but reports zero height and depth, so tall material overlaps
  // its neighbours instead of spreading the line. The argument is set in the current mode — as a math sub-formula
  // inside a formula, as ordinary horizontal material otherwise — so a phantom matches what it stands in for.
  def phantomPrimitive(name: String, keepWidth: Boolean, keepHeight: Boolean, visible: Boolean): Unit =
    proc.registerPrimitive(
      name,
      new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit =
          t.mode match
            case parent: MathMode =>
              handler.mathSubFormula(proc, parent.style, proc.readArgument(pos)) match
                case inner: Box => parent.add(new PhantomBox(inner, keepWidth, keepHeight, visible))
                case null       =>
            case _ =>
              val inner = HBox(typesetGroupBoxes(proc, t, pos).toVector)
              t.add(new PhantomBox(inner, keepWidth, keepHeight, visible))
      },
    )

  phantomPrimitive("phantom", keepWidth = true, keepHeight = true, visible = false)
  phantomPrimitive("hphantom", keepWidth = true, keepHeight = false, visible = false)
  phantomPrimitive("vphantom", keepWidth = false, keepHeight = true, visible = false)
  phantomPrimitive("smash", keepWidth = true, keepHeight = false, visible = true)

  // columns - [gap:<dim>] {n} {body}: set the body as n balanced side-by-side columns. The body is typeset once
  // into a vbox at the column width — the current \hsize less the gutters, divided by n — then divided into n
  // pieces of roughly equal height with \vsplit's page-style breaking, and the pieces are set side by side with
  // the gutter between them. This is column balancing (the columns come out level), as opposed to filling one
  // column to the bottom before starting the next. The whole thing enters the current list as a single
  // horizontal box, so it sits within the ordinary page; a balanced block taller than the page is not yet split
  // across pages. The gutter defaults to one em, overridable with [gap:<dim>].
  proc.registerPrimitive(
    "columns",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val opts = proc.readOptionalParams(pos)
        val gap  = opts.get("gap").flatMap(points).getOrElse(t.currentFont.size)
        val n    = math.max(1, argInt(proc, pos))
        val body = proc.readArgument(pos)

        val hsize    = t.getNumber("hsize")
        val colWidth = (hsize - (n - 1) * gap) / n

        // typeset the body once into a vbox at the column width, then restore the page measure
        t.set("hsize", colWidth)
        t.vbox()
        proc.processTokenList(body)
        t.paragraph()
        val full = t.mode.exit
        t.set("hsize", hsize)

        full match
          case vb: VerticalBox =>
            // Balance the material across the columns by line count. Each column takes ceil(remaining lines /
            // remaining columns), so any surplus from an uneven division lands in the EARLIER columns and the
            // last column is the shortest — the newspaper convention. Each column is a \vtop, so its reference
            // point is the first line's baseline: stood side by side in the hbox, the columns align along their
            // tops, not their (unequal) last baselines.
            //
            // Counting lines, rather than targeting a column height, sidesteps a measurement trap: a column
            // split off as the trailing remainder has its leading interline glue stripped, so the same lines
            // measure a hair shorter than they would as a prefix. A height target tuned to that short remainder
            // leaves the earlier (prefix) columns one line short of it, which dumped the extra line into the
            // last column and left it the tallest.
            val legalBreak: (Vector[Box], Int) => Boolean = (list, i) =>
              list(i) match
                case p: Penalty   => p.penalty < Penalty.Inhibit
                case g: Glue      => !g.nobreak && i > 0 && !list(i - 1).isSpace
                case _: VSpaceBox => i > 0 && !list(i - 1).isSpace
                case _            => false

            var rest = vb.boxes.toVector
            val cols = Vector.newBuilder[Box]
            for col <- 0 until n - 1 do
              val breaks = (1 until rest.length).filter(i => legalBreak(rest, i))
              val take   = math.ceil((breaks.size + 1).toDouble / (n - col)).toInt
              val cutPos = if take - 1 < breaks.size then breaks(take - 1) else rest.length
              cols += new VTop(rest.take(cutPos))
              rest = rest.drop(cutPos).dropWhile(_.isSpace)
            cols += new VTop(rest)

            // lay the columns out left to right with the gutter between adjacent pairs
            val laid = cols.result().zipWithIndex.flatMap {
              case (c, 0) => Seq(c)
              case (c, _) => Seq(HSpaceBox(gap), c)
            }
            t.add(new HBox(laid))
          case _ =>
    },
  )

  // geometry - LaTeX geometry-style page setup, taken as texish name:value options rather than a {key=value,…}
  // list. It resolves the page frame and sets the underlying variables: the physical sheet (paper / paperwidth /
  // paperheight / landscape / portrait) and, on each axis independently, the margins and text size from whichever
  // of {margin, hmargin/vmargin, left/right, top/bottom, textwidth/textheight (or width/height), centering} were
  // given. The settings are global, so a \geometry in the preamble holds for the whole document. Examples:
  //   \geometry paper:a4 margin:1in
  //   \geometry left:1.5in right:1in top:1in bottom:1in
  //   \geometry textwidth:6in centering:on
  //   \geometry paper:a4 landscape:on
  proc.registerPrimitive(
    "geometry",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val opts = proc.readOptionalParams(pos)

        def dim(key: String): Option[Double]  = opts.get(key).flatMap(Value.number)
        def flag(key: String): Boolean = opts.get(key).exists { v =>
          Value.display(v).trim.toLowerCase match
            case "off" | "false" | "no" | "0" | "" => false
            case _                                 => true
        }

        // The physical sheet: a named size, then explicit overrides, then an orientation swap.
        var pw = t.getNumber("paperwidth")
        var ph = t.getNumber("paperheight")
        opts.get("paper").map(Value.display).foreach { name =>
          t.paperSize(name) match
            case Some((w, h)) => pw = w; ph = h
            case None => handler.error(s"\\geometry: unknown paper size '$name' (letter, legal, a3, a4, a5, a6)", pos)
        }
        dim("paperwidth").foreach(pw = _)
        dim("paperheight").foreach(ph = _)
        if flag("landscape") && pw < ph then { val s = pw; pw = ph; ph = s }
        if flag("portrait") && pw > ph then { val s = pw; pw = ph; ph = s }

        val centerAll = flag("centering")
        val (hoff, hs) = resolveGeometryAxis(pw, dim("margin").orElse(dim("hmargin")),
          dim("left"), dim("right"), dim("textwidth").orElse(dim("width")),
          centerAll || flag("hcentering"), t.getNumber("hoffset"), t.getNumber("hsize"))
        val (voff, vs) = resolveGeometryAxis(ph, dim("margin").orElse(dim("vmargin")),
          dim("top"), dim("bottom"), dim("textheight").orElse(dim("height")),
          centerAll || flag("vcentering"), t.getNumber("voffset"), t.getNumber("vsize"))

        if hs <= 0 then handler.error(s"\\geometry: the horizontal margins leave no room for text", pos)
        else if vs <= 0 then handler.error(s"\\geometry: the vertical margins leave no room for text", pos)
        else
          t.setGlobal("paperwidth", pw)
          t.setGlobal("paperheight", ph)
          t.setGlobal("hoffset", hoff)
          t.setGlobal("hsize", hs)
          t.setGlobal("voffset", voff)
          t.setGlobal("vsize", vs)
          dim("headsep").foreach(t.setGlobal("headsep", _))
          dim("footskip").foreach(t.setGlobal("footskip", _))
    },
  )

  // arrange - choose the page arrangement (output routine): how finished logical pages are placed onto physical
  // sheets. The default is one page per sheet; `two-up-booklet` sets pages two-up in saddle-stitch folding order on a
  // sheet twice as wide (each sheet folded once), `four-up-booklet` stacks that layout two sheets high on a sheet
  // twice as tall as well (print, cut in half, then fold as the two-up booklet), and `nup` tiles a grid. It must
  // appear in the preamble, before any content, because the arrangement fixes the physical sheet size when the output
  // surface is created. Examples:
  //   \arrange two-up-booklet               % \geometry paper:a6 pages, two-up on an A5-landscape sheet
  //   \arrange four-up-booklet              % A6 pages four-up on an A4 sheet, to cut in half and fold
  //   \arrange four-up-booklet duplexshift:3mm % pull the front side 3mm left to cancel duplex misregistration
  //   \arrange two-up-booklet signature:16  % fold into 16-page groups instead of one nested stack
  //   \arrange two-up-booklet binding:right % right-bound (Arabic/Hebrew) — spine and cover on the right
  //   \arrange nup rows:2 cols:2            % four logical pages to a sheet
  proc.registerPrimitive(
    "arrange",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val mode = proc
          .readArgument(pos)
          .map {
            case Token.Text(s, _)  => s
            case Token.Space(s, _) => s
            case _                 => ""
          }
          .mkString
          .trim
          .toLowerCase
        val opts = proc.readOptionalParams(pos)
        def intOpt(key: String): Option[Int]    = opts.get(key).flatMap(Value.number).map(_.toInt)
        def dimOpt(key: String): Option[Double] = opts.get(key).flatMap(Value.number)
        // binding chooses which edge the book is bound on: left (the default, spine on the left) or right (Arabic,
        // Hebrew and other right-to-left books, spine and front cover on the right). Right-binding mirrors the
        // saddle-stitch imposition so the pages read back to front. Returns None for an unrecognised value so the
        // caller can report it against the specific arrangement.
        def rightBoundOpt: Option[Boolean] = opts.get("binding").map(Value.display).map(_.toLowerCase) match
          case None          => Some(false)
          case Some("left")  => Some(false)
          case Some("right") => Some(true)
          case Some(_)       => None

        if t.contentStarted then handler.error("\\arrange must appear before any page content", pos)
        else
          mode match
            case "simple" => t.getDocument.arrangement = SimpleArrangement
            case "two-up-booklet" =>
              (intOpt("signature"), rightBoundOpt) match
                case (Some(s), _) if s <= 0 || s % 4 != 0 =>
                  handler.error("\\arrange two-up-booklet: signature must be a positive multiple of 4", pos)
                case (_, None) =>
                  handler.error("\\arrange two-up-booklet: binding must be left or right", pos)
                case (sig, Some(rb)) =>
                  t.getDocument.arrangement = new TwoUpBookletArrangement(sig, dimOpt("duplexshift").getOrElse(0.0), rb)
            case "four-up-booklet" =>
              (intOpt("signature"), rightBoundOpt) match
                case (Some(s), _) if s <= 0 || s % 4 != 0 =>
                  handler.error("\\arrange four-up-booklet: signature must be a positive multiple of 4", pos)
                case (_, None) =>
                  handler.error("\\arrange four-up-booklet: binding must be left or right", pos)
                case (sig, Some(rb)) =>
                  t.getDocument.arrangement = new FourUpBookletArrangement(sig, dimOpt("duplexshift").getOrElse(0.0), rb)
            case "nup" =>
              val rows = intOpt("rows").getOrElse(1)
              val cols = intOpt("cols").getOrElse(1)
              if rows < 1 || cols < 1 then handler.error("\\arrange nup: rows and cols must be positive", pos)
              else t.getDocument.arrangement = new NupArrangement(rows, cols)
            case other =>
              handler.error(s"\\arrange: unknown arrangement '$other' (simple, two-up-booklet, four-up-booklet, nup)", pos)
    },
  )

  // qrcode - {data} plus optional params: encode the argument text as a QR Code and add it as a square box of
  // filled cells. The symbol is drawn in vector rectangles (crisp on every backend) with a light quiet zone around
  // it. Options: ecc (l/m/q/h, default q — the smudge-tolerant quartile level), cell (per-module size, default
  // 3pt), quiet (border width in modules, default 4), dark and light (colours; dark defaults to the current colour
  // like a rule, light to white — use light:none for a transparent background). Examples:
  //   \qrcode{https://example.com}
  //   \qrcode[ecc:h cell:4pt dark:navy]{HELLO}
  proc.registerPrimitive(
    "qrcode",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        // Both the [options] and the {data} are read verbatim off live input, in that order: the data is a URL as
        // often as not, whose // would otherwise start a comment (and %, ~ and friends must survive into the
        // encoded bytes unchanged), so it is taken raw exactly as \href/\url take their targets. A raw body cannot
        // follow token-parsed options, so the bracket is parsed by hand into simple space-separated key:value pairs.
        val opts: Map[String, String] =
          proc.readOptionalRawBracket() match
            case None => Map.empty
            case Some(s) =>
              s.trim.split("\\s+").filter(_.nonEmpty).map { entry =>
                entry.indexOf(':') match
                  case -1 => handler.error(s"\\qrcode: option '$entry' must be key:value", pos)
                  case i  => entry.substring(0, i) -> entry.substring(i + 1)
              }.toMap
        val data = proc.readRawArgument(pos)

        val ecc = opts.get("ecc").map(_.toLowerCase) match
          case Some("l") => Ecc.Low
          case Some("m") => Ecc.Medium
          case Some("q") => Ecc.Quartile
          case Some("h") => Ecc.High
          case None      => Ecc.Quartile
          case Some(bad) => handler.error(s"\\qrcode: unknown ecc level '$bad' (l, m, q, h)", pos)

        val cell  = opts.get("cell").map(s => resolveLength(proc, t, s, pos, "\\qrcode")).getOrElse(3.0)
        val quiet = opts.get("quiet").map(_.toInt).getOrElse(4)
        val dark  = opts.get("dark").map(Color(_)).getOrElse(t.currentColor)
        val light = opts.get("light").map(_.toLowerCase) match
          case Some("none") | Some("transparent") => Color.TRANSPARENT
          case Some(name)                         => Color(name)
          case None                               => Color("white")

        if cell <= 0 then handler.error("\\qrcode: cell size must be positive", pos)
        else if quiet < 0 then handler.error("\\qrcode: quiet zone must be non-negative", pos)
        else t.add(new QrBox(QrCode.encodeText(data, ecc), cell, quiet, dark, light))
    },
  )
