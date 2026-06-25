package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.*

/** Font-shape and decorated-box primitives: the shape-wrapping commands (\bold / \italic / \smallcaps /
  * \slanted), the LaTeX shape and family declarations (\itshape / \bfseries / \rmfamily / …), the role-text
  * forms (\texttt / \textsf / \textrm), \glyphwidth, text scripts (\textsub / \textsup), verbatim and code
  * (\verb / \code), \underline, and the framing and transform boxes (\fbox / \colorbox / \rotatebox /
  * \scalebox / \resizebox / \raisebox and friends). */
private[parser] def registerFontShapePrimitives(proc: Processor, handler: TypesetterHandler): Unit =
  val t = handler.typesetter

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

  // underline - 1 body arg. In math mode it draws a full-width rule under a math sub-formula (the companion of
  // \overline); in text it wraps the content in an underlined hbox. The rule case keeps the content's class
  // spacing by entering as an Ord atom.
  proc.registerPrimitive(
    "underline",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case parent: MathMode =>
            val inner = handler.mathSubFormula(proc, parent.style, proc.readArgument(pos))
            if inner ne null then parent.addNode(MathAtom(MathClass.Ord, parent.makeBar(inner, over = false)))
          case _ =>
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
