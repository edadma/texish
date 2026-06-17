package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.*

private[parser] def registerMathPrimitives(proc: Processor, handler: TypesetterHandler): Unit =
  val t = handler.typesetter

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

  // text - 1 body arg: a run of ordinary text set inside a formula. Math-mode only; the argument is typeset
  // through the normal text path (the string seam, in the surrounding body font) into a horizontal box, which
  // enters the math list as an Ord atom. This is how words appear in a formula upright — \text{E}[X], the
  // "for" in a piecewise definition — without the letters being mistaken for italic variables.
  proc.registerPrimitive(
    "text",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case parent: MathMode =>
            buildBox(proc, t, vertical = false, top = false, pos) match
              case b: Box => parent.add(b)
              case null   =>
          case _ => handler.error("\\text is only allowed in math mode", pos)
    },
  )

  // mathcal - 1 body arg: its letters set in the calligraphic (script) alphabet, as \mathcal{N} sets a script
  // N. Math-mode only; each letter is remapped to its Mathematical Script codepoint and added as an Ord atom,
  // so the whole run gets ordinary inter-atom spacing. A non-letter in the argument falls back to its usual
  // math classification.
  proc.registerPrimitive(
    "mathcal",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case parent: MathMode =>
            for tok <- stripOuterBraces(proc.readArgument(pos)) do
              tok match
                case Token.Text(s, _) =>
                  for ch <- s do
                    MathSymbols.mathcalNode(parent.mathFont, ch.toInt) match
                      case Some(node) => parent.addNode(node)
                      case None       => parent.addChar(ch.toInt)
                case _ =>
          case _ => handler.error("\\mathcal is only allowed in math mode", pos)
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

  // omit - 0 args: at the start of a cell, drop that cell's column template (it is set with no \hfil etc.),
  // as in TeX. Only meaningful inside a row.
  proc.registerPrimitive("omit", SimplePrimitive(() => t.op("omit")))

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

  // tabular - a LaTeX-style table written with a column spec instead of a hand-rolled \halign template. The spec
  // uses l, c, and r for left/centre/right columns and | for a vertical rule; in the body, rows are separated by
  // \\ and a horizontal rule is \hline. It lowers to \halign: the spec becomes the template line, each \\ becomes
  // \cr, and each \hline becomes \noalign{\hrule}.
  proc.registerPrimitive(
    "tabular",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val spec = stripOuterBraces(proc.readArgument(pos)).collect { case Token.Text(s, _) => s }.mkString
        val body = stripOuterBraces(proc.readArgument(pos))
        t.halign
        proc.processTokenList(tabularPreamble(proc, spec, pos) ++ (Token.ControlSeq("cr", pos) +: tabularBody(body)))
        t.done()
    },
  )

/** Translate a `\tabular` column spec (l/c/r columns, | rules) into an \halign template line. Each column letter
  * becomes a `#` placeholder with the alignment glue TeX uses — l = `#\hfil`, r = `\hfil#`, c = `\hfil#\hfil` — and
  * a `|` becomes a `\vrule` glued to the front of the following column (or, when it is the last character, to the
  * back of the final column). Columns are separated by the alignment-active `&`. */
private def tabularPreamble(proc: Processor, spec: String, pos: CharReader): Vector[Token] =
  val hfil = Token.ControlSeq("hfil", pos)
  val hash = Token.Active('#', pos)
  def vrule = Token.ControlSeq("vrule", pos)
  val columns      = Vector.newBuilder[Vector[Token]]
  var pendingRules = 0
  for c <- spec do
    c match
      case '|'                 => pendingRules += 1
      case 'l'                 => columns += Vector.fill(pendingRules)(vrule) ++ Vector(hash, hfil); pendingRules = 0
      case 'r'                 => columns += Vector.fill(pendingRules)(vrule) ++ Vector(hfil, hash); pendingRules = 0
      case 'c'                 => columns += Vector.fill(pendingRules)(vrule) ++ Vector(hfil, hash, hfil); pendingRules = 0
      case w if w.isWhitespace => // spaces in the spec are ignored
      case other               => proc.handler.error(s"Unknown column specifier '$other' in \\tabular", pos)
  var cols = columns.result()
  if cols.isEmpty then proc.handler.error("\\tabular needs at least one column", pos)
  if pendingRules > 0 then cols = cols.init :+ (cols.last ++ Vector.fill(pendingRules)(vrule))
  cols.zipWithIndex.flatMap { case (col, i) => if i == 0 then col else Token.Active('&', pos) +: col }

/** Rewrite a `\tabular` body into `\halign` row syntax: `\\` ends a row (`\cr`) and `\hline` draws a rule across
  * the whole table (`\noalign{\hrule}`). A single space or newline immediately after `\hline` is dropped so the
  * rule does not open the next row with a blank leading cell. */
private def tabularBody(body: Vector[Token]): Vector[Token] =
  val out = Vector.newBuilder[Token]
  var i   = 0
  while i < body.length do
    body(i) match
      case Token.ControlSeq("\\", p) => out += Token.ControlSeq("cr", p)
      case Token.ControlSeq("hline", p) =>
        out ++= Vector(Token.ControlSeq("noalign", p), Token.BeginGroup(p), Token.ControlSeq("hrule", p), Token.EndGroup(p))
        if i + 1 < body.length then
          body(i + 1) match
            case Token.Space(_, _) | Token.Newline(_) => i += 1
            case _                                     =>
      case other => out += other
    i += 1
  out.result()

// Read an optional bracketed argument like \sqrt's [degree]. Brackets are ordinary text characters, not
// tokenizer-special, so the opening '[' may share a text token with the degree and the closing ']' may sit
// mid-token; this scans across tokens, splitting text runs at the brackets and pushing back any tail after
// ']'. Returns None when the next token is not a '[' run (no degree present), leaving the stream untouched.
private[parser] def readOptionalDegree(proc: Processor): Option[Vector[Token]] =
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
private[parser] def splitMatrixBody(body: Vector[Token]): Vector[Vector[Vector[Token]]] =
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
private[parser] def readDelimiter(proc: Processor, handler: TypesetterHandler, pos: CharReader): Option[Int] =
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
