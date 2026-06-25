package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.*

/** Box-register and rigid-spacing primitives: the box registers (\setbox / \box / \copy / \vsplit / \unhbox /
  * \unvbox / \wd / \ht / \dp / \isvoid), rigid horizontal space (\kern), author break points (\discretionary /
  * \softhyphen), the leader family (\leaders / \dotfill / \hrulefill), the TeX math spaces (\, \: \; \! \quad
  * \qquad), and box shifts (\lower / \raise). */
private[parser] def registerRegisterPrimitives(proc: Processor, handler: TypesetterHandler): Unit =
  val t = handler.typesetter

  // setbox name \hbox{...} (or \vbox / \vtop) - typeset a box now and save it in a register under `name`, for
  // later measurement (\wd / \ht / \dp) and placement (\box / \copy). Like \set, the assignment is local to the
  // current group. The box's contents are typeset at this point, not when the register is later used.
  proc.registerPrimitive(
    "setbox",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = proc.readIdentifier(pos)
        readBoxArg(proc, handler, t, pos) match
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

  // vsplit name to:<height> - split a saved vbox at the latest legal breakpoint no taller than the height, the
  // way a page breaks: the top piece is produced (added to the current list when used on its own, or captured by
  // a surrounding \setbox), and the remainder is left in the register for the next split. This is how a long
  // vertical list — overflowing footnotes, a column to balance — is divided into page- or column-sized pieces.
  proc.registerPrimitive(
    "vsplit",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        vsplitBox(proc, handler, t, pos) match
          case b: Box => t.add(b)
          case null   =>
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

  // isvoid name - true when the named box register holds no box: either it was never \setbox, or it has been used
  // up (\box empties it, and \vsplit empties the source once nothing remains to split). The predicate, unlike \box
  // or \copy, never errors on an empty register, so a document can test before placing — \if {\isvoid col3}...\fi
  // guards against a body too short to fill every column. Mirrors TeX's \ifvoid.
  proc.registerPrimitive(
    "isvoid",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = proc.readIdentifier(pos)
        proc.setResult(Value.Bool(proc.handler.get(name) match
          case Value.Native(_: Box) => false
          case _                    => true))
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
        readBoxArg(proc, handler, t, pos) match
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
            readBoxArg(proc, handler, t, pos) match
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
            readBoxArg(proc, handler, t, pos) match
              case b: Box => t.add(ShiftBox(b, -d))
              case null   => handler.error("\\raise expects a box (\\hbox or \\vbox)", argumentPos(proc, pos))
          case None => handler.error("\\raise expects a dimension", argPos)
    },
  )
