package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.*

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

  // \opacity{v} sets fill and stroke opacity together; \fillopacity / \strokeopacity set one. The value is a
  // multiplier (0 transparent, 1 opaque) on the paint's own alpha, applied when a shape is drawn.
  picturePrimitive(
    proc,
    handler,
    "opacity",
    (pm, p) =>
      val v = num1(proc, p); pm.setFillOpacity(v); pm.setStrokeOpacity(v),
  )
  picturePrimitive(proc, handler, "fillopacity", (pm, p) => pm.setFillOpacity(num1(proc, p)))
  picturePrimitive(proc, handler, "strokeopacity", (pm, p) => pm.setStrokeOpacity(num1(proc, p)))
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
private[parser] def picturePrimitive(
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

private[parser] def requirePicture(t: Typesetter, handler: TypesetterHandler, name: String, pos: CharReader): PictureMode =
  t.mode match
    case pm: PictureMode => pm
    case _               => handler.error(s"\\$name is only allowed inside \\picture", pos)

// Read a coordinate group as a flat list of points (in the engine's point space). Each whitespace-separated
// piece is either a parenthesised coordinate — Cartesian `(x,y)`, polar `(a:r)`, or a named `(name)`, each
// contributing two numbers (see `Coord`) — or a bare scalar expression contributing one (a literal `2in`, a
// variable `\the\x`, a computed `\*{a}{b}` or `\calc{…}`). So `\line{(0,0) (60:1in)}` and `\line{0 0 36 62}`
// produce the same flat stream the shape primitives consume, and the two notations interoperate.
private[parser] def readNumbers(proc: Processor, pos: CharReader): Vector[Double] =
  splitTopLevel(stripOuterBraces(proc.readArgument(pos))).flatMap { chunk =>
    val text = coordText(chunk)
    if Coord.looksLikeCoord(text) then
      val (x, y) = Coord.parse(text, varResolver(proc), proc.handler.fontUnit, namedResolver(proc))
      Vector(x, y)
    else Vector(points(proc.evalExpr(chunk, pos)).getOrElse(0.0))
  }

// Reconstruct a chunk's raw text for the coordinate parser: a control sequence contributes its bare name (so a
// variable `\R` reads as the identifier `R`) and an active character its symbol, matching how `\calc` flattens.
private[parser] def coordText(tokens: Vector[Token]): String =
  tokens.map {
    case Token.Text(s, _)       => s
    case Token.Space(s, _)      => s
    case Token.Newline(_)       => " "
    case Token.ControlSeq(n, _) => n
    case Token.Active(c, _)     => c.toString
    case _                      => ""
  }.mkString

// Resolve a bare identifier in a coordinate component expression to a document variable's number.
private[parser] def varResolver(proc: Processor): String => Option[Double] = name =>
  Value.number(proc.handler.get(name))

// Resolve a `(name)` reference to a point stored by \coordinate (a two-element numeric sequence).
private[parser] def namedResolver(proc: Processor): String => Option[(Double, Double)] = name =>
  proc.handler.get(name) match
    case Value.Seq(Vector(Value.Num(x), Value.Num(y))) => Some((x, y))
    case _                                             => None

// Read a single-number group, e.g. \linewidth{2pt} or \rotate{30}.
private[parser] def num1(proc: Processor, pos: CharReader): Double =
  points(proc.evalArgumentExpr(pos)).getOrElse(0.0)

// Split a coordinate group into its whitespace-separated pieces, keeping each piece intact across a brace group
// (`\*{a}{b}`) and across a parenthesised coordinate (`(60:1in)`, even with an internal space like `(2, 3)`).
// Brace depth is tracked from the group tokens; parenthesis depth from the characters of text tokens outside any
// braces, since parentheses are ordinary text. A whitespace token splits only when both depths are zero.
private[parser] def splitTopLevel(tokens: Vector[Token]): Vector[Vector[Token]] =
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

private[parser] def coordPairs(ns: Vector[Double]): Vector[(Double, Double)] =
  ns.grouped(2).collect { case Vector(x, y) => (x, y) }.toVector

// Evaluate a color argument: a named color (steelblue) or an #rrggbb hex code.
private[parser] def readColorArg(proc: Processor, pos: CharReader): Color =
  Color(Value.display(proc.evalArgumentExpr(pos)))

// Read an optional anchor:NAME parameter before a placement's coordinates.
private[parser] def readAnchor(proc: Processor, pos: CharReader, default: Anchor): Anchor =
  proc.readOptionalParams(pos).get("anchor").flatMap(v => Anchor.fromString(Value.display(v))).getOrElse(default)
