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

  // \coordinate{name}{coord} - name a point for later reference as (name). Stored as a Value.Point in the document
  // scope, so it reads back through the same variable machinery everything else uses and \the prints it as "(x, y)".
  picturePrimitive(
    proc,
    handler,
    "coordinate",
    (_, p) =>
      val name = Value.display(proc.evalArgumentExpr(p))
      val c    = readNumbers(proc, p)
      if c.length < 2 then handler.error(s"\\coordinate '$name' expects a point", p)
      t.set(name, Value.Point(c(0), c(1))),
  )

  // \point{coord} - evaluate a coordinate to a first-class point value (for \set, and for \xof / \yof). Unlike
  // \coordinate it does not name or store anything and is not picture-only: \set m {\point{(2,3)}} keeps a point in
  // an ordinary variable, which \the renders as "(2, 3)".
  proc.registerPrimitive(
    "point",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val c = readNumbers(proc, pos)
        proc.setResult(Value.Point(if c.nonEmpty then c(0) else 0.0, if c.length > 1 then c(1) else 0.0))
    },
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

  // Point arithmetic — small vector operations on points, so coordinate geometry (bond directions, offsets,
  // midpoints) is written in the document language instead of unpacked into per-component \calc. Each argument is
  // any coordinate (a literal, a named point, or a nested point expression); the result is a Value.Point that drops
  // straight into coordinate position, or a number for \pdist. Not picture-only — these are pure value computations.
  def pointBinary(name: String, op: ((Double, Double), (Double, Double)) => Value): Unit =
    proc.registerPrimitive(
      name,
      new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit =
          val a = readPoint(proc, pos)
          val b = readPoint(proc, pos)
          proc.setResult(op(a, b))
      },
    )
  def pointUnary(name: String, op: ((Double, Double)) => Value): Unit =
    proc.registerPrimitive(
      name,
      new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit =
          proc.setResult(op(readPoint(proc, pos)))
      },
    )

  pointBinary("padd", (a, b) => Value.Point(a._1 + b._1, a._2 + b._2)) // a + b
  pointBinary("psub", (a, b) => Value.Point(a._1 - b._1, a._2 - b._2)) // a − b (the vector from b to a)
  pointBinary("pmid", (a, b) => Value.Point((a._1 + b._1) / 2, (a._2 + b._2) / 2))
  pointBinary("pdist", (a, b) => Value.Num(math.hypot(b._1 - a._1, b._2 - a._2)))
  pointUnary("pperp", p => Value.Point(-p._2, p._1)) // a quarter-turn counter-clockwise
  pointUnary(
    "pnormalize",
    p =>
      val len = math.hypot(p._1, p._2)
      if len == 0 then Value.Point(0, 0) else Value.Point(p._1 / len, p._2 / len),
  )
  // \pscale{point}{factor} - the point scaled by a scalar, for stepping a unit vector out to a length.
  proc.registerPrimitive(
    "pscale",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val p = readPoint(proc, pos)
        val s = num1(proc, pos)
        proc.setResult(Value.Point(p._1 * s, p._2 * s))
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
  // it with the current state. The segment commands are themselves picture-only. An optional arrow: end|start|both
  // (with the same head:/size: options as \arrow) caps the path with an arrowhead oriented to its true end tangent —
  // for a Bézier the last control point → endpoint, for an arc perpendicular to the end radius — not the straight
  // start→end chord, so a curved arrow points the way the curve actually leaves the page.
  picturePrimitive(
    proc,
    handler,
    "path",
    (pm, p) => {
      val opts   = proc.readOptionalParams(p)
      val arrows = opts.get("arrow").map(Value.display).getOrElse("")
      pm.beginTrackedPath()
      pm.newPath()
      proc.processTokenList(proc.readArgument(p))
      pm.paint()
      pm.endTrackedPath()
      if arrows.nonEmpty then
        if arrows != "end" && arrows != "start" && arrows != "both" then
          handler.error(s"unknown \\path arrow '$arrows' (end, start, both)", p)
        val style = arrowStyleOpt(handler, opts, p)
        val len   = opts.get("size").flatMap(points).getOrElse(7.0)
        if arrows == "end" || arrows == "both" then
          pm.pathEndAnchor.foreach((end, dir) => emitArrowhead(pm, end._1 - dir._1, end._2 - dir._2, end._1, end._2, style, len))
        if arrows == "start" || arrows == "both" then
          pm.pathStartAnchor.foreach((st, dir) => emitArrowhead(pm, st._1 + dir._1, st._2 + dir._2, st._1, st._2, style, len))
    },
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

  // \arrow[head: size: heads:]{a b} - a shaft from a to b plus an arrowhead, drawn in the current \stroke colour
  // (an arrow is ink: shaft and head share the pen, \fill is ignored). The shaft is shortened at each headed end so
  // the line meets the back of the head rather than poking through its tip. heads = end (default) | start | both.
  proc.registerPrimitive(
    "arrow",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val pm    = requirePicture(t, handler, "arrow", pos)
        val opts  = proc.readOptionalParams(pos)
        val style = arrowStyleOpt(handler, opts, pos)
        val len   = opts.get("size").flatMap(points).getOrElse(7.0)
        val heads = opts.get("heads").map(Value.display).getOrElse("end")
        val c     = readNumbers(proc, pos)
        if c.length < 4 then handler.error("\\arrow expects two points {a b}", pos)
        val (ax, ay, bx, by) = (c(0), c(1), c(2), c(3))
        val d        = math.hypot(bx - ax, by - ay)
        val (ux, uy) = if d == 0 then (0.0, 0.0) else ((bx - ax) / d, (by - ay) / d)
        val ink      = pm.strokeColor.orElse(pm.fillColor).getOrElse(Color("black"))

        val insetEnd   = if heads == "end"   || heads == "both" then emitArrowhead(pm, ax, ay, bx, by, style, len) else 0.0
        val insetStart = if heads == "start" || heads == "both" then emitArrowhead(pm, bx, by, ax, ay, style, len) else 0.0

        if d > insetStart + insetEnd then
          pm.newPath()
          pm.moveTo(ax + insetStart * ux, ay + insetStart * uy)
          pm.lineTo(bx - insetEnd * ux, by - insetEnd * uy)
          pm.paintWith(None, Some(ink))
    },
  )

  // \arrowhead[head: size:]{a b} - just the head, placed at b and pointing away from a, in the current \stroke
  // colour. The direction comes from the two points, so a head can cap a hand-built \path (point it from the last
  // control point to the path end) without the document computing any angle.
  proc.registerPrimitive(
    "arrowhead",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val pm    = requirePicture(t, handler, "arrowhead", pos)
        val opts  = proc.readOptionalParams(pos)
        val style = arrowStyleOpt(handler, opts, pos)
        val len   = opts.get("size").flatMap(points).getOrElse(7.0)
        val c     = readNumbers(proc, pos)
        if c.length < 4 then handler.error("\\arrowhead expects two points {a b}", pos)
        emitArrowhead(pm, c(0), c(1), c(2), c(3), style, len)
    },
  )

// The arrowhead shapes. The head is geometry — a filled (or, for a bar, stroked) shape oriented along the shaft —
// so it rides the picture's y-up flip exactly like any other shape and needs none of the upright handling that
// placed text and glyphs do.
private[parser] enum ArrowHead:
  case Triangle, Stealth, Bar, Dot

private[parser] object ArrowHead:
  def fromString(s: String): Option[ArrowHead] = s.toLowerCase match
    case "triangle" => Some(Triangle)
    case "stealth"  => Some(Stealth)
    case "bar"      => Some(Bar)
    case "dot"      => Some(Dot)
    case _          => None

// Read the optional head: style (default the swept-back stealth), validating the name.
private[parser] def arrowStyleOpt(handler: TypesetterHandler, opts: Map[String, Value], pos: CharReader): ArrowHead =
  opts.get("head") match
    case None => ArrowHead.Stealth
    case Some(v) =>
      val s = Value.display(v)
      ArrowHead.fromString(s).getOrElse(handler.error(s"unknown arrowhead '$s' (triangle, stealth, bar, dot)", pos))

// Draw an arrowhead of length `len` at tip `(tx,ty)` pointing away from `(ax,ay)`, in the picture's current pen
// colour. Lowers to the existing path + paint ops, so no backend support beyond the shapes already render. Returns
// how far back along the shaft the line should stop at this end so it meets the head cleanly (0 when degenerate).
private[parser] def emitArrowhead(
    pm: PictureMode,
    ax: Double,
    ay: Double,
    tx: Double,
    ty: Double,
    style: ArrowHead,
    len: Double,
): Double =
  val d = math.hypot(tx - ax, ty - ay)
  if d == 0.0 then return 0.0
  val ux = (tx - ax) / d; val uy = (ty - ay) / d // unit direction, toward the tip
  val px = -uy; val py = ux                       // unit perpendicular (quarter-turn counter-clockwise)
  val w  = 0.36 * len                             // half the base width — about a 20° half-angle
  val ink = pm.strokeColor.orElse(pm.fillColor).getOrElse(Color("black"))

  // A point `back` units behind the tip along the shaft and `side` units off to the perpendicular.
  def at(back: Double, side: Double): (Double, Double) =
    (tx - back * ux + side * px, ty - back * uy + side * py)

  style match
    case ArrowHead.Triangle =>
      val (blx, bly) = at(len, w)
      val (brx, bry) = at(len, -w)
      pm.newPath(); pm.moveTo(tx, ty); pm.lineTo(blx, bly); pm.lineTo(brx, bry); pm.close()
      pm.paintWith(Some(ink), None)
      len
    case ArrowHead.Stealth =>
      val (blx, bly) = at(len, w)
      val (ntx, nty) = at(0.55 * len, 0.0) // a concave notch pulled in toward the tip
      val (brx, bry) = at(len, -w)
      pm.newPath(); pm.moveTo(tx, ty); pm.lineTo(blx, bly); pm.lineTo(ntx, nty); pm.lineTo(brx, bry); pm.close()
      pm.paintWith(Some(ink), None)
      0.55 * len // the shaft meets the notch, not the wide base
    case ArrowHead.Bar =>
      val (b1x, b1y) = at(0.0, w)
      val (b2x, b2y) = at(0.0, -w)
      pm.newPath(); pm.moveTo(b1x, b1y); pm.lineTo(b2x, b2y)
      pm.paintWith(None, Some(ink))
      0.0
    case ArrowHead.Dot =>
      val r = len / 2
      pm.newPath(); pm.arc(tx, ty, r, 0, 2 * math.Pi, false); pm.close()
      pm.paintWith(Some(ink), None)
      r // the shaft meets the near edge of the dot centred on the tip

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
//
// A parenthesised coordinate may be made relative to the running current point with the TikZ markers: `++(dx,dy)`
// resolves against the current point and then advances it, so `\polygon{(0,0) ++(2,0) ++(0,2) ++(-2,0)}` walks a
// square; `+(dx,dy)` resolves against it without moving it, for several spokes from one hub. An absolute coordinate
// sets the current point too, so a `++` after it chains naturally. The current point lives in the PictureMode (and
// is also advanced by path segments), so `++` works across commands inside a `\path` as well as within one group.
private[parser] def readNumbers(proc: Processor, pos: CharReader): Vector[Double] =
  val pm = proc.handler match
    case th: TypesetterHandler =>
      th.typesetter.mode match
        case pm: PictureMode => pm
        case _               => null
    case _ => null
  def current: (Double, Double)        = if pm != null then pm.currentPoint else (0.0, 0.0)
  def advance(x: Double, y: Double): Unit = if pm != null then pm.setCurrentPoint(x, y)

  splitTopLevel(stripOuterBraces(proc.readArgument(pos))).flatMap { chunk =>
    val text   = coordText(chunk).trim
    val update = text.startsWith("++")
    // `+` is relative only when a coordinate follows it (`+(…)`); a bare `+5` is just a signed scalar.
    val keep   = !update && text.startsWith("+") && text.drop(1).trim.startsWith("(")
    if update || keep then
      val body     = (if update then text.drop(2) else text.drop(1)).trim
      val (dx, dy) = Coord.parse(body, varResolver(proc), proc.handler.fontUnit, namedResolver(proc))
      val (cx, cy) = current
      val (x, y)   = (cx + dx, cy + dy)
      if update then advance(x, y)
      Vector(x, y)
    else if Coord.looksLikeCoord(text) then
      val (x, y) = Coord.parse(text, varResolver(proc), proc.handler.fontUnit, namedResolver(proc))
      advance(x, y)
      Vector(x, y)
    else
      // A chunk that evaluates to a point (a point variable, or a point-arithmetic expression like \padd{…}{…})
      // contributes both its components, so computed points sit in coordinate position beside literal ones. A
      // point expression does not move the current point — only coordinates the document writes (`(…)`, `++`) do.
      proc.evalExpr(chunk, pos) match
        case Value.Point(x, y) => Vector(x, y)
        case v                 => Vector(points(v).getOrElse(0.0))
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

// Resolve a `(name)` reference to a point stored by \coordinate or \point. A two-element numeric sequence is still
// accepted so points stored the old way keep resolving.
private[parser] def namedResolver(proc: Processor): String => Option[(Double, Double)] = name =>
  proc.handler.get(name) match
    case Value.Point(x, y)                             => Some((x, y))
    case Value.Seq(Vector(Value.Num(x), Value.Num(y))) => Some((x, y))
    case _                                             => None

// Read a single-number group, e.g. \linewidth{2pt} or \rotate{30}.
private[parser] def num1(proc: Processor, pos: CharReader): Double =
  points(proc.evalArgumentExpr(pos)).getOrElse(0.0)

// Read one coordinate argument as a point: any coordinate form, a point variable, or a nested point expression —
// the first two numbers of the group (see readNumbers, which already expands all of these to numbers).
private[parser] def readPoint(proc: Processor, pos: CharReader): (Double, Double) =
  val c = readNumbers(proc, pos)
  (if c.nonEmpty then c(0) else 0.0, if c.length > 1 then c(1) else 0.0)

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
