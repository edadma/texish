package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.*

/** Text-wrap primitives: the bare cutout geometry (\cutout / \cutshape) that reserves a rectangle or silhouette
  * the following text flows around, the figure-placing forms (\wrapbox / \wrapshape) that set a body into that
  * cutout, and \clearwrap to step past any wrap still in progress. */
private[parser] def registerWrapPrimitives(proc: Processor, handler: TypesetterHandler): Unit =
  val t = handler.typesetter

  // The side argument of a cutout primitive: anything starting with 'r' is the right margin, otherwise the left.
  def readSide(proc: Processor, pos: CharReader): Side =
    if Value.display(evalArg(proc, pos)).trim.toLowerCase.startsWith("r") then Side.Right else Side.Left

  // The galley a between-paragraph cutout attaches to. Cutouts only make sense in vertical mode, where the current
  // list is the vertical galley; inside a paragraph there is no fixed vertical position to anchor one to.
  def cutoutGalley(name: String, pos: CharReader): VerticalMode =
    t.mode match
      case v: VerticalMode => v
      case _ =>
        handler.error(s"\\$name must be used between paragraphs", pos)

  // cutout - two dimensions and a side: reserve a bare rectangle of the given width and height at the current point
  // in the galley, hugging the left or right margin, that the following text flows around. The low-level geometry
  // primitive — it places no content; \wrapbox places a figure and sizes a cutout to it.
  proc.registerPrimitive(
    "cutout",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val width  = Value.number(evalArg(proc, pos)).getOrElse(0.0)
        val height = Value.number(evalArg(proc, pos)).getOrElse(0.0)
        val side   = readSide(proc, pos)
        val g      = cutoutGalley("cutout", pos)
        // anchor the cutout to a marker dropped at this point, so its band is resolved from the marker's live
        // position the same way a figure's is from its overlay — keeping it aligned across a page break
        val marker = new CutoutMarker
        t.add(marker)
        g.cutouts += Cutout(marker, height, side, Cutout.rect(width))
    },
  )

  // cutshape - a named silhouette, then a width, height and side: like \cutout but the text follows the shape's
  // outline rather than its bounding box. A bare geometry primitive with no figure of its own; \wrapshape both
  // places a figure and gives it a shaped cutout. The bare cutout keeps no gutter — the width given is the inset.
  proc.registerPrimitive(
    "cutshape",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val shape  = CutoutShape.named(Value.display(evalArg(proc, pos)))
        val width  = Value.number(evalArg(proc, pos)).getOrElse(0.0)
        val height = Value.number(evalArg(proc, pos)).getOrElse(0.0)
        val side   = readSide(proc, pos)
        val g      = cutoutGalley("cutshape", pos)
        val marker = new CutoutMarker
        t.add(marker)
        g.cutouts += Cutout(marker, height, side, CutoutShape.profile(shape, width, height, 0.0))
    },
  )

  // A wrapped figure: read a side, a width and a body, typeset the body into its own box of that width, anchor it
  // against the chosen margin at the current point, and register a cutout of the given shape sized to the body (plus
  // the \wrapsep gutter) so the following text wraps beside it. The box overlays the galley with no height of its own
  // (see OverlayBox), so it sits across the lines the cutout narrows rather than pushing them down. \wrapbox reads no
  // shape and is always a rectangle; \wrapshape reads a leading shape name so the text can follow the figure's
  // silhouette instead of its bounding box.
  def wrapPrimitive(name: String, readShape: (Processor, CharReader) => CutoutShape): Primitive =
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val shape = readShape(proc, pos)
        val s     = readSide(proc, pos)
        val width = Value.number(evalArg(proc, pos)).getOrElse(0.0)
        val body  = proc.readArgument(pos)

        // typeset the body now, into its own vertical box set to the figure width; the scope brackets the width
        // change and any font or spacing changes the body makes. The indent flag is bracketed too: the body runs a
        // paragraph (its caption), which would leave indentParagraph set and so indent — or after a section, flush —
        // the wrapping text regardless of what the surrounding flow asked for. Preserving it means the text beside
        // the figure indents like any body paragraph but stays flush when it opens a section.
        val savedIndent = t.indentParagraph
        t.enter()
        t.vbox()
        if width > 0 then t.set("hsize", width)
        proc.processTokenList(body)
        t.paragraph()

        val content = t.mode.exit

        t.exit()
        t.indentParagraph = savedIndent

        if content ne null then
          val g       = cutoutGalley(name, pos)
          val gutter  = t.getNumber("wrapsep")
          val hsize   = t.getNumber("hsize")
          val dx      = if s == Side.Right then hsize - content.width else 0.0
          val overlay = new OverlayBox(content, dx, content.ascent)
          t.add(overlay)

          // anchor the cutout to the overlay, so the band the text wraps into tracks wherever the figure actually
          // sits — after any topskip above it, and after a page break carries it to the next page — rather than a
          // fixed coordinate that would drift from the figure and narrow the wrong lines
          g.cutouts += Cutout(overlay, content.height, s, CutoutShape.profile(shape, content.width, content.height, gutter))

          // forbid a page break between the figure and the text that wraps it: with the inhibit penalty here, the
          // glue that follows the overlay has a penalty for its predecessor and so is no longer a legal break, the
          // way ParagraphMode.wrapPenalty forbids breaks between the wrapped lines. The only break left is above
          // the figure, so a wrap that will not fit moves to the next page whole rather than stranding the figure.
          t.add(Penalty(Penalty.Inhibit))
    }

  proc.registerPrimitive("wrapbox", wrapPrimitive("wrapbox", (_, _) => CutoutShape.Rectangle))
  proc.registerPrimitive(
    "wrapshape",
    wrapPrimitive("wrapshape", (proc, pos) => CutoutShape.named(Value.display(evalArg(proc, pos)))),
  )

  // clearwrap - end any text-wrap in progress: pad the galley down past the foot of every active wrapped figure so
  // the following content clears it, then drop the cutouts. A wrapped figure reserves no vertical space of its own
  // (its overlay is zero-height, so text flows beside it), so a figure taller than its accompanying text would
  // otherwise be ridden over by whatever block comes next — a section heading is a full-width \leftline that does
  // not narrow around the figure. A sectioning command issues \clearwrap so its heading steps below the figure;
  // running text needs no clearing, since its lines narrow and flow past the figure on their own. Padding nothing
  // when the content has already cleared the figure, it is always safe to call.
  proc.registerPrimitive(
    "clearwrap",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        t.mode match
          case g: VerticalMode =>
            val floor = g.cutouts.flatMap(c => g.cutoutBand(c).map(_._2)).maxOption.getOrElse(0.0)
            val gap   = floor - g.naturalHeight
            if gap > 0 then t.add(VSpaceBox(gap))
            g.cutouts.clear()
          case _ =>
    },
  )
