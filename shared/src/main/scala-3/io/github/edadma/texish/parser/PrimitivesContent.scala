package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.*

/** Content-selection primitives: page-break \penalty, hyphenation control (\loadhyphenation / \language /
  * \usehyphenation), font selection (\typeface / \font / \fontsize / \fontscale), raster images
  * (\image / \includegraphics / \defbitmap / \usebitmap), hyperlinks (\href / \url), and pen colour
  * (\color / \textcolor). */
private[parser] def registerContentPrimitives(proc: Processor, handler: TypesetterHandler): Unit =
  val t = handler.typesetter

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
          case (Value.Text(l), Value.Text(p)) =>
            Hyphenation.loadPatterns(l, p)
            handler.typesetter.language = Some(l)
          case _ => handler.error("\\loadhyphenation expects {language}{path}", pos)
    },
  )

  // language - 1 braced arg: switch active hyphenation language
  proc.registerPrimitive(
    "language",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val arg = evalArg(proc, pos)
        arg match
          case Value.Text(lang) =>
            if Hyphenation.isLoaded(lang) then handler.typesetter.language = Some(lang)
            else handler.error(s"\\language: no hyphenation patterns loaded for '$lang'", pos)
          case _ => handler.error("\\language expects a language name", pos)
    },
  )

  // usehyphenation - 1 braced arg: enable the hyphenation patterns compiled into the binary for a
  // language tag (en-us, es, fr, ...) and make that language active. This is how a document switches on
  // hyphenation without shipping a pattern file -- the TeX-format equivalent of loading hyphen.tex. For
  // a language whose patterns are not bundled, load them from a file with \loadhyphenation instead.
  proc.registerPrimitive(
    "usehyphenation",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        evalArg(proc, pos) match
          case Value.Text(lang) =>
            if Hyphenation.enableEmbedded(lang) then handler.typesetter.language = Some(lang)
            else
              val have = Hyphenation.embeddedLanguages.toSeq.sorted.mkString(", ")
              handler.error(
                s"\\usehyphenation: no patterns are bundled for '$lang' (available: $have); use \\loadhyphenation{$lang}{path} for an external pattern file",
                pos,
              )
          case _ => handler.error("\\usehyphenation expects a language tag, e.g. \\usehyphenation{en-us}", pos)
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
            t.set("spaceskip", interwordGlue(font.space))
            t.set("xspaceskip", xinterwordGlue(font.space))
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
            t.set("spaceskip", interwordGlue(font.space))
            t.set("xspaceskip", xinterwordGlue(font.space))
          case _ => handler.error("\\font expects <typeface> <size> <style>", pos)
    },
  )

  // loadfont - 2 braced args: register a font file from disk under a typeface name so it can then be
  // selected with \typeface or \font. Where the bundled families cover only Latin, this is how a document
  // brings in a face the build does not ship — most importantly a CJK font, since the line breaker can wrap
  // CJK text but has no glyphs to set it with until one is loaded.
  //
  // A relative path is resolved against the directory of the file doing the loading first, exactly as \use
  // resolves a module, so a font kept beside the document is found however the engine was launched — a host
  // that runs from elsewhere (an editor, a preview app) would otherwise miss it.
  proc.registerPrimitive(
    "loadfont",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name = evalArg(proc, pos)
        val path = evalArg(proc, pos)
        (name, path) match
          case (Value.Text(nm), Value.Text(p)) =>
            t.loadFont(nm, p, Set.empty, Set.empty, proc.currentDir)
          case _ => handler.error("\\loadfont expects <name> <path>", pos)
    },
  )

  // fontsize - 1 braced arg: change only the type size, keeping the current typeface and shape/series. Unlike
  // \font, which re-selects all three, this is the size knob the LaTeX-style size declarations (\small, \large,
  // \Large …) are built on, so a size change inside bold or italic text keeps that style. selectFont also resets
  // \baselineskip to TeX's 1.2 × size default leading; a document that wants a different leading sets it after.
  proc.registerPrimitive(
    "fontsize",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        evalArg(proc, pos) match
          case Value.Num(sz) =>
            val cur  = t.currentFont
            val font = t.selectFont(cur.typeface, sz.toDouble, cur.style)
            t.set("spaceskip", interwordGlue(font.space))
            t.set("xspaceskip", xinterwordGlue(font.space))
          case _ => handler.error("\\fontsize expects a number", pos)
    },
  )

  // fontscale - 2 args, a factor and a style: re-select the current typeface at the current size scaled by the
  // factor, with the given style. Like \font but relative and keeping the typeface, so a small ornament tracks the
  // surrounding size at any scale — the slanted "ish" of \TeXish and the small A of \LaTeX size off the current
  // font instead of a fixed point size, and the logos render proportionally in a footnote or a title alike.
  proc.registerPrimitive(
    "fontscale",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val factor = evalArg(proc, pos)
        val style  = evalArg(proc, pos)
        (factor, style) match
          case (Value.Num(f), Value.Text(st)) =>
            val cur  = t.currentFont
            val font = t.selectFont(cur.typeface, cur.size * f.toDouble, st.split("\\s+").toSet)
            t.set("spaceskip", interwordGlue(font.space))
            t.set("xspaceskip", xinterwordGlue(font.space))
          case _ => handler.error("\\fontscale expects <factor> <style>", pos)
    },
  )

  // image - 1 arg
  proc.registerPrimitive(
    "image",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val arg = evalArg(proc, pos)
        arg match
          case Value.Text(path) => t.image(resolveImagePath(proc, path))
          case _                => handler.error("\\image expects a path", pos)
    },
  )

  // includegraphics [width=…,height=…,scale=…] {path} — place a raster image, sized by an optional LaTeX-style
  // key=value list. width and height are lengths (a dimension like 200pt, or a factor times \linewidth /
  // \textwidth, the current line width); giving only one scales the other to keep the aspect ratio. scale
  // multiplies the natural size. With no options the image is placed at its natural pixel size.
  proc.registerPrimitive(
    "includegraphics",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val (width, height, scale) = parseGraphicsOptions(proc, t, proc.readOptionalArg(pos), pos)
        evalArg(proc, pos) match
          case Value.Text(path) => t.image(resolveImagePath(proc, path), width, height, scale)
          case _                => handler.error("\\includegraphics expects a path", pos)
    },
  )

  // defbitmap {name} {width} {height} {depth} {base64} — define an inline raster image from data embedded in the
  // source, so a package can carry a glyph (a clef, a logo) with no external file. The pixels are alpha only,
  // black where opaque: `depth` bits per pixel (1 for crisp line art, 2/4/8 for antialiased grey levels), packed
  // MSB-first row-major and base64-encoded. The image is built once and stored under name for \usebitmap. On a
  // backend without image support it is silently skipped (the name stays undefined), so a document can fall back.
  proc.registerPrimitive(
    "defbitmap",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val name  = Value.display(evalArg(proc, pos))
        val width  = argInt(proc, pos)
        val height = argInt(proc, pos)
        val depth  = argInt(proc, pos)
        // the data is read verbatim — base64 is not markup, and an expression read would drop characters and
        // could trip over // (a comment) or other specials, as a URL does in \href
        val data = proc.readRawArgument(pos)
        if t.imagesSupported then
          val argb   = unpackBitmapAlpha(base64Decode(data), width, height, depth)
          val handle = t.createImage(width, height, argb)
          proc.handler.set(name, Value.Native(InlineBitmap(handle, width, height)))
    },
  )

  // usebitmap [width:… height:…] {name} — place a bitmap defined by \defbitmap, sized like \includegraphics
  // (one of width/height keeps the aspect ratio). An undefined name places nothing, so an image-less backend
  // simply shows whatever the document drew instead.
  proc.registerPrimitive(
    "usebitmap",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val opts = proc.readOptionalParams(pos)
        val reqW = opts.get("width").flatMap(points)
        val reqH = opts.get("height").flatMap(points)
        val name = Value.display(evalArg(proc, pos))
        proc.handler.get(name) match
          case Value.Native(b: InlineBitmap) =>
            t.add(new HandleImageBox(t, b.handle, b.width, b.height, reqW, reqH, None))
          case _ =>
    },
  )

  // href {uri} {text} - a hyperlink: typeset the display text (blue, like hyperref's coloured links) and wrap
  // it so that, in PDF output, the drawn region becomes a clickable annotation pointing at uri. The uri is read
  // verbatim — it may contain // (otherwise a comment), ~, % and other specials — while the text is ordinary
  // markup. On a backend without annotations the text just draws blue.
  proc.registerPrimitive(
    "href",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val uri  = proc.readRawArgument(pos)
        val body = proc.readArgument(pos)
        handler.flushPendingSpace()
        t.hbox(null)
        t.enter()
        t.currentColor = Color("blue")
        proc.processTokenList(body)
        t.exit()
        val box = t.mode.exit
        if box ne null then handler.addBox(new LinkBox(box, uri))
    },
  )

  // url {uri} - typeset the URL itself, in the monospaced face and blue, as a link to itself. The argument is
  // read verbatim and set literally (no ligatures, no re-tokenizing), so the address appears exactly as written.
  proc.registerPrimitive(
    "url",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val uri = proc.readRawArgument(pos)
        handler.flushPendingSpace()
        t.hbox(null)
        t.enter()
        val font = t.mono()
        t.set("spaceskip", interwordGlue(font.space))
        t.set("xspaceskip", xinterwordGlue(font.space))
        t.currentColor = Color("blue")
        t.add(t.charBox(uri))
        t.exit()
        val box = t.mode.exit
        if box ne null then handler.addBox(new LinkBox(box, uri))
    },
  )

  // color [alpha] name - set the pen colour for the text, rules and glyphs that follow in the current group; the
  // colour reverts at the group's close, exactly as \font does (both are saved on enter and restored on exit). The
  // name is a CSS colour word (blue, darkred, …), a #RRGGBB or #RRGGBBAA hex code, or `transparent`. An optional
  // [alpha] (0–1) makes any colour translucent, so \color[0.5]{red} and \color{#ff000080} are the same half-red.
  proc.registerPrimitive(
    "color",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val alpha = readOptionalNumber(proc)
        evalArg(proc, pos) match
          case Value.Text(name) => t.currentColor = withAlpha(Color(name), alpha)
          case _                => handler.error("\\color expects a colour name or #RRGGBB code", pos)
    },
  )

  // textcolor [alpha] name body - set the pen colour for just its body, which is typeset in its own group so the
  // colour reverts immediately after. \textcolor{blue}{link} is the local form of \color, and it takes the same
  // optional [alpha] and #RRGGBBAA translucency.
  proc.registerPrimitive(
    "textcolor",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val alpha = readOptionalNumber(proc)
        evalArg(proc, pos) match
          case Value.Text(name) =>
            val body = proc.readArgument(pos)
            t.enter()
            t.currentColor = withAlpha(Color(name), alpha)
            proc.processTokenList(body)
            t.exit()
          case _ => handler.error("\\textcolor expects a colour name or #RRGGBB code", pos)
    },
  )

  // thecolor - the pen colour in force right here, as a #rrggbb string (#rrggbbaa when translucent). The counterpart
  // to \color, and what lets a package draw in the document's ink rather than a colour of its own: \stroke{\thecolor}
  // follows the pen into a dark scheme, where a literal would not. Read it at the moment of drawing — \set evaluates
  // its value immediately, so \set myink {\thecolor} freezes the pen as it was when the line ran.
  proc.registerPrimitive(
    "thecolor",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val result = t.currentColor.hex

        proc.setResult(Value.Text(result))
        proc.handler.text(result)
    },
  )

  // thepagecolor - the colour the page is painted, as a #rrggbb string (#rrggbbaa when translucent). The counterpart
  // to \pagecolor. With \thecolor it gives a package both ends of the document's scheme, so a fill can be derived
  // from the paper — \oklchof{\thepagecolor} for its coordinates, \oklch to rebuild a tint of it — instead of being
  // a literal that only suits one background.
  proc.registerPrimitive(
    "thepagecolor",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val result = t.backgroundColor.hex

        proc.setResult(Value.Text(result))
        proc.handler.text(result)
    },
  )

  // pagecolor [alpha] name - set the colour painted across the whole page, under all content: a CSS colour word, a
  // #RRGGBB or #RRGGBBAA hex code, or `transparent`. An optional [alpha] (0–1) tints the page translucently, so
  // \pagecolor[0.6]{black} (or \pagecolor{#000000aa}) lets a compositor show video through it, and
  // \pagecolor{transparent} leaves the page unpainted. The setting applies to the document's pages; give it in the
  // preamble. It is not part of the saved graphics state, so it is not scoped to a group the way \color is.
  proc.registerPrimitive(
    "pagecolor",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        val alpha = readOptionalNumber(proc)
        evalArg(proc, pos) match
          case Value.Text(name) => t.backgroundColor = withAlpha(Color(name), alpha)
          case _                => handler.error("\\pagecolor expects a colour name or #RRGGBB code", pos)
    },
  )
