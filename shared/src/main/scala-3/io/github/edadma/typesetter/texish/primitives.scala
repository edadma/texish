package io.github.edadma.typesetter.texish

import io.github.edadma.char_reader.CharReader
import io.github.edadma.typesetter.{Box, Glue, Hyphenation, InfGlue, Penalty, RuleBox, UnderlineBox}

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
  proc.registerPrimitive("vfil", SimplePrimitive(() => t.fil))
  proc.registerPrimitive("hfill", SimplePrimitive(() => t.fill))
  proc.registerPrimitive("vfill", SimplePrimitive(() => t.fill))
  proc.registerPrimitive("hss", SimplePrimitive(() => t.add(InfGlue)))
  proc.registerPrimitive("vss", SimplePrimitive(() => t.add(InfGlue)))

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

  // vskip - glue spec: dimension with optional plus/minus continuation, braced glue, or glue variable
  proc.registerPrimitive(
    "vskip",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        glueArg(proc, pos) match
          case Some(g) => t.add(g)
          case None    => handler.error("\\vskip expects a dimension or glue", pos)
    },
  )

  // hskip - glue spec: dimension with optional plus/minus continuation, braced glue, or glue variable
  proc.registerPrimitive(
    "hskip",
    new Primitive {
      def execute(proc: Processor, pos: CharReader): Unit =
        glueArg(proc, pos) match
          case Some(g) => t.add(g)
          case None    => handler.error("\\hskip expects a dimension or glue", pos)
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

  // Running headers and footers: if the document defines a headline or footline macro, each shipped page builds
  // an hbox to hsize from its body at shipout time — pageno is already set to the shipping page's number, so
  // \the\pageno in the macro is always current. The hbox is built on a temporary mode pushed over whatever is
  // being typeset and popped with exit (not done), so the box never lands in the page being broken.
  t.pageDecorator = () =>
    def line(name: String): Box | Null =
      t.get(name) match
        case Some(Value.Macro(_, body, _)) =>
          t.hbox(t.getNumber("hsize"))
          proc.processTokenList(body)
          t.mode.exit
        case _ => null

    (line("headline"), line("footline"))

  // Active characters
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

// A dimension value in big points: Dimen carries its own unit; a bare number means points
private def points(v: Value): Option[Double] = v match
  case Value.Dimen(p) => Some(p.toDouble)
  case Value.Num(n)   => Some(n.toDouble)
  case _              => None

// Resolve a glue argument: a braced glue spec ({12pt plus 2pt}), a glue-valued variable, or a bare dimension
// optionally continued by `plus`/`minus` keywords in the token stream (\vskip 12pt plus 2pt minus 1fil)
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
