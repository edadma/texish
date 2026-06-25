package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.*

/** Register the standard typesetting primitives (\newpage, \hbox, \font, \bold, ...) with a processor.
  *
  * These are the language-level bindings to the typesetting API — the builtin vocabulary any document language gets
  * for free. Applications register their own primitives on top.
  *
  * The vocabulary is large, so it is split across companion files, each a `registerXxxPrimitives` function over the
  * same `(proc, handler)` pair, with the shared helpers gathered in `PrimitivesSupport.scala`:
  *   - [[registerFlowPrimitives]]      — paragraph/page control, cross-references, contents lists, footnotes, floats
  *   - [[registerWrapPrimitives]]      — text-wrap cutouts and wrapped figures
  *   - [[registerContentPrimitives]]   — \penalty, hyphenation, font selection, images, links, colour
  *   - [[registerBoxPrimitives]]       — glue, rules, box builders, minipage, lengths, phantoms, columns, \geometry
  *   - [[registerRegisterPrimitives]]  — box registers, \kern, discretionaries, leaders, math spaces, \lower/\raise
  *   - [[registerMathPrimitives]]      — math-mode constructs (see `PrimitivesMath.scala`)
  *   - [[registerFontShapePrimitives]] — font-shape declarations, role text, scripts, verbatim/code, framed boxes
  *   - [[registerPictureGraphicsPrimitives]] — the picture/drawing layer (see `PrimitivesPicture.scala`)
  */
def registerTypesettingPrimitives(proc: Processor, handler: TypesetterHandler): Unit =
  val t = handler.typesetter

  // Expose whether this backend can render images, so a document can choose an inline bitmap (\defbitmap) when it
  // can and a drawn fallback when it cannot (e.g. the JS layer).
  t.set("imagessupported", if t.imagesSupported then 1.0 else 0.0)

  registerFlowPrimitives(proc, handler)
  registerWrapPrimitives(proc, handler)
  registerContentPrimitives(proc, handler)
  registerBoxPrimitives(proc, handler)
  registerRegisterPrimitives(proc, handler)
  registerMathPrimitives(proc, handler)
  registerFontShapePrimitives(proc, handler)

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
