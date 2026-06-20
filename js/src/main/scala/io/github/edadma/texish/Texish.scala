package io.github.edadma.texish

import io.github.edadma.texish.parser.{Processor, TypesetterHandler, registerTypesettingPrimitives}

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.*

/** The in-browser texish renderer, exported to JavaScript as `texish`. It lays a texish source document out
  * with the pure-Scala SVG backend and hands back SVG — as a string, as a parsed DOM element, or rendered in
  * place over the document, in the style of KaTeX's auto-render. No server, no pre-baked images, no fonts to
  * download separately: glyphs are filled as outline paths from the fonts embedded in the build.
  *
  * A source renders to one or more pages; the snippet-oriented [[renderToString]]/[[render]] return the first
  * page, while [[renderAllToStrings]] returns every page for multi-page documents. */
@JSExportTopLevel("texish")
object Texish:

  /** Lay `source` out and return every page as a standalone `<svg>` document. */
  private def renderPages(source: String): Seq[String] =
    val t       = new SvgTypesetterJS
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)

    registerTypesettingPrimitives(proc, handler)
    proc.setBaseDir(".")
    proc.process(source)
    t.end()
    t.pageSvgs

  /** Render `source` and return the first page as an SVG document string. */
  @JSExport
  def renderToString(source: String): String =
    renderPages(source).headOption.getOrElse("")

  /** Render `source` and return every page as an array of SVG document strings. */
  @JSExport
  def renderAllToStrings(source: String): js.Array[String] =
    js.Array(renderPages(source)*)

  /** Render `source` and return the first page parsed into a detached `<svg>` element the caller can insert
    * into the page. Browser only — needs a DOM. */
  @JSExport
  def render(source: String): dom.SVGElement =
    parseSvg(renderToString(source))

  /** Render `source` and return every page as an array of `<svg>` elements. Browser only. */
  @JSExport
  def renderAll(source: String): js.Array[dom.SVGElement] =
    js.Array(renderPages(source).map(parseSvg)*)

  /** Parse one SVG document string into a live `<svg>` element. The HTML5 parser recognises inline `<svg>` and
    * builds it in the SVG namespace, so the element comes back ready to insert. */
  private def parseSvg(svg: String): dom.SVGElement =
    val holder = dom.document.createElement("div")
    holder.innerHTML = svg
    holder.firstElementChild.asInstanceOf[dom.SVGElement]

  // ─── canvas rendering (crisp, hinted text) ──────────────────────────────────────

  /** Render `source` to an HTML `<canvas>`, drawing ordinary text with the browser's own hinted rasteriser so
    * it is as sharp as native text (the SVG path renderer cannot be), and falling back to canvas paths only for
    * the math glyphs that have no codepoint. Fonts must be loaded into the browser first, so this is async: it
    * returns a promise of the first page's canvas and, if `container` is given, appends it there. Browser only.
    */
  @JSExport
  def renderToCanvas(source: String, container: dom.Element): js.Promise[dom.html.Canvas] =
    val t = new CanvasTypesetter
    t.fontsReady.`then`[dom.html.Canvas] { (_: js.Any) =>
      val handler = new TypesetterHandler(t)
      val proc    = new Processor(handler)
      registerTypesettingPrimitives(proc, handler)
      proc.setBaseDir(".")
      proc.process(source)
      t.end()
      val canvas = t.pageCanvases.headOption.getOrElse {
        val c = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
        c.width = 1; c.height = 1; c
      }
      if container != null && !js.isUndefined(container) then container.appendChild(canvas)
      (canvas: dom.html.Canvas | js.Thenable[dom.html.Canvas])
    }

  /** Render every element matching `selector` (default `.texish`) to a canvas in place, the canvas analogue of
    * [[autoRender]]. Each render is asynchronous (fonts load first); failures are isolated per element. Browser
    * only. */
  @JSExport
  def autoRenderCanvas(selector: String = ".texish"): Unit =
    val nodes = dom.document.querySelectorAll(selector)
    var i     = 0
    while i < nodes.length do
      val el  = nodes(i).asInstanceOf[dom.Element]
      val src = el.textContent
      el.textContent = ""
      renderToCanvas(src, el).`catch` { (e: Any) =>
        dom.console.error("texish: failed to render an element to canvas", e.asInstanceOf[js.Any])
        (): Unit | js.Thenable[Unit]
      }
      i += 1

  /** Render every element matching `selector` (default `.texish`) in place: its text content is taken as a
    * texish source and replaced with the rendered SVG, the way KaTeX's auto-render walks a page. Browser only.
    */
  @JSExport
  def autoRender(selector: String = ".texish"): Unit =
    val nodes = dom.document.querySelectorAll(selector)
    var i     = 0
    while i < nodes.length do
      val el = nodes(i).asInstanceOf[dom.Element]
      // Isolate failures: a source that does not lay out leaves its element untouched rather than aborting the
      // whole sweep, so one bad block never blanks the rest of the page.
      try el.innerHTML = renderToString(el.textContent)
      catch case e: Throwable => dom.console.error("texish: failed to render an element", e.asInstanceOf[js.Any])
      i += 1
