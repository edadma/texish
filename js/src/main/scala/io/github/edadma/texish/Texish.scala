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

  /** Render every element matching `selector` (default `.texish`) in place: its text content is taken as a
    * texish source and replaced with the rendered SVG, the way KaTeX's auto-render walks a page. Browser only.
    */
  @JSExport
  def autoRender(selector: String = ".texish"): Unit =
    val nodes = dom.document.querySelectorAll(selector)
    var i     = 0
    while i < nodes.length do
      val el = nodes(i).asInstanceOf[dom.Element]
      el.innerHTML = renderToString(el.textContent)
      i += 1
