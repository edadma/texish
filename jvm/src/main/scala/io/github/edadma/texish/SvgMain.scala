package io.github.edadma.texish

import io.github.edadma.texish.parser.{Processor, TypesetterHandler, registerTypesettingPrimitives}

import scala.io.Source

/** Build-time SVG rendering: run a texish source document through the [[SvgTypesetter]] and write one
  * self-contained `.svg` file per page. This is the JVM half of the SVG backend — it reads fonts off disk and
  * produces static SVG that can be embedded in a web page with no client-side code (the in-browser renderer is
  * the Scala.js half). Native has Cairo for build-time output, so it needs no SVG CLI of its own.
  */
object SvgMain:

  /** Render `source` and return one SVG document per page. `baseDir` resolves `\use` packages and relative
    * paths against the document's own directory. */
  def renderSvg(source: String, baseDir: String = "."): Seq[String] =
    val t = Passes.untilStable() { () => new SvgTypesetter } { t =>
      val handler = new TypesetterHandler(t)
      val proc    = new Processor(handler)

      registerTypesettingPrimitives(proc, handler)
      proc.setBaseDir(baseDir)
      proc.process(source)
      t.end()
    }

    t.asInstanceOf[SvgTypesetter].pageSvgs

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      Console.err.println("usage: SvgMain <input.script> [output-base]")
      sys.exit(1)

    val input  = args(0)
    val file   = new java.io.File(input)
    if !file.isFile then
      Console.err.println(s"texish: file '$input' not found")
      sys.exit(1)

    val source =
      val src = Source.fromFile(file)
      try src.mkString
      finally src.close()

    val baseDir = Option(file.getAbsoluteFile.getParent).getOrElse(".")
    val base =
      if args.length > 1 then args(1)
      else
        val name = file.getName
        val dot  = name.lastIndexOf('.')
        val stem = if dot > 0 then name.substring(0, dot) else name
        Option(file.getParentFile).map(d => new java.io.File(d, stem).getPath).getOrElse(stem)

    val pages = renderSvg(source, baseDir)

    pages.zipWithIndex.foreach { case (svg, i) =>
      val name = if pages.length == 1 then s"$base.svg" else s"${base}_${i + 1}.svg"
      val w    = new java.io.PrintWriter(name)
      try w.write(svg)
      finally w.close()
      println(s"wrote $name")
    }
