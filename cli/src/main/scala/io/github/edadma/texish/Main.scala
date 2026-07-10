package io.github.edadma.texish

import io.github.edadma.libcairo.Surface
import io.github.edadma.texish.parser.{Processor, TypesetterHandler, registerTypesettingPrimitives}

import scopt.OParser

import scala.io.Source
import scala.scalanative.posix.unistd.isatty

/** Command-line entry point. Reads a texish source document (from a file or standard input), runs it
  * through the parser over a Cairo backend, and writes the result as a PDF or one PNG per page.
  */

private val Version = "0.19.5"

private case class Config(
    input: Option[String] = None,
    output: Option[String] = None,
    typ: String = "pdf",
    paper: String = "letter",
    resolution: String = "hd",
)

private def fail(msg: String): Nothing =
  Console.err.println(s"texish: $msg")
  sys.exit(1)

@main def run(args: String*): Unit =
  val builder = OParser.builder[Config]
  val parser =
    import builder.*
    OParser.sequence(
      programName("texish"),
      head("texish", Version),
      arg[String]("<input file>")
        .optional()
        .action((x, c) => c.copy(input = Some(x)))
        .text("texish source to typeset; reads standard input if omitted"),
      opt[String]('o', "output")
        .valueName("<file>")
        .action((x, c) => c.copy(output = Some(x)))
        .text("output path (default: the input file's path with the output extension, or out)"),
      opt[String]('t', "type")
        .valueName("<pdf | png>")
        .action((x, c) => c.copy(typ = x))
        .validate {
          case "pdf" | "png" => success
          case _             => failure("only 'pdf' or 'png' are allowed as output types")
        }
        .text("output type (default: pdf)"),
      opt[String]('p', "paper")
        .valueName("<letter | legal | a3 | a4 | a5>")
        .action((x, c) => c.copy(paper = x))
        .validate {
          case "letter" | "legal" | "a3" | "a4" | "a5" => success
          case _ => failure("paper size must be one of: letter, legal, a3, a4, a5")
        }
        .text("paper size (default: letter)"),
      opt[String]('r', "resolution")
        .valueName("<sd | hd | fhd>")
        .action((x, c) => c.copy(resolution = x))
        .validate {
          case "sd" | "hd" | "fhd" => success
          case _                   => failure("only 'sd', 'hd', or 'fhd' are allowed as resolutions")
        }
        .text("PNG device resolution (default: hd)"),
      help("help").text("prints this usage text"),
      version("version").text("prints the current version"),
    )

  OParser.parse(parser, args, Config()) match
    case Some(config) => render(config)
    case None         => sys.exit(1)

private def render(config: Config): Unit =
  val source =
    config.input match
      case Some(path) =>
        val f = new java.io.File(path)

        if !f.exists then fail(s"file '$path' not found")
        else if !f.isFile then fail(s"'$path' is not a regular file")
        else if !f.canRead then fail(s"file '$path' is not readable")
        else
          val src = Source.fromFile(f)

          try src.mkString
          finally src.close()
      case None =>
        if isatty(0) == 1 then fail("no input file given and standard input is a terminal")
        Source.stdin.mkString

  val base = defaultOutputBase(config.input, config.output)

  // Resolve \use packages relative to the document's own directory (and the directories of modules it loads).
  val baseDir =
    config.input.flatMap(p => Option(new java.io.File(p).getAbsoluteFile.getParent)).getOrElse(".")

  config.typ match
    case "pdf" => renderPdf(source, ensureExtension(base, "pdf"), config.paper, baseDir)
    case "png" => renderPng(source, base, config.paper, config.resolution, baseDir)

/** Build a typesetter, feed it the document, and flush it. The engine ships only primitives; higher-level
  * macros (logos, sectioning, lists, …) come from a format the document includes itself. */
private def typeset(t: Typesetter, source: String, baseDir: String = "."): Unit =
  val handler = new TypesetterHandler(t)
  val proc    = new Processor(handler)

  registerTypesettingPrimitives(proc, handler)
  proc.setBaseDir(baseDir)
  proc.process(source)
  t.end()

private[texish] def renderPdf(source: String, output: String, paper: String, baseDir: String = "."): Unit =
  val t = Passes.untilStable() { () =>
    val t      = new CairoPDFTypesetter(output)
    val (w, h) = paperDimensions(paper, t)

    t.set("paperwidth", w)
    t.set("paperheight", h)
    t
  }(typeset(_, source, baseDir))

  t.destroy()
  println(s"wrote $output")

private[texish] def renderPng(source: String, base: String, paper: String, resolution: String, baseDir: String = "."): Unit =
  val dpi =
    resolution match
      case "sd"  => 96.0
      case "hd"  => 150.0
      case "fhd" => 300.0

  val t = Passes.untilStable() { () =>
    val t      = new CairoImageTypesetter(dpi)
    val (w, h) = paperDimensions(paper, t)

    t.set("paperwidth", w)
    t.set("paperheight", h)
    t
  }(typeset(_, source, baseDir))

  // each shipped page is its own ARGB32 surface; write one PNG per page
  val pages = t.getDocument.printedPages

  pages.zipWithIndex.foreach { case (page, i) =>
    val name    = if pages.length == 1 then ensureExtension(base, "png") else s"${stripExtension(base)}_${i + 1}.png"
    val surface = page.asInstanceOf[Surface]

    surface.writeToPNG(name)
    surface.destroy() // a shipped surface belongs to us once we have it
    println(s"wrote $name")
  }

  t.destroy()

/** Paper dimensions in the engine's point space, using the typesetter's own unit constants. */
private def paperDimensions(paper: String, t: Typesetter): (Double, Double) =
  // the same named-size table the \geometry primitive uses, so the CLI flag and the in-document command agree
  t.paperSize(paper).getOrElse(sys.error(s"unknown paper size: $paper")) // unreachable: the CLI validates --paper

/** The output base path: an explicit `-o` wins; otherwise the input file's own path with its extension dropped
  * but its directory kept, so the result lands beside the source rather than in the current directory; with no
  * input file (reading stdin) it is "out".
  */
private[texish] def defaultOutputBase(input: Option[String], output: Option[String]): String =
  output.getOrElse(
    input.map { p =>
      val f    = new java.io.File(p)
      val name = stripExtension(f.getName)
      Option(f.getParentFile).map(d => new java.io.File(d, name).getPath).getOrElse(name)
    }.getOrElse("out"),
  )

private[texish] def stripExtension(name: String): String =
  val dot = name.lastIndexOf('.')
  if dot > 0 then name.substring(0, dot) else name

private[texish] def ensureExtension(name: String, ext: String): String =
  if name.toLowerCase.endsWith(s".$ext") then name else s"$name.$ext"
