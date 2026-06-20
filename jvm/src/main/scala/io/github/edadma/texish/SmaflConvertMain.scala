package io.github.edadma.texish

import java.nio.file.{Files, Paths}

import io.github.edadma.texish.opentype.{Smafl, SmaflEntry}

/** Build-time SMaFL converter: read an OpenType math font, assign private-use codepoints to its MATH-table
  * size-variant and assembly glyphs (the `opentype.Smafl` converter), and write the extended font alongside a `smafl.json` table of
  * the assignments. Run it to regenerate `LatinModernMath-SMaFL.otf`; the in-browser renderer then draws the
  * tall surd and other stretchy math glyphs through the browser's hinted text path instead of filling outlines.
  *
  * Usage: `SmaflConvertMain <input-math.otf> [output.otf]`. The metadata sidecar is written next to the output
  * with a `.json` extension.
  */
object SmaflConvertMain:

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      Console.err.println("usage: SmaflConvertMain <input-math.otf> [output.otf]")
      sys.exit(1)

    val inPath = args(0)
    if !Files.isRegularFile(Paths.get(inPath)) then
      Console.err.println(s"SmaflConvertMain: file '$inPath' not found")
      sys.exit(1)

    val outPath =
      if args.length > 1 then args(1)
      else inPath.replaceAll("\\.otf$", "") + "-SMaFL.otf"
    val jsonPath = outPath.replaceAll("\\.otf$", "") + ".json"

    val input = Files.readAllBytes(Paths.get(inPath))

    Smafl.convert(input) match
      case None =>
        Console.err.println(s"SmaflConvertMain: '$inPath' has no MATH table — nothing to convert")
        sys.exit(1)
      case Some(result) =>
        Files.write(Paths.get(outPath), result.font)
        Files.write(Paths.get(jsonPath), writeJson(result.entries).getBytes("UTF-8"))

        val variants = result.entries.count(_.kind == "variant")
        val parts    = result.entries.count(_.kind == "part")
        println(s"read $inPath (${input.length} bytes)")
        println(s"assigned ${result.entries.length} private-use codepoints: $variants variants, $parts assembly parts")
        result.entries.headOption.foreach { e =>
          val last = result.entries.last
          println(f"  range U+${e.pua}%04X … U+${last.pua}%04X")
        }
        println(s"wrote $outPath (${result.font.length} bytes)")
        println(s"wrote $jsonPath")

  /** Emit the assignment table as JSON: a top-level object with the PUA range and an array of per-codepoint
    * records, so a consumer with no MATH-table reader can still place the variant and assembly glyphs. */
  private def writeJson(entries: Vector[SmaflEntry]): String =
    val sb = new StringBuilder
    sb.append("{\n")
    entries.headOption.foreach { e =>
      sb.append(f"""  "puaStart": "U+${e.pua}%04X",\n""")
      sb.append(f"""  "puaEnd": "U+${entries.last.pua}%04X",\n""")
    }
    sb.append(s"""  "count": ${entries.length},\n""")
    sb.append("""  "entries": [""").append('\n')
    entries.zipWithIndex.foreach { case (e, i) =>
      val comma = if i == entries.length - 1 then "" else ","
      sb.append(
        f"""    {"pua": "U+${e.pua}%04X", "glyph": ${e.glyph}, "base": ${e.base}, "axis": "${e.axis}", """ +
          s""""kind": "${e.kind}", "index": ${e.index}, "advance": ${e.advance}, "extender": ${e.isExtender}}$comma\n""",
      )
    }
    sb.append("  ]\n}\n")
    sb.toString
