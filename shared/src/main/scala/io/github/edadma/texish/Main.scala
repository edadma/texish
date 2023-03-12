package io.github.edadma.texish

import io.github.edadma.char_reader.CharReader

import scala.collection.mutable
import scala.io

object Main extends App {

  val config =
    Map(
      "today" -> "MMMM d, y",
      "include" -> ".",
      "rounding" -> "HALF_EVEN",
    )
  val assigns = new mutable.HashMap[String, Any]
  val actives =
    List(
      new Active("<") {
        def apply(pos: CharReader, r: Renderer) = {
          "lt"
        }
      },
    )
  val parser = new Parser(Command.standard, actives, blanks = true)
  val renderer = new Renderer(parser, config, _.mkString)
  val src =
    """
      |<a  c>
    """.trim.stripMargin
  renderer.render(parser.parse(src), assigns, Console.print)

}
