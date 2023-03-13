package io.github.edadma.texish

import io.github.edadma.char_reader.CharReader

import scala.collection.mutable
import scala.io

@main def run(): Unit =
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
        def apply(pos: CharReader, r: Renderer): String = {
          "lt"
        }
      },
    )
  val parser = new Parser(Command.builtins, actives, blanks = true)
  val renderer = new Renderer(parser, config, _.mkString)
  val src =
    """
    |\asdf
    |<a  c>
    |
    """.trim.stripMargin
  val ast = parser.parse(src)

  println(ast)
  renderer.render(ast, assigns, Console.print)
