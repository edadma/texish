package io.github.edadma.texish

import io.github.edadma.char_reader.CharReader

import scala.collection.mutable
import scala.io

import pprint.pprintln

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
  val renderer = new Renderer(parser, config, _.mkString, null)
  val src =
    """
    |asdf
    |
    |qwer
    |
    """.trim.stripMargin
  val ast = parser.parse(src)

  pprintln(ast)
  renderer.render(ast, assigns, x => pprintln(x))
