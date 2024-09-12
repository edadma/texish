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
  val actives =
    List(
      new Active("<") {
        def apply(pos: CharReader, r: Renderer): String = {
          "lt"
        }
      },
    )
  val commands =
    List(
      new Command("verses", 1):
        def apply(
            pos: CharReader,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match
            case List(a: String) => println(s"verses: $a")
            case List(a)         => problem(pos, s"expected arguments <string>: $a")
            case _               => problem(pos, "expected arguments <string>")
    )
  val parser = new Parser(Command.builtins ++ commands, actives, blanks = true)
  val renderer = new Renderer(parser, config, _.mkString, null, x => pprintln(x))
  val src =
    """
    |\verses{asdf qwer}
    """.trim.stripMargin
  val ast = parser.parse(src)

  pprintln(ast)
  renderer.render(ast)
