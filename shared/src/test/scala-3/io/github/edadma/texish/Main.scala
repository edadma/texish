package io.github.edadma.texish

import io.github.edadma.char_reader.CharReader
import scala.collection.mutable

import pprint.pprintln

@main def run(): Unit =
  val actives =
    List(
      new Active("<") {
        def apply(pos: CharReader, r: Renderer, context: Any): String = {
          "lt"
        }
      },
    )
  val commands =
    List(
      new Command("verses", 1):
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match
            case List(a: String) => println(s"verses: $a")
            case List(a)         => problem(pos, s"expected arguments <string>: $a")
            case _               => problem(pos, "expected arguments <string>"),
    )
  val parser = new Parser(commands, actives, blanks = true)
  val scopes = mutable.Stack[Map[String, Any]](Map.empty)
  val renderer =
    new Renderer:
      val parser: Parser = parser

      val config: Map[String, Any] =
        Map(
          "today" -> "MMMM d, y",
          "include" -> ".",
          "rounding" -> "HALF_EVEN",
        )
      val context: Any = null

      def output(v: Any): Unit = println(display(v))

      def group(vals: Seq[Any]): Any = vals.mkString

      def get(name: String): Any = scopes.top.getOrElse(name, UNDEFINED)

      def set(name: String, value: Any): Unit = scopes(0) += (name -> value)

      def enterScope(): Unit = scopes push scopes.top

      def exitScope(): Unit = scopes.pop
  val src =
    """
    |asdf <<< zxcv
    """.trim.stripMargin
  val ast = parser.parse(src)

  pprintln(ast)
  renderer.render(ast)
