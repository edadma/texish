package io.github.edadma.texish

import java.io.{ByteArrayOutputStream, PrintStream}
import scala.collection.mutable

trait Testing:
  def test(src: String): String =
    val scopes = mutable.Stack[Map[String, Any]](Map.empty)
    val bytes = new ByteArrayOutputStream
    val parser: Parser = new Parser(Nil, Nil, blanks = true)
    val renderer =
      new Renderer:
        val config: Map[String, Any] =
          Map(
            "today" -> "MMMM d, y",
            "include" -> ".",
            "rounding" -> "HALF_EVEN",
          )
        val context: Any = null

        def output(v: Any): Unit = new PrintStream(bytes, false).print(display(v))

        def get(name: String): Any = scopes.top.getOrElse(name, UNDEFINED)

        def set(name: String, value: Any): Unit = scopes(0) += (name -> value)

        def enterScope(): Unit = scopes push scopes.top

        def exitScope(): Unit = scopes.pop
    val ast = parser.parse(src)

    renderer.render(ast)
    bytes.toString
