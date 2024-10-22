package io.github.edadma.texish

import io.github.edadma.char_reader.CharReader
import io.github.edadma.cross_platform.readFile
import pprint.pprintln

import java.io.File
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.util.regex.Matcher
import scala.compiletime.uninitialized
import scala.io
import scala.language.postfixOps
import scala.util.matching.Regex

abstract class Command(val name: String, val arity: Int, val eval: Boolean = true)
    extends ((CharReader, Parser, Renderer, List[Any], Map[String, Any], Any) => Any) {
  override def toString = s"""Command: "$name""""
}

object Command {

  class Const[T] {
    private var set = false
    private var value: T = uninitialized

    def apply(v: => T): T = {
      if (!set)
        synchronized {
          if (!set) {
            value = v
            set = true
          }
        }

      value
    }
  }

  def invoke(renderer: Renderer, lambda: AST, arg: Any): Any = {
    renderer.enterScope()
    renderer.set("_", arg)

    val res = renderer.eval(lambda)

    renderer.exitScope()
    res
  }

  private val escapeRegex = """([^\w _.,!:;?-])""".r

  val builtins: Seq[Command] =
    List(
      new Command(" ", 0) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = " "
      },
      new Command("*", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: BigDecimal, b: BigDecimal) => a * b
            case List(a, b)                         => problem(pos, s"expected arguments <number> <number>: $a, $b")
            case _                                  => problem(pos, "expected arguments <number> <number>")
          }
      },
      new Command("+", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: BigDecimal, b: BigDecimal)                     => a + b
            case List(a: collection.Map[_, _], b: collection.Map[_, _]) => (a ++ b).toMap
            case List(a: Seq[_], b: Seq[_])                             => a ++ b
            case List(a: Seq[_], b: Any)                                => a :+ b
            case List(a: Any, b: Seq[_])                                => a +: b
            case List(a: String, b: String)                             => a + b
            case List(a, b) => problem(pos, s"expected arguments <number> <number> or <string> <string>: $a, $b")
            case _          => problem(pos, "expected arguments <number> <number> or <string> <string>")
          }
      },
      new Command("-", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: BigDecimal, b: BigDecimal) => a - b
            case List(a, b)                         => problem(pos, s"expected arguments <number> <number>: $a, $b")
            case _                                  => problem(pos, "expected arguments <number> <number>")
          }
      },
      new Command("/", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: BigDecimal, b: BigDecimal) => a / b
            case List(a, b)                         => problem(pos, s"expected arguments <number> <number>: $a, $b")
            case _                                  => problem(pos, "expected arguments <number> <number>")
          }
      },
      new Command("/=", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args.head != args.tail.head
      },
      new Command("<", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: String, b: String)         => a < b
            case List(a: BigDecimal, b: BigDecimal) => a < b
            case List(a, b) => problem(pos, s"expected arguments <number> <number> or <string> <string>: $a, $b")
            case _          => problem(pos, "expected arguments <number> <number> or <string> <string>")
          }
      },
      new Command("<=", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: String, b: String)         => a <= b
            case List(a: BigDecimal, b: BigDecimal) => a <= b
            case List(a, b) => problem(pos, s"expected arguments <number> <number> or <string> <string>: $a, $b")
            case _          => problem(pos, "expected arguments <number> <number> or <string> <string>")
          }
      },
      new Command("=", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args.head == args.tail.head
      },
      new Command(">", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: String, b: String)         => a > b
            case List(a: BigDecimal, b: BigDecimal) => a > b
            case List(a, b) => problem(pos, s"expected arguments <number> <number> or <string> <string>: $a, $b")
            case _          => problem(pos, "expected arguments <number> <number> or <string> <string>")
          }
      },
      new Command(">=", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: String, b: String)         => a >= b
            case List(a: BigDecimal, b: BigDecimal) => a >= b
            case List(a, b) => problem(pos, s"expected arguments <number> <number> or <string> <string>: $a, $b")
            case _          => problem(pos, "expected arguments <number> <number> or <string> <string>")
          }
      },
      new Command("[]", 0) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          Nil
      },
      new Command("^", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: BigDecimal, b: BigDecimal) if b.isValidInt => a pow b.intValue
            case List(a, b) => problem(pos, s"expected arguments <number> <integer>: $a, $b")
            case _          => problem(pos, "expected arguments <number> <number>")
          }
      },
      new Command("abs", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          args.head match {
            case n: BigDecimal => n.abs
            case a             => problem(pos, s"not a number: $a")
          }
        }
      },
      new Command("append", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: Any, b: Seq[_])    => b :+ a
            case List(a: String, b: String) => b + a
            case List(a, b) => problem(pos, s"expected arguments <any> <sequence> or <string> <string>: $a, $b")
            case _          => problem(pos, "expected arguments <number> <number> or <string> <string>")
          }
      },
      new Command("ceil", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          args.head match {
            case n: BigDecimal => n.setScale(0, BigDecimal.RoundingMode.CEILING)
            case a             => problem(pos, s"not a number: $a")
          }
        }
      },
      new Command("contains", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: String, b: String) => a.contains(b)
            case List(a: Seq[_], b)         => a contains b
            case List(a: Map[_, _], b)      => a.asInstanceOf[Map[Any, Any]] contains b
            case List(a, b) =>
              problem(pos, s"expected arguments <string> <string> or <sequence> <any> or <object> <any>: $a, $b")
            case List(_, _, _, _*) => ???
            case List(_)           => ???
            case Nil               => ???
          }
      },
      new Command("date", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(format: String, date: TemporalAccessor) => DateTimeFormatter.ofPattern(format).format(date)
            case List(a, b)        => problem(pos, s"expected arguments <format> <date>, given $a, $b")
            case List(_, _, _, _*) => ???
            case List(_)           => ???
            case Nil               => ???
          }
      },
      new Command("default", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: Any, b: Any) => if (b == nil) a else b
            case List(_, _, _, _*)    => ???
            case List(_)              => ???
            case Nil                  => ???
          }
      },
      new Command("distinct", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(s: Seq[_]) => s.distinct
            case List(a)         => problem(pos, s"expected sequence argument: $a")
            case List(_, _, _*)  => ???
            case Nil             => ???
          }
      },
      new Command("downcase", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(s: String) => s.toLowerCase
            case List(a)         => problem(pos, s"expected string argument: $a")
            case List(_, _, _*)  => ???
            case Nil             => ???
          }
      },
      new Command("drop", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(n: BigDecimal, s: Seq[_]) if n.isValidInt => s drop n.toInt
            case List(n: BigDecimal, s: String) if n.isValidInt => s drop n.toInt
            case List(a, b) =>
              problem(pos, s"expected arguments <integer> <sequence> or <integer> <string>, given $a, $b")
            case List(_, _, _, _*) => ???
            case List(_)           => ???
            case Nil               => ???
          }
      },
      new Command("escape", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          args.head match {
            case s: String => escape(s)
            case a         => problem(pos, s"not a string: $a")
          }
        }
      },
      new Command("escapeFull", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          args.head match {
            case s: String => escapeFull(s)
            case a         => problem(pos, s"not a string: $a")
          }
        }
      },
      new Command("escapeOnce", 1) {
        private val regex = """&#?\w+;""".r

        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          args.head match {
            case s: String =>
              val it = regex.findAllIn(s)
              var last = 0
              val buf = new StringBuilder

              while (it.hasNext) {
                val m = it.next

                buf ++= escape(s.substring(last, it.start))
                buf ++= it.matched
                last = it.end
              }

              buf ++= escape(s.substring(last, s.length))
              buf.toString
            case a => problem(pos, s"not a string: $a")
          }
        }
      },
      new Command("filter", 2, false) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          (args.head, renderer eval args.tail.head.asInstanceOf[AST]) match {
            case (lambda: AST, s: Seq[_]) => s filter (e => truthy(invoke(renderer, lambda, e)))
            case (a, b)                   => problem(pos, s"expected arguments <lambda <sequence>, given $a, $b")
          }
      },
      new Command("floor", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          args.head match {
            case n: BigDecimal => n.setScale(0, BigDecimal.RoundingMode.FLOOR)
            case a             => problem(pos, s"not a number: $a")
          }
        }
      },
      new Command("head", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(s: String) => s.head
            case List(s: Seq[_]) => s.head
            case List(a)         => problem(pos, s"expected string or sequence argument: $a")
            case List(_, _, _*)  => ???
            case Nil             => ???
          }
      },
      new Command("accent", 2) {
        private val accents =
          Map(
            ("'", "e") -> "é",
            ("'", "E") -> "É",
            ("`", "e") -> "è",
            ("`", "E") -> "È",
            ("^", "e") -> "ê",
            ("^", "E") -> "Ê",
            ("\"", "e") -> "ë",
            ("\"", "E") -> "Ë",
            ("`", "a") -> "à",
            ("`", "A") -> "À",
            ("^", "a") -> "â",
            ("^", "A") -> "Â",
            ("\"", "u") -> "ü",
            ("\"", "U") -> "Ü",
            ("^", "u") -> "û",
            ("^", "U") -> "Û",
            ("\"", "i") -> "ï",
            ("\"", "I") -> "Ï",
            ("^", "i") -> "î",
            ("^", "I") -> "Î",
            ("c", "c") -> "ç",
            ("c", "C") -> "Ç",
          )

        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(accent: String, base: String) =>
              accents get (accent, base) match
                case Some(c) => c
                case None    => problem(pos, s"accented character not found: $accent $base")
            case _ => problem(pos, "expected <accent> <base>")
          }
      },
      new Command("isEmpty", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          args.head match {
            case s: String               => s.isEmpty
            case m: collection.Map[_, _] => m.isEmpty
            case s: Seq[_]               => s.isEmpty
            case a                       => problem(pos, s"expected string, map or sequence argument: $a")
          }
        }
      },
      new Command("join", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(sep: String, s: Seq[_]) => s mkString sep
            case List(a, b)        => problem(pos, s"expected arguments <separator> <sequence>, given $a, $b")
            case List(_, _, _, _*) => ???
            case List(_)           => ???
            case Nil               => ???
          }
      },
      new Command("last", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(s: String) => s.last
            case List(s: Seq[_]) => s.last
            case List(a)         => problem(pos, s"expected string or sequence argument: $a")
            case List(_, _, _*)  => ???
            case Nil             => ???
          }
      },
      new Command("lit", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args.head
      },
      new Command("map", 2, false) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          (args.head, renderer eval args.tail.head.asInstanceOf[AST]) match {
            case (LiteralAST(f: String), s: Seq[_]) => s.asInstanceOf[Seq[Map[String, Any]]] map (_ getOrElse (f, nil))
            case (lambda: AST, s: Seq[_])           => s map (invoke(renderer, lambda, _))
            case (a, b) =>
              problem(pos, s"expected arguments <variable> <sequence> or <lambda <sequence>>, given $a, $b")
          }
      },
      new Command("max", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: BigDecimal, b: BigDecimal) => a max b
            case List(a, b)                         => problem(pos, s"expected arguments <number> <number>: $a, $b")
            case List(_, _, _, _*)                  => ???
            case List(_)                            => ???
            case Nil                                => ???
          }
      },
      new Command("min", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: BigDecimal, b: BigDecimal) => a min b
            case List(a, b)                         => problem(pos, s"expected arguments <number> <number>: $a, $b")
            case List(_, _, _, _*)                  => ???
            case List(_)                            => ???
            case Nil                                => ???
          }
      },
      new Command("negate", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          args.head match {
            case n: BigDecimal => -n
            case a             => problem(pos, s"not a number: $a")
          }
        }
      },
      new Command("nil", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          args.head
          nil
        }
      },
      new Command("nonEmpty", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          args.head match {
            case s: String               => s.nonEmpty
            case m: collection.Map[_, _] => m.nonEmpty
            case s: Seq[_]               => s.nonEmpty
            case a                       => problem(pos, s"expected string, map or sequence argument: $a")
          }
        }
      },
      new Command("normalize", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args.head.toString.trim.replaceAll("""\s+""", " ")
      },
      new Command("now", 0) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          ZonedDateTime.now
      },
      new Command("range", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(start: BigDecimal, end: BigDecimal) => start to end by 1
            case List(a, b)        => problem(pos, s"expected arguments <number> <number>: $a, $b")
            case List(_, _, _, _*) => ???
            case List(_)           => ???
            case Nil               => ???
          }
      },
      new Command("rem", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(a: BigDecimal, b: BigDecimal) => a remainder b
            case List(a, b)                         => problem(pos, s"expected arguments <number> <number>: $a, $b")
            case List(_, _, _, _*)                  => ???
            case List(_)                            => ???
            case Nil                                => ???
          }
      },
      new Command("remove", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(l: String, r: String) => r.replace(l, "")
            case List(a, b)                 => problem(pos, s"expected arguments <string> <string>: $a, $b")
            case List(_, _, _, _*)          => ???
            case List(_)                    => ???
            case Nil                        => ???
          }
      },
      new Command("removeFirst", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(l: String, r: String) => r.replaceFirst(Matcher.quoteReplacement(l), "")
            case List(a, b)                 => problem(pos, s"expected arguments <string> <string>: $a, $b")
            case List(_, _, _, _*)          => ???
            case List(_)                    => ???
            case Nil                        => ???
          }
      },
      new Command("replace", 3) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(l1: String, l2: String, r: String) => r.replace(l1, l2)
            case List(a, b, c)        => problem(pos, s"expected arguments <string> <string> <string>: $a, $b, $c")
            case List(_, _, _, _, _*) => ???
            case List(_, _)           => ???
            case List(_)              => ???
            case Nil                  => ???
          }
      },
      new Command("replaceFirst", 3) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(l1: String, l2: String, r: String) => r.replaceFirst(Matcher.quoteReplacement(l1), l2)
            case List(a, b, c)        => problem(pos, s"expected arguments <string> <string> <string>: $a, $b, $c")
            case List(_, _, _, _, _*) => ???
            case List(_, _)           => ???
            case List(_)              => ???
            case Nil                  => ???
          }
      },
      new Command("reverse", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(s: String) => s.reverse
            case List(s: Seq[_]) => s.reverse
            case List(a)         => problem(pos, s"expected string or sequence argument: $a")
            case List(_, _, _*)  => ???
            case Nil             => ???
          }
      },
      new Command("round", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          (args.head, optional.getOrElse("scale", 0)) match {
            case (n: BigDecimal, scale: Number) => round(n, scale.intValue, renderer.config)
            case (a, b)                         => problem(pos, s"not a number: $a, $b")
          }
        }
      },
      new Command("size", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(s: String)               => s.length
            case List(s: Seq[_])               => s.length
            case List(s: collection.Map[_, _]) => s.size
            case List(a)                       => problem(pos, s"expected string or sequence argument: $a")
            case List(_, _, _*)                => ???
            case Nil                           => ???
          }
      },
      new Command("slice", 3) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          args match {
            case List(start: BigDecimal, end: BigDecimal, s: String) if start.isValidInt && end.isValidInt =>
              s.slice(start.intValue, end.intValue)
            case List(start: BigDecimal, end: BigDecimal, s: Seq[_]) if start.isValidInt && end.isValidInt =>
              s.slice(start.intValue, end.intValue)
            case List(a, b, c) =>
              problem(pos, s"expected arguments <start> <end> <string> or <start> <end> <sequence>: $a, $b, $c")
            case List(_, _, _, _, _*) => ???
            case List(_, _)           => ???
            case List(_)              => ???
            case Nil                  => ???
          }
        }
      },
      new Command("sort", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          val on = optional get "on" map (_.toString)
          val desc = (optional get "order" map (_.toString)) contains "desc"

          def comp(a: Any, b: Any) = if (desc) !lt(a, b) else lt(a, b)

          def lt(a: Any, b: Any) =
            (a, b) match {
              case (a: Comparable[_], b: Comparable[_]) => (a.asInstanceOf[Comparable[Any]].compareTo(b)) < 0
              case _                                    => a.toString < b.toString
            }

          args match {
            case List(s: Seq[_]) if on.isDefined =>
              s.asInstanceOf[Seq[Map[String, Any]]] sortWith ((a, b) => comp(a(on.get), b(on.get)))
            case List(s: Seq[_]) => s sortWith comp
            case List(a)         => problem(pos, s"expected sequence argument: $a")
            case List(_, _, _*)  => ???
            case Nil             => ???
          }
        }
      },
      new Command("split", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(sep: String, s: String) => s.split(sep).toVector
            case List(a, b)                   => problem(pos, s"expected arguments <string> <string>, given $a, $b")
            case List(_, _, _, _*)            => ???
            case List(_)                      => ???
            case Nil                          => ???
          }
      },
      new Command("tail", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(s: String) => s.tail
            case List(s: Seq[_]) => s.tail
            case List(a)         => problem(pos, s"expected string or sequence argument: $a")
            case List(_, _, _*)  => ???
            case Nil             => ???
          }
      },
      new Command("take", 2) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(n: BigDecimal, s: Seq[_]) if n.isValidInt => s take n.toInt
            case List(n: BigDecimal, s: String) if n.isValidInt => s take n.toInt
            case List(a, b) =>
              problem(pos, s"expected arguments <integer> <sequence> or <integer> <string>, given $a, $b")
            case List(_, _, _, _*) => ???
            case List(_)           => ???
            case Nil               => ???
          }
      },
      new Command("timestamp", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(s: String) => ZonedDateTime.parse(s)
            case List(millis: BigDecimal) if millis.isValidLong =>
              Instant.ofEpochMilli(millis.longValue).atOffset(ZoneOffset.UTC).toZonedDateTime
            case List(a)        => problem(pos, s"expected string or integer argument: $a")
            case List(_, _, _*) => ???
            case Nil            => ???
          }
      },
      new Command("toInteger", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          val x = args.head

          number(x) match {
            case None                => problem(pos, s"not a number: $x")
            case Some(n: BigDecimal) => if (n.isWhole) n else n.setScale(0, BigDecimal.RoundingMode.DOWN)
          }
        }
      },
      new Command("toNumber", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any = {
          val x = args.head

          number(x) match {
            case None    => problem(pos, s"not a number: $x")
            case Some(n) => n
          }
        }
      },
      new Command("toString", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          display(args.head)
      },
      new Command("today", 0) {
        val format = new Const[DateTimeFormatter]

        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          ZonedDateTime.now.format(format(DateTimeFormatter.ofPattern(renderer.config("today").toString)))
      },
      new Command("trim", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(s: String) => s.trim
            case List(a)         => problem(pos, s"expected string argument: $a")
            case _               => problem(pos, "expected string argument")
          }
      },
      new Command("u", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(n: BigDecimal) if n.isValidChar => n.toChar
            case List(n: BigDecimal)                  => problem(pos, s"number not a valid character: $n")
            case List(a)                              => problem(pos, s"expected number argument: $a")
            case _                                    => problem(pos, "expected number argument")
          }
      },
      new Command("upcase", 1) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          args match {
            case List(s: String) => s.toUpperCase
            case List(a)         => problem(pos, s"expected string argument: $a")
            case _               => problem(pos, "expected string argument")
          }
      },
      new Command("{}", 0) {
        def apply(
            pos: CharReader,
            parser: Parser,
            renderer: Renderer,
            args: List[Any],
            optional: Map[String, Any],
            context: Any,
        ): Any =
          Map()
      },
    )

  def escape(s: String) =
    escapeRegex.replaceSomeIn(s, { m => Entity(m group 1 head) map (e => s"&$e;") })

  def escapeFull(s: String) =
    escapeRegex.replaceAllIn(s, { m => s"&${Entity.full(m group 1 head)};" })

}
