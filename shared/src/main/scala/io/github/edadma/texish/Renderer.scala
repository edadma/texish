package io.github.edadma.texish

import java.io.{ByteArrayOutputStream, PrintStream}
import scala.collection.mutable
import scala.io
import scala.language.postfixOps

class Renderer(val parser: Parser, val config: Map[String, Any], group: Seq[Any] => Any, context: Any) {

  val globals = new mutable.HashMap[String, Any]
  val scopes = new mutable.Stack[mutable.HashMap[String, Any]]

  def setVar(name: String, value: Any): Unit =
    scopes find (_ contains name) match {
      case None        => globals(name) = value
      case Some(scope) => scope(name) = value
    }

  def getVar(name: String, locals: Map[String, Any]): Any =
    scopes find (_ contains name) match {
      case None =>
        locals get name match {
          case None =>
            globals get name match {
              case None    => problem(null, s"variable '$name' not found") // nil
              case Some(v) => v
            }
          case Some(v) => v
        }
      case Some(scope) => scope(name)
    }

  def enterScope(): Unit = scopes push new mutable.HashMap

  def exitScope(): Unit = scopes.pop

  def render(ast: AST, assigns: collection.Map[String, Any], out: Any => Unit): Unit = {
    def output(ast: AST): Unit = 
      out(deval(ast))

    globals ++= assigns

    ast match {
      case GroupAST(b) => b foreach output
      case _           => output(ast)
    }
  }

  // todo: think about whether the implicit codec is needed
  def capture(ast: AST, assigns: collection.Map[String, Any])(implicit codec: io.Codec) = {
    val bytes = new ByteArrayOutputStream

    render(ast, assigns, (new PrintStream(bytes, false, codec.name)).print)
    bytes.toString
  }

  def deval(ast: AST) = display(eval(ast))

  def teval(ast: AST) = truthy(eval(ast))

  def eval(ast: AST): Any =
    ast match {
      case SetAST(v, expr) =>
        setVar(v, eval(expr))
        nil
      case InAST(cpos, v, epos, expr) =>
        eval(expr) match {
          case s: Seq[_] =>
            if (scopes.isEmpty) problem(cpos, "not inside a loop")

            ForGenerator(v, s)
          case a => problem(epos, s"expected a sequence: $a")
        }
      case DotAST(epos, expr, kpos, key) =>
        (eval(expr), eval(key)) match {
          case (m: collection.Map[_, _], k) =>
            m.asInstanceOf[collection.Map[Any, Any]] get k match {
              case None    => nil
              case Some(v) => v
            }
          case (s: String, idx: BigDecimal) if idx.isValidInt => s(idx.intValue)
          case (s: Seq[_], idx: BigDecimal) if idx.isValidInt => s(idx.intValue)
          case (o, k)                                         => problem(epos, s"not indexable: $o[$k]")
        }
      case SeqAST(seq)         => seq map eval
      case ObjectAST(seq)      => seq map eval grouped 2 map { case Vector(a, b) => a -> b } toMap
      case NotAST(expr)        => !teval(expr)
      case AndAST(left, right) => teval(left) && teval(right)
      case OrAST(left, right) =>
        val l = eval(left)

        if (truthy(l)) l
        else {
          val r = eval(right)

          if (truthy(r))
            r
          else
            false
        }
      case MacroAST(Macro(parms, body), args) =>
        enterScope()
        scopes.top ++= parms zip args map { case (k, v) => (k, eval(v)) }

        val res = eval(body)

        exitScope()
        res
      case MatchAST(expr, cases, els) =>
        val e = eval(expr)

        cases find { case (exp, _) => e == eval(exp) } match {
          case None =>
            els match {
              case None     => problem(null, s"match error") // nil
              case Some(no) => eval(no)
            }
          case Some((_, yes)) => eval(yes)
        }
      case GroupAST(statements) =>
        if (statements.length == 1)
          eval(statements.head)
        else
          group(statements map deval)
      case LiteralAST(v) => v
      case CommandAST(pos, c, args, optional) =>
        c(pos, this, if (c.eval) args map eval else args, optional map { case (k, v) => k -> eval(v) }, context)
      case ActiveAST(pos, a) => a(pos, this)
      case ForAST(pos, expr, body, els) =>
        val buf = new StringBuilder

        enterScope()

        val (in, seq) =
          eval(expr) match {
            case ForGenerator(v, s)      => (Some(v), s)
            case s: Seq[Any]             => (None, s)
            case m: collection.Map[_, _] => (None, List(m))
            case a                       => problem(pos, s"expected sequence or map: $a")
          }

        try {
          val len = BigDecimal(seq.length)

          seq.zipWithIndex foreach { case (e, idx) =>
            try {
              val forloop =
                Map(
                  "first" -> (idx == 0),
                  "index" -> (idx + BigDecimal(1)),
                  "indexz" -> BigDecimal(idx),
                  "last" -> (idx == len - 1),
                  "length" -> len,
                  "rindex" -> (len - idx),
                  "rindexz" -> (len - idx - 1),
                  "element" -> e,
                )
              scopes.top("forloop") = forloop

              in match {
                case None if e.isInstanceOf[collection.Map[_, _]] =>
                  e.asInstanceOf[collection.Map[String, Any]] foreach { case (k, v) =>
                    scopes.top(k) = v
                  }
                case None    =>
                case Some(v) => scopes.top(v) = e
              }

              buf ++= deval(body)
            } catch {
              case _: ContinueException =>
            }
          }

          els match {
            case None        =>
            case Some(after) => buf ++= deval(after)
          }
        } catch {
          case _: BreakException =>
        }

        exitScope()
        buf.toString
      case BreakAST(pos) =>
        if (scopes isEmpty)
          problem(pos, s"not inside a 'for' loop")

        throw new BreakException
      case ContinueAST(pos) =>
        if (scopes isEmpty)
          problem(pos, s"not inside a 'for' loop")

        throw new ContinueException
      case IfAST(cond, els) =>
        cond find { case (expr, _) => teval(expr) } match {
          case None =>
            els match {
              case None     => nil
              case Some(no) => eval(no)
            }
          case Some((_, yes)) => eval(yes)
        }
      case UnlessAST(cond, body, els) =>
        if (!teval(cond))
          eval(body)
        else
          els match {
            case None      => nil
            case Some(yes) => eval(yes)
          }
      case VariableAST(v) => getVar(v, Map())
    }

  private class BreakException extends RuntimeException

  private class ContinueException extends RuntimeException

  private case class ForGenerator(v: String, s: Seq[Any])

}
