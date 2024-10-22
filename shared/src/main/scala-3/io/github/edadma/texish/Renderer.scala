package io.github.edadma.texish

import scala.language.postfixOps

import pprint.pprintln

abstract class Renderer:
  val config: Map[String, Any]
  val context: Any

  def output(v: Any): Unit

  def set(name: String, value: Any): Unit

  def get(name: String): Any

  def set(vars: Seq[(String, Any)]): Unit = vars foreach { (k, v) => set(k, v) }

  def enterScope(): Unit

  def exitScope(): Unit

  val undefined: UNDEFINED.type = UNDEFINED

  def render(ast: AST): Unit =
    eval(ast) match
      case () =>
      case v  => output(v)

  def deval(ast: AST): String = display(eval(ast))

  def teval(ast: AST): Boolean = truthy(eval(ast))

  infix def eval(ast: AST): Any =
    ast match {
      case SetAST(v, expr) =>
        set(v, eval(expr))
      case InAST(cpos, v, epos, expr) =>
        eval(expr) match {
          case s: Seq[_] =>
//            if (scopes.isEmpty) problem(cpos, "not inside a loop") // todo: need a better way to check if inside a loop

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
      case SeqAST(seq) => seq map eval
      case ObjectAST(seq) =>
        seq map eval grouped 2 map {
          case Vector(a, b) => a -> b
          case _            => problem(null, "object must be a sequence of key/value pairs")
        } toMap
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
        set(parms zip args map { (k, v) => (k, eval(v)) })

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
        enterScope()

        val res = statements map eval filterNot (_ == ())

        exitScope()

        if (res.length == 1) res.head
        else res
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
              set("forloop", forloop)

              in match {
                case None if e.isInstanceOf[collection.Map[?, ?]] =>
                  e.asInstanceOf[collection.Map[String, Any]] foreach { case (k, v) =>
                    set(k, v)
                  }
                case None    =>
                case Some(v) => set(v, e)
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
//        if (scopes isEmpty) // todo: inside a loop
//          problem(pos, s"not inside a 'for' loop")

        throw new BreakException
      case ContinueAST(pos) =>
//        if (scopes isEmpty) // todo: inside a loop
//          problem(pos, s"not inside a 'for' loop")

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
      case VariableAST(v) => get(v)
    }

  private class BreakException extends RuntimeException

  private class ContinueException extends RuntimeException

  private case class ForGenerator(v: String, s: Seq[Any])
