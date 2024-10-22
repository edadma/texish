//@
package io.github.edadma.texish

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io
import scala.language.postfixOps

class Parser(
    commands: List[Command],
    actives: List[Active],
    blanks: Boolean = false,
    var csDelim: String = "\\",
    var beginDelim: String = "{",
    var endDelim: String = "}",
    var pipeDelim: String = "|",
    var rawBeginDelim: String = "<<<",
    var rawEndDelim: String = ">>>",
) {

  val commandMap: Map[String, Command] = (Command.builtins ++ commands) map (c => c.name -> c) toMap
  val activeDelims: List[String] = actives map (_.name) sortWith (_ > _)
  private val varRegex = """\.([^.]*)""".r
  private val unicodeRegex = "\\\\u[0-9a-fA-F]{4}".r
  private val keywords = List("true", "false", "null")

  private def escapes(s: String) =
    unicodeRegex.replaceAllIn(
      s
        .replace("""\b""", "\b")
        .replace("""\t""", "\t")
        .replace("""\f""", "\f")
        .replace("""\n""", "\n")
        .replace("""\r""", "\r")
        .replace("""\\""", "\\")
        .replace("""\"""", "\"")
        .replace("""\'""", "\'"),
      m => Integer.parseInt(m.matched.substring(2), 16).toChar.toString,
    )

  val macros = new mutable.HashMap[String, Macro]

  def parse(src: String): AST = parse(CharReader.fromString(src))

  def parse(src: CharReader): AST =
    parseStatements(src) match {
      case (r1, b) if r1 eoi => b
      case (r1, _)           => problem(r1, s"expected end of input: $r1")
    }

  def parseStatements(r: CharReader, v: Vector[AST] = Vector()): (CharReader, GroupAST) =
    if (r.eoi || lookahead(r, endDelim))
      (r, GroupAST(v))
    else
      parseStatement(r) match {
        case (r1, null) => parseStatements(r1, v)
        case (r1, s)    => parseStatements(r1, v :+ s)
      }

  def parseGroup(r: CharReader, v: Vector[AST] = Vector()): (CharReader, GroupAST) = {
    val (r1, g) = parseStatements(r)

    matches(r1, endDelim) match {
      case Some(r2) => (r2, g)
      case None =>
        problem(r, "unexpected end of input")
    }
  }

  def parseStatic(r: CharReader): (CharReader, AST) = {
    val (r1, s) = consumeCond(
      r,
      r =>
        !r.eoi && !lookahead(r, csDelim) && !lookahead(r, beginDelim) && !lookahead(r, endDelim) &&
          !lookahead(r, activeDelims) &&
          !(blanks && matches(r, ' ', '\t', '\n')),
    )

    (r1, LiteralAST(s))
  }

  def parseActive(r: CharReader, as: List[Active] = actives): Option[(CharReader, AST)] =
    as match {
      case Nil => None
      case h :: t =>
        matches(r, h.name) match {
          case None     => parseActive(r, t)
          case Some(r1) => Some(r1, ActiveAST(r, h))
        }
    }

  def parseStatement(r: CharReader): (CharReader, AST) =
    parseControlSequence(r) match {
      case None =>
        matches(r, beginDelim) match {
          case None =>
            parseActive(r) match {
              case None =>
                if blanks && matches(r, ' ', '\t') then (skip(r, p => p.ch != ' ' && p.ch != '\t'), LiteralAST(" "))
                else if blanks && r.ch == '\n' then (r.next, LiteralAST("\n"))
                else parseStatic(r)
              case Some(a) => a
            }
          case Some(r1) => parseGroup(r1)
        }
      case Some((r1, "#")) =>
        matches(skip(r1, lookahead(_, csDelim + "#")), csDelim + "#") match {
          case None     => problem(r, "unclosed comment")
          case Some(r2) => (r2, null)
        }
      case Some((r1, "delim")) =>
        val (r2, c) = parseStringArgument(r1)
        val (r3, b) = parseStringArgument(r2)
        val (r4, e) = parseStringArgument(r3)

        csDelim = c
        beginDelim = b
        endDelim = e
        (r4, null)
      case Some((r1, "raw")) =>
        val (r2, b) = parseStringArgument(r1)
        val (r3, e) = parseStringArgument(r2)

        rawBeginDelim = b
        rawEndDelim = e
        (r3, null)
      case Some((r1, "include")) =>
        val (r2, path) = parseStringArgument(r1)

        parse(CharReader.fromFile(path))
        (r2, null)
      case Some((r1, "def")) =>
        val (r2, v) = parseStringArguments(r1)

        if (v isEmpty)
          problem(r1, "expected name of macro")

        val name = v.head

        if (r2.eoi || !lookahead(r2, beginDelim))
          problem(r2, s"expected body of definition for $name")

        val mac = Macro(v.tail, null)

        macros(name) = mac

        val (r3, body) = parseRegularArgument(r2)

        mac.body = body
        (r3, null)
      case Some((r1, name)) => parseCommand(r, name, r1, true)
    }

  private def parseList(r: CharReader, begin: Boolean) = {
    @tailrec
    def parseList(r: CharReader, buf: ArrayBuffer[AST] = new ArrayBuffer): (CharReader, Vector[AST]) = {
      matches(r, endDelim) match {
        case None =>
          val (r1, ast) = parseRegularArgument(r)

          buf += ast
          parseList(r1, buf)
        case Some(r1) => (r1, buf.toVector)
      }
    }

    if (begin)
      matches(r, beginDelim) match {
        case None     => problem(r, s"expected list")
        case Some(r1) => parseList(r1)
      }
    else
      parseList(r)
  }

  def parseStringArguments(r: CharReader, v: Vector[String] = Vector()): (CharReader, Vector[String]) = {
    val r1 = skipSpace(r)

    if (r1.eoi || lookahead(r1, beginDelim))
      (r1, v)
    else
      parseString(r1) match {
        case (r2, s) => parseStringArguments(r2, v :+ s)
      }
  }

  private def nameFirst(c: Char) = c.isLetter || c == '_'

  private def nameRest(c: Char) = c.isLetter || c == '_' || c == '.'

  private def parseFilter(r: CharReader) =
    parseControlSequence(r) match {
      case None => parseControlSequenceName(r)
      case cs   => cs
    }

  def parseControlSequence(r: CharReader): Option[(CharReader, String)] =
    if (r.eoi)
      None
    else
      matches(r, csDelim) match {
        case Some(r1) => parseControlSequenceName(r1)
        case None     => None
      }

  private def parseName(r: CharReader) =
    if (r eoi)
      None
    else if (nameFirst(r.ch))
      Some(consume(r, nameRest))
    else
      None

  private def parseControlSequenceName(r: CharReader) =
    if (r eoi) None
    else {
      val (r1, s) =
        if (nameFirst(r.ch))
          consumeCond(r, r => !r.eoi && nameRest(r.ch) && !(r.ch == '.' && (r.next.eoi || !nameRest(r.next.ch))))
        else if (r.ch.isWhitespace)
          (r.next, " ")
        else if (r.ch.isDigit)
          problem(r, s"control sequence name can't start with a digit")
        else
          consumeCond(
            r,
            r =>
              !(r.eoi ||
                r.ch.isLetterOrDigit || r.ch == '_' || r.ch.isWhitespace || lookahead(r, csDelim)),
          )

      Some((skipSpace(r1), s))
    }

  def keyword(r: CharReader, words: List[String]): Option[(CharReader, String)] =
    words match {
      case Nil => None
      case h :: t =>
        matches(r, h) match {
          case None => keyword(r, t)
          case Some(r1) =>
            if (r1.eoi || !r1.ch.isLetter && r1.ch != '_')
              Some((r1, h))
            else
              keyword(r, t)
        }
    }

  def matches(r: CharReader, c: Char*): Boolean = !r.eoi && c.contains(r.ch)

  def matches(r: CharReader, s: String, idx: Int = 0): Option[CharReader] =
    if (idx == s.length)
      Some(r)
    else if (r.eoi || r.ch != s.charAt(idx))
      None
    else
      matches(r.next, s, idx + 1)

  def lookahead(r: CharReader, s: String): Boolean = matches(r, s) nonEmpty

  def lookahead(r: CharReader, delims: List[String]): Boolean =
    delims match {
      case Nil => false
      case h :: t =>
        if (lookahead(r, h))
          true
        else
          lookahead(r, t)
    }

  def consumeCond(
      r: CharReader,
      cond: CharReader => Boolean,
      buf: StringBuilder = new StringBuilder,
  ): (CharReader, String) =
    if (cond(r)) {
      buf += r.ch
      consumeCond(r.next, cond, buf)
    } else
      (r, buf toString)

  def consume(r: CharReader, set: Char => Boolean, buf: StringBuilder = new StringBuilder): (CharReader, String) =
    consumeCond(r, r => !r.eoi && set(r.ch), buf)

  def consumeStringLiteral(r: CharReader): (CharReader, String) =
    val del = r.ch
    var first = true
    var prev = ' '

    def cond(cr: CharReader) =
      val res =
        if (cr eoi)
          problem(r, "unclosed string literal")
        else
          cr ch match
            case '\\' if cr.next.eoi             => problem(cr, "unclosed string literal")
            case `del` if !first && prev == '\\' => true
            case `del`                           => false
            case _                               => true

      first = false
      prev = cr.ch
      res

    val (r1, s) = consumeCond(r.next, cond)

    (r1.next, escapes(s))

  def parseLiteralArgument(r: CharReader): (CharReader, Any) =
    r.ch match {
      case '"' | '\'' => consumeStringLiteral(r)
      case '0' if !r.next.eoi && r.next.ch == 'x' =>
        consume(r.next.next, "0123456789abcdefABCDEF" contains _) match {
          case (r1, s) => (r1, BigDecimal(Integer.parseInt(s, 16)))
        }
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        consume(r, c => c.isDigit || c == '.') match {
          case (r1, n) if isNumber(n) => (r1, BigDecimal(n))
          case s                      => s
        }
      case '-' =>
        if (r.next.eoi)
          (r.next, "-")
        else
          parseLiteralArgument(r.next) match {
            case (r1, s: String)     => (r1, s"-$s")
            case (r1, n: BigDecimal) => (r1, -n)
            case _                   => problem(r, "something bad happened")
          }
      case _ =>
        keyword(r, keywords) match {
          case None => parseString(r)
          case Some((r1, k)) =>
            (
              r1,
              k match {
                case "true"  => true
                case "false" => false
                case "null"  => null
              },
            )
        }
    }

  def parseString(r: CharReader): (CharReader, String) = consumeCond(
    r,
    r => !r.eoi && !r.ch.isWhitespace && !lookahead(r, csDelim) && !lookahead(r, beginDelim) && !lookahead(r, endDelim),
  )

  def parseStringWhitespace(r: CharReader): (CharReader, String) = consume(r, !_.isWhitespace)

  def parseStringArgument(r: CharReader): (CharReader, String) = {
    val r1 = skipSpace(r)

    if (r1 eoi)
      problem(r1, "expected string argument")

    parseStringWhitespace(r1)
  }

  def parseRegularArgument(r: CharReader): (CharReader, AST) = {
    val r1 = skipSpace(r)

    if (r1 eoi)
      problem(r1, "expected command argument")

    parseControlSequence(r1) match {
      case None =>
        matches(r1, beginDelim) match {
          case Some(r2) => parseGroup(r2)
          case None =>
            parseLiteralArgument(r1) match {
              case (r2, "_") => (r2, VariableAST("_"))
              case (r2, s)   => (skipSpace(r2), LiteralAST(s))
            }
        }
      case Some((r2, name)) => parseCommand(r1, name, r2, false)
    }
  }

  def dotExpression(pos: CharReader, v: String): AST = {
    @tailrec
    def fields(start: Int, expr: AST): AST =
      v.indexOf('.', start) match {
        case -1 => expr
        case dot =>
          v.indexOf('.', dot + 1) match {
            case -1  => DotAST(pos, expr, pos, LiteralAST(v.substring(dot + 1))) // todo: support integers after '.'
            case idx => fields(idx, DotAST(pos, expr, pos, LiteralAST(v.substring(dot + 1, idx)))) // todo: as above
          }
      }

    v.indexOf('.') match {
      case -1 => VariableAST(v)
//      case 0 => problem( pos, "illegal variable reference" )
      case dot => fields(dot, VariableAST(v.substring(0, dot)))
    }
  }

  def parseVariableArgument(r: CharReader): (CharReader, String) = {
    val res @ (_, s) = parseString(r)

    if (s.isEmpty || !nameFirst(s.head) || !s.tail.forall(nameRest))
      problem(r, "illegal variable name")

    check(r, s)
    res
  }

  def parseExpressionArgument(r: CharReader): (CharReader, AST) = {
    val r0 = skipSpace(r)

    if (r0.eoi)
      problem(r0, "expected command argument")

    parseControlSequence(r0) match {
      case None =>
        matches(r0, beginDelim) match {
          case Some(r1) => parseGroup(r1)
          case None =>
            if (keyword(r0, keywords).isEmpty && nameFirst(r0.ch)) {
              val (r1, s) = parseVariableArgument(r0)

              (r1, dotExpression(r0, s))
            } else {
              val (r1, s) = parseLiteralArgument(r0)

              (r1, LiteralAST(s))
            }
        }
      case Some((r1, name)) => parseCommand(r0, name, r1, false)
    }
  }

  def parseRegularArguments(
      r: CharReader,
      n: Int,
      buf: ListBuffer[AST] = new ListBuffer[AST],
  ): (CharReader, List[AST]) = {
    if (n == 0) (r, buf toList)
    else {
      val (r1, s) = parseRegularArgument(r)

      buf += s
      parseRegularArguments(r1, n - 1, buf)
    }
  }

  def parseExpressionArguments(
      r: CharReader,
      n: Int,
      buf: ListBuffer[AST] = new ListBuffer[AST],
  ): (CharReader, List[AST]) = {
    if (n == 0) (r, buf toList)
    else {
      val (r1, s) = parseExpressionArgument(r)

      buf += s
      parseExpressionArguments(r1, n - 1, buf)
    }
  }

  def skipSpace(r: CharReader): CharReader = skip(r, !_.ch.isWhitespace)

  def skip(r: CharReader, cond: CharReader => Boolean): CharReader = if (r.eoi || cond(r)) r else skip(r.next, cond)

  private def check(pos: CharReader, name: String) =
    if (
      Set(
        "#",
        "delim",
        "def",
        "{",
        ".",
        "elsif",
        "case",
        "in",
        "if",
        "for",
        "unless",
        "match",
        "set",
        "in",
        "and",
        "or",
        "not",
        "seq",
        "raw",
        " ",
        "break",
        "continue",
        "true",
        "false",
        "null",
      ) contains name
    )
      problem(pos, "illegal variable name, it's a reserved word")
    else if (commandMap contains name)
      problem(pos, "illegal variable name, it's a command")

  def parseCommand(pos: CharReader, name: String, r: CharReader, statement: Boolean): (CharReader, AST) = {
    val res @ (rr, ast) =
      if (name == rawBeginDelim) {
        var first = true
        var prev = ' '

        def cond(cr: CharReader) = {
          val res =
            if (cr eoi)
              problem(r, "unclosed raw text")
            else
              !lookahead(cr, csDelim + rawEndDelim)

          first = false
          prev = cr.ch
          res
        }

        val (r1, s) = consumeCond(r, cond)

        (r1, LiteralAST(s))
      } else
        name match {
          case "seq" =>
            val (r1, vec) = parseList(r, true)

            (r1, SeqAST(vec))
          case "{" =>
            val (r1, vec) = parseList(r, false)

            if (vec.length % 2 == 1)
              problem(r1, s"expected an even number of expressions: ${vec.length}")

            (r1, ObjectAST(vec))
          case "." =>
            val (r1, ast) = parseExpressionArgument(r)
            val r2 = skipSpace(r1)
            val (r3, a) = parseRegularArgument(r1)

            (r3, DotAST(r, ast, r2, a))
          case "set" =>
            val (r1, v) = parseVariableArgument(r)
            val (r2, ast) = parseRegularArgument(r1)

            (r2, SetAST(v, ast))
          case "in" =>
            val (r1, v) = parseVariableArgument(r)
            val r2 = skipSpace(r1)
            val (r3, ast) = parseExpressionArgument(r2)

            (r3, InAST(pos, v, r2, ast))
          case "not" =>
            val (r1, expr) = parseExpressionArgument(r)

            (r1, NotAST(expr))
          case "and" =>
            val (r1, args) = parseExpressionArguments(r, 2)

            (r1, AndAST(args.head, args.tail.head))
          case "or" =>
            val (r1, args) = parseExpressionArguments(r, 2)

            (r1, OrAST(args.head, args.tail.head))
          case "if" =>
            val (r1, expr) = parseExpressionArgument(r)
            val (r2, body) = parseRegularArgument(r1)
            val (r3, elsifs) = parseCases("elsif", r2)
            val conds = (expr, body) +: elsifs

            parseElse(r3) match {
              case Some((r4, els)) => (r4, IfAST(conds, Some(els)))
              case _               => (r3, IfAST(conds, None))
            }
          case "unless" =>
            val (r1, expr) = parseExpressionArgument(r)
            val (r2, body) = parseRegularArgument(r1)

            parseElse(r2) match {
              case Some((r3, els)) => (r3, UnlessAST(expr, body, Some(els)))
              case _               => (r2, UnlessAST(expr, body, None))
            }
          case "match" =>
            val (r1, expr) = parseExpressionArgument(r)
            val (r2, cases) = parseCases("case", r1)

            parseElse(r2) match {
              case Some((r3, els)) => (r3, MatchAST(expr, cases, Some(els)))
              case _               => (r2, MatchAST(expr, cases, None))
            }
          case "for" =>
            val r0 = skipSpace(r)
            val (r1, expr) = parseExpressionArgument(r0)
            val (r2, body) = parseRegularArgument(r1)

            parseElse(r2) match {
              case Some((r3, els)) => (r3, ForAST(r0, expr, body, Some(els)))
              case _               => (r2, ForAST(r0, expr, body, None))
            }
          case "break"    => (r, BreakAST(pos))
          case "continue" => (r, ContinueAST(pos))
          case _ =>
            macros get name match {
              case None =>
                commandMap get name match {
                  case None => (r, dotExpression(pos, name))
                  case Some(c) =>
                    val (r1, args, optional) = parseCommandArguments(r, c.arity)

                    (r1, CommandAST(pos, c, args, optional))
                }
              case Some(mac @ Macro(parameters, body)) =>
                if (parameters isEmpty) // todo: recursize parameterless macros could occur
                  (r, body)
                else {
                  val (r1, args) = parseRegularArguments(r, parameters.length)

                  (r1, MacroAST(mac, args))
                }
            }
        }

    @tailrec
    def filters(r: CharReader, ast: AST): (CharReader, AST) =
      matches(skipSpace(r), pipeDelim) match {
        case None => (r, ast)
        case Some(r1) =>
          val r2 = skipSpace(r1)
          val (r3, name) =
            parseFilter(r2) match {
              case None =>
                parseControlSequenceName(r2) match {
                  case None     => problem(r2, "expected a command or macro")
                  case Some(cs) => cs
                }
              case Some(cs) => cs
            }

          macros get name match {
            case None =>
              commandMap get name match {
                case None                    => problem(r2, "expected a command or macro")
                case Some(c) if c.arity == 0 => problem(r2, "expected a command with parameters")
                case Some(c) =>
                  val (r4, args, optional) = parseCommandArguments(r3, c.arity - 1)

                  filters(r4, CommandAST(r2, c, args :+ ast, optional))
              }
            case Some(Macro(parameters, _)) if parameters isEmpty => problem(r2, "expected a macro with parameters")
            case Some(mac @ Macro(parameters, _)) =>
              val (r4, args) = parseRegularArguments(r3, parameters.length - 1)

              filters(r4, MacroAST(mac, args :+ ast))
          }
      }

    if (statement)
      filters(rr, ast)
    else
      res
  }

  private def parseCommandArguments(r: CharReader, n: Int): (CharReader, List[AST], Map[String, AST]) = {
    @tailrec
    def parseOptional(r: CharReader, optional: Map[String, AST]): (CharReader, Map[String, AST]) =
      parseName(skipSpace(r)) match {
        case None => (r, optional)
        case Some((r1a, name)) =>
          matches(r1a, ":") match {
            case None => (r, optional)
            case Some(r2) =>
              val (r3, ast) = parseRegularArgument(r2)

              parseOptional(r3, optional ++ Map(name -> ast))
          }
      }

    val (r1, optional) = parseOptional(r, Map())
    val (r2, args) = parseRegularArguments(r1, n)

    (r2, args, optional)
  }

  def parseCases(cs: String, r: CharReader, cases: Vector[(AST, AST)] = Vector()): (CharReader, Vector[(AST, AST)]) =
    parseControlSequence(skipSpace(r)) match {
      case Some((r1, `cs`)) =>
        val (r2, expr) = parseExpressionArgument(r1)
        val (r3, yes) = parseRegularArgument(r2)

        parseCases(cs, r3, cases :+ (expr, yes))
      case _ => (r, cases)
    }

  def parseElse(r: CharReader): Option[(CharReader, AST)] =
    parseControlSequence(skipSpace(r)) match {
      case Some((r1, "else")) => Some(parseRegularArgument(r1))
      case _                  => None
    }

}
