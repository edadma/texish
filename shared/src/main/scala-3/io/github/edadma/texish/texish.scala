package io.github.edadma

import io.github.edadma.char_reader.CharReader

import java.io.File

package object texish {

  private val numberRegex = """-?\d+(\.\d+)?|0x[0-9a-fA-F]+""".r

  case object UNDEFINED { override def toString = "undefined" }

//  def problem( r: Reader, error: String ): Nothing = problem( r.pos, error )

  def problem(pos: CharReader, error: String): Nothing =
    if (pos eq null)
      sys.error(error)
    else
      sys.error(s"${pos.line}, ${pos.col}: $error" + "\n" + pos.lineText)

  case object nil { override def toString = "" }

  def docroot(name: String, settings: Map[Symbol, Any]) = new File(settings(Symbol("docroot")).toString, name)

  def isNumber(a: String): Boolean = numberRegex.pattern.matcher(a).matches

  def number(a: Any): Option[BigDecimal] =
    a match {
      case s: String if isNumber(s) =>
        if (s.startsWith("0x"))
          Some(BigDecimal(Integer.parseInt(s.substring(2), 16)))
        else
          Some(BigDecimal(s))
      case n: BigDecimal => Some(n)
      case _             => None
    }

  def round(n: BigDecimal, scale: Int, config: Map[String, Any]): BigDecimal =
    n.setScale(scale, BigDecimal.RoundingMode.withName(config("rounding").toString))

  def truthy(a: Any): Boolean = !falsy(a)

  def falsy(a: Any): Boolean = a == false || a == nil || a == UNDEFINED || a == null

  def display(a: Any): String =
    a match {
      case m: collection.Map[_, _] =>
        m map { case (k, v) => qdisplay(k) + ": " + qdisplay(v) } mkString ("{", ", ", "}")
      case l: collection.Seq[_] => l map qdisplay mkString ("[", ", ", "]")
      case ()                   => ""
      case s                    => String.valueOf(s)
    }

  private def qdisplay(a: Any): String =
    a match {
      case s: String => s""""$s""""
      case true      => "<true>"
      case false     => "<false>"
      case null      => "<null>"
      case `nil`     => "<nil>"
      case '\n'      => """"\n""""
      case ' '       => """" """"
      case _         => display(a)
    }

}
