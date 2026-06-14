package io.github.edadma.texish

import scala.annotation.tailrec

//import pprint.pprintln

object Ligatures:
  @tailrec
  def replace(s: String, replacements: List[(String, String)], allowed: Set[String]): String =
    replacements match
      case Nil                                                => s
      case (target, replacement) :: t if allowed(replacement) => replace(s.replace(target, replacement), t, allowed)
      case _ :: t                                             => replace(s, t, allowed)

  def apply(s: String, allowed: Set[String]): String =
    if EXCEPTIONS.exists(e => s.endsWith(e)) then s
    else replace(s, LIGATURES, allowed)

  private val LIGATURES = List(
    "ffi" -> `LATIN SMALL LIGATURE FFI`,
    "ffl" -> `LATIN SMALL LIGATURE FFL`,
    "ff"  -> `LATIN SMALL LIGATURE FF`,
    "fi"  -> `LATIN SMALL LIGATURE FI`,
    "fl"  -> `LATIN SMALL LIGATURE FL`,
  )

  private[texish] val REPRESENTATIONS = List(
    "``"   -> `LEFT DOUBLE QUOTATION MARK`,
    "`"    -> `LEFT SINGLE QUOTATION MARK`,
    "''"   -> `RIGHT DOUBLE QUOTATION MARK`,
    "'"    -> `RIGHT SINGLE QUOTATION MARK`,
    "---"  -> `EM DASH`,
    "<-->" -> `LONG LEFT RIGHT ARROW`,
    "<==>" -> `LONG LEFT RIGHT DOUBLE ARROW`,
    "<--"  -> `LONG LEFTWARDS ARROW`,
    "-->"  -> `LONG RIGHTWARDS ARROW`,
    "<=="  -> `LONG LEFTWARDS DOUBLE ARROW`,
    "==>"  -> `LONG RIGHTWARDS DOUBLE ARROW`,
    "<->"  -> `LEFT RIGHT ARROW`,
    "<=>"  -> `LEFT RIGHT DOUBLE ARROW`,
    "<-"   -> `LEFTWARDS ARROW`,
    "->"   -> `RIGHTWARDS ARROW`,
    "<="   -> `LEFTWARDS DOUBLE ARROW`,
    "=>"   -> `RIGHTWARDS DOUBLE ARROW`,
    "--"   -> `EN DASH`,
    "..."  -> `HORIZONTAL ELLIPSIS`,
  )

  val ALL: Set[String] = (Map() ++ LIGATURES ++ REPRESENTATIONS).values.toSet

  private val EXCEPTIONS = List(
    "fful",
    "fing",
    "fish",
    "fier",
    "fily",
    "finess",
    "fless",
    "fly",
    "flike",
    "flife",
    "fline",
    "flet",
    "pdflatex",
    "ffing",
    "ffish",
    "ffier",
    "ffily",
    "ffiness",
    "ffies",
    "ffian",
    "ffly",
    "ffless",
    "scofflaw",
    "cufflink",
    "offline",
    "offload",
    "fflike",
    "chaffinch",
    "wolffish",
    "safflower",
    "fteen",
    "fth",
    "ftie",
    "fty",
    "halftime",
    "halftone",
    "rooftop",
    "rooftree",
    "offtrack",
  )
