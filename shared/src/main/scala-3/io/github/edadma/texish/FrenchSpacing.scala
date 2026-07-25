package io.github.edadma.texish

import scala.collection.mutable.ArrayBuffer

/** French spacing of high punctuation.
  *
  * French sets a space before the marks English sets tight — the colon, semicolon, exclamation and question
  * marks — and inside its quotation marks, the guillemets. The space is not an ordinary interword space. It
  * must not stretch when the line is justified, or the mark drifts away from the word it belongs to, and it
  * must not break, or the mark is carried alone to the head of the next line. Both faults are visible in a
  * narrow measure, and both are what an ordinary space gives.
  *
  * The tradition distinguishes two widths. The colon takes a full space, unbreakable; the semicolon, the
  * exclamation and question marks, and the guillemets take a *fine* space, half that width. Both are taken
  * from the interword space of the type the text is set in, so they follow the font rather than a fixed
  * measurement.
  *
  * The rule runs over a finished paragraph, where the words are boxes and the spaces between them are glue.
  * A space already typed before one of these marks is replaced with the right one; where the author typed
  * none, one is inserted, so `« Oui ! »` and `«Oui!»` set alike. Text in any other language is untouched —
  * this runs only when the document declares itself French (see `Typesetter.language`).
  */
object FrenchSpacing:

  // The marks a space is set before, with the width of that space as a fraction of the interword space.
  private val fine = 0.5
  private def spaceBefore(c: Char): Double =
    c match
      case ':'                   => 1.0
      case ';' | '!' | '?' | '»' => fine
      case _                     => 0.0

  private def spaceAfter(c: Char): Boolean = c == '«'

  /** Whether `language` is French, and so whether a paragraph in it is spaced this way. Accepts a plain `fr`
    * and any regional tag built on it (`fr-CA`, `fr_FR`). */
  def applies(language: Option[String]): Boolean =
    language.exists(l =>
      val t = l.toLowerCase
      t == "fr" || t.startsWith("fr-") || t.startsWith("fr_"),
    )

  /** Space the high punctuation of one finished paragraph, in place. */
  def apply(boxes: ArrayBuffer[Box]): Unit =
    split(boxes)
    respace(boxes)

  // A mark the author typed tight against its word is separated out, so the space can then be put in. The
  // box is cut just before the mark — `cela?` becomes `cela` and `?` — and likewise just after an opening
  // guillemet. A mark standing alone in its box is already separate and is left as it is.
  private def split(boxes: ArrayBuffer[Box]): Unit =
    var i = 0
    while i < boxes.length do
      boxes(i) match
        case c: CharBox if c.text.length > 1 =>
          val at = cut(c.text)
          if at > 0 then
            boxes(i) = c.newCharBox(c.text.substring(0, at))
            boxes.insert(i + 1, c.newCharBox(c.text.substring(at)))
        case _ =>
      i += 1

  // Where a box's text should be cut, or 0 for no cut: before the first mark that takes a space in front of
  // it, or after an opening guillemet. Only the first cut is made here; the box that follows is examined on
  // the next pass of the loop, so `Oui!»` comes apart in stages.
  private def cut(s: String): Int =
    var i = 1
    while i < s.length do
      if takesSpace(s(i), s(i - 1)) then return i
      if spaceAfter(s(i - 1)) then return i
      i += 1
    0

  // Whether `c`, coming after `prev`, is set off by a space. A run of marks is set as one — `Quoi ?!` takes
  // its space before the pair, not inside it — but a closing guillemet always takes its own, so `Oui ! »`
  // keeps both.
  private def takesSpace(c: Char, prev: Char): Boolean =
    spaceBefore(c) > 0 && (c == '»' || !clusters(prev))

  // The marks that group together when several are written in a row.
  private def clusters(c: Char): Boolean = ";:!?".contains(c)

  // Put the right space beside each mark: replace the glue where the author typed a space, and insert glue
  // where none was typed. The width comes from the interword glue already there when there is some, and from
  // the neighbouring box's font when there is not, so either way it follows the type.
  private def respace(boxes: ArrayBuffer[Box]): Unit =
    var i = 0
    while i < boxes.length do
      boxes(i) match
        case c: CharBox if c.text.nonEmpty =>
          val want = spaceBefore(c.text.head)
          if want > 0 && i > 0 then
            boxes(i - 1) match
              case g: Glue => boxes(i - 1) = frenchGlue(want, g.naturalSize)
              case p: CharBox if p.text.nonEmpty && takesSpace(c.text.head, p.text.last) =>
                boxes.insert(i, frenchGlue(want, p.font.space))
                i += 1
              case _ =>
          if c.text.last == '«' && i + 1 < boxes.length then
            boxes(i + 1) match
              case g: Glue    => boxes(i + 1) = frenchGlue(fine, g.naturalSize)
              case _: CharBox => boxes.insert(i + 1, frenchGlue(fine, c.font.space))
              case _          =>
        case _ =>
      i += 1

  // Fixed, unbreakable glue of `fraction` of an interword space: it neither stretches under justification nor
  // offers the line breaker a place to break, which is what keeps the mark against its word.
  private def frenchGlue(fraction: Double, interword: Double): Glue =
    Glue(interword * fraction, 0, 0, 0, 0, nobreak = true)
