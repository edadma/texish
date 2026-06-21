package io.github.edadma.texish

/** A discretionary break — the author's hook into the line breaker, modelled on TeX's `\discretionary`.
  *
  * It carries three pieces of horizontal material. When the breaker takes a break here, `pre` is appended to
  * the end of the line and `post` opens the next line (the usual case has `pre` a hyphen and `post` empty —
  * that is what `\-` produces). When no break is taken, `noBreak` shows instead, in line with the surrounding
  * text. All three are commonly empty or a single glyph: a zero-width break after a slash for URLs is
  * `\discretionary{}{}{}`, a soft hyphen is `\discretionary{-}{}{}`, and a compound that respells across the
  * break (German `ck` → `k-k`) uses all three.
  *
  * The box itself measures as its `noBreak` form, so a discretionary that survives into a context with no line
  * breaking (a bare `\hbox`) simply renders the unbroken material. Inside a paragraph the breaker resolves it
  * into real boxes — see [[KnuthPlass]] — and this box is never drawn. */
class DiscretionaryBox(val pre: Seq[Box], val post: Seq[Box], val noBreak: Seq[Box]) extends Box:
  val width: Double    = noBreak.map(_.width).sum
  val xAdvance: Double = noBreak.map(_.xAdvance).sum
  val ascent: Double   = if noBreak.isEmpty then 0 else noBreak.map(_.ascent).max
  val descent: Double  = if noBreak.isEmpty then 0 else noBreak.map(_.descent).max
  def isSpace: Boolean = false

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    var currentX = x
    for b <- noBreak do
      b.draw(t, currentX, y)
      currentX += b.width

  override def toString: String = s"DiscretionaryBox(pre=$pre, post=$post, noBreak=$noBreak)"
