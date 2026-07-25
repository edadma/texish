package io.github.edadma.texish

import io.github.edadma.char_reader.CharReader

/** An error texish raises on purpose — the document, or the way the engine was driven, is at fault. A font the
  * document never loaded, a variable it never set, two superscripts on one atom: mistakes in the input, which an
  * embedder catches to report to whoever wrote it.
  *
  * The distinction that earns this type is with everything else. An exception that is *not* a `TexishException`
  * and still escapes the engine is a defect in texish, and the processor says so: it reports such a failure as
  * an internal error and keeps the original as `cause`, so the stack trace that locates the bug is not thrown
  * away behind a formatted message. Without the distinction the two are indistinguishable, and a null
  * dereference deep in layout arrives looking exactly like a misspelled font name — sending the author hunting
  * through their source for a mistake that is not there.
  *
  * `pos` is the source position, once one is known. The engine raises this with no position, having no notion
  * of where in a document it is; the processor catches it and re-raises it against the token it was handling,
  * which is what puts a line and column on an error thrown from deep inside the typesetter.
  */
class TexishException(message: String, val pos: CharReader = null, cause: Throwable = null)
    extends RuntimeException(message, cause):

  /** The message as raised, before the processor formatted a source excerpt around it. */
  def msg: String = message

object TexishException:
  def apply(message: String, pos: CharReader = null, cause: Throwable = null): TexishException =
    new TexishException(message, pos, cause)
