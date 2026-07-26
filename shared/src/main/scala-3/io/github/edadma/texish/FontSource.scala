package io.github.edadma.texish

import io.github.edadma.path.Path

/** A place the engine looks for a font file it knows only by its bundled relative path
  * (`fonts/LatinModernRoman/lmroman10-regular.otf`). Sources are how a host says where its fonts live without
  * the engine having to know anything about installation layouts: a CLI registers the tree its package shipped,
  * an application registers a folder it manages, a test registers nothing at all.
  *
  * A source answers in whichever of the two currencies it actually holds. [[file]] is for a source backed by the
  * filesystem, and is preferred where both are available: a backend opens a face from a path more cheaply than
  * from bytes, since it can read the file lazily instead of being handed the whole thing in memory. [[bytes]] is
  * for a source with no filesystem behind it — the core compiled into the artifact, a font fetched over the
  * network, a face a host holds as a resource.
  */
trait FontSource:

  /** The path on disk this source has for a bundled path, if it has one. */
  def file(path: String): Option[String] = None

  /** The bytes this source has for a bundled path, if it has them and cannot offer a file. */
  def bytes(path: String): Option[Array[Byte]] = None

/** A font tree on disk: a directory under which bundled relative paths resolve as written, so `root/fonts/…`.
  * The probing is guarded because a filesystem-less host reaches for platform APIs that may be absent; there,
  * nothing resolves and the engine falls through to the next source, which is exactly right.
  */
class DirectoryFontSource(val root: String) extends FontSource:

  override def file(path: String): Option[String] =
    try
      val p = Path(root) / path

      Option.when(p.exists)(p.toPlatformString)
    catch case _: Throwable => None

  override def toString: String = s"DirectoryFontSource($root)"

/** The core faces compiled into the artifact. Always consulted last, so a font tree that carries the same path
  * shadows it — a host may ship newer cuts than the ones the artifact was built with — and always consulted, so
  * that a program embedding texish as a library has a working engine having configured nothing at all.
  */
object EmbeddedFontSource extends FontSource:

  override def bytes(path: String): Option[Array[Byte]] = EmbeddedFonts.get(path)

  override def toString: String = "EmbeddedFontSource"
