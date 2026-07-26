package io.github.edadma.texish

import java.io.File

import scala.scalanative.meta.LinktimeInfo
import scala.scalanative.posix.stdlib.realpath
import scala.scalanative.posix.unistd.readlink
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** Finding the font tree an installation shipped, by finding the executable that is asking.
  *
  * The engine deliberately knows nothing about installation layouts — it looks in the font sources it is given
  * (see `FontSource`). Somebody has to give it one, and for a packaged command-line tool the only reliable
  * starting point is the tool's own location: a package manager decides where that is, the user may invoke the
  * binary through a symlink, from `$PATH`, or by a relative path, and there is no environment variable in play
  * unless somebody remembered to set one. Locating ourselves makes a package with no wrapper script work.
  */
private[texish] object Install:

  /** Long enough for any path either platform will hand back — macOS caps at 1024, Linux at 4096. */
  private final val PathMax = 4096

  @extern
  private object dyld:
    /** macOS: the path the running image was loaded from. Fills `buf` and returns 0, or returns -1 having set
      * `size` to the buffer it wanted. The result may still contain symlinks or `..`, so Apple's own guidance is
      * to pass it through `realpath`. */
    def _NSGetExecutablePath(buf: CString, size: Ptr[CUnsignedInt]): CInt = extern

  /** The absolute, symlink-resolved path of the running executable, or None on a platform with no way to ask.
    *
    * Resolving symlinks is what makes this work under a package manager: Homebrew puts `texish` in `bin/` as a
    * link into a versioned keg, and following it lands in the keg — where the package's own `share/` sits, so the
    * font tree is found without depending on the manager's symlink farm being intact.
    */
  def executablePath(): Option[String] = Zone {
    val buf = alloc[Byte](PathMax)

    def resolved(raw: CString): Option[String] =
      val out = alloc[Byte](PathMax)

      // A path that cannot be resolved (a deleted file, a permission the user does not have) is still worth
      // reporting as written — the walk below may well find the tree from it anyway.
      if realpath(raw, out) != null then Some(fromCString(out)) else Some(fromCString(raw))

    if LinktimeInfo.isMac then
      val size = alloc[CUnsignedInt]()

      !size = PathMax.toUInt

      if dyld._NSGetExecutablePath(buf, size) == 0 then resolved(buf) else None
    else if LinktimeInfo.isLinux then
      // Already symlink-resolved by the kernel, but pass it through the same path for one code path to reason
      // about. readlink does not terminate the buffer.
      val n = readlink(c"/proc/self/exe", buf, PathMax.toUSize)

      if n > 0 then
        buf(n) = 0.toByte
        resolved(buf)
      else None
    else None
  }

  /** The directory *containing* a `fonts/` folder, found by walking up from an executable. Bundled font paths
    * begin at `fonts/`, so this is what `Typesetter.fontsDir` wants.
    *
    * At each level two layouts are tried: `share/texish/fonts`, which is where a Unix package installs data
    * belonging to a program, and a plain `fonts/` beside the binary, which is what an unpacked archive or a build
    * tree looks like. Four levels up covers `<prefix>/bin/texish` and a keg one deeper without wandering off into
    * the user's home directory.
    */
  def fontsDirNear(executable: String): Option[String] =
    val start = new File(executable).getAbsoluteFile.getParentFile

    Iterator
      .iterate(start)(_.getParentFile)
      .takeWhile(_ != null)
      .take(4)
      .flatMap(dir => Seq(new File(dir, "share/texish"), dir))
      .find(candidate => new File(candidate, "fonts").isDirectory)
      .map(_.getPath)

  /** Point the engine at the font tree this installation shipped, if it shipped one. Silent when there is none:
    * the core is compiled into the binary, so a texish with no tree beside it still typesets — it simply has the
    * guaranteed families and not the catalogue.
    *
    * Call before constructing any typesetter, since `Typesetter.fontsDir` is read when one is built.
    */
  def offerBundledFonts(): Unit =
    for
      exe <- executablePath()
      dir <- fontsDirNear(exe)
    do Typesetter.fontsDir = dir
