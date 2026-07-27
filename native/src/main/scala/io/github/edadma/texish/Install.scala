package io.github.edadma.texish

import java.io.File

import scala.scalanative.meta.LinktimeInfo
import scala.scalanative.posix.stdlib.realpath
import scala.scalanative.posix.unistd.readlink
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** Finding the `fonts/` and `packages/` an installation shipped, by finding the executable that is asking.
  *
  * The engine deliberately knows nothing about installation layouts — it looks under [[Typesetter.home]] and in
  * the font sources it is given. Somebody has to tell it where those are, and for a packaged program the only
  * reliable starting point is its own location: a package manager decides where that is, the binary may be
  * invoked through a symlink, from `$PATH`, or by a relative path, and there is no environment variable in play
  * unless somebody remembered to set one. Locating ourselves is what makes a package work with neither a wrapper
  * script nor `$TEXISHHOME`.
  *
  * This lives in the library rather than in the command-line tool because the tool is not the only program that
  * needs it. An application embedding texish and installed alongside it — a preview app whose package depends on
  * texish's — has exactly the same question to answer, and would otherwise have to guess.
  */
object Install:

  /** Long enough for any path either platform will hand back — macOS caps at 1024, Linux at 4096. */
  private final val PathMax = 4096

  @extern
  private object dyld:
    /** macOS: the path the running image was loaded from. Fills `buf` and returns 0, or returns -1 having set
      * `size` to the buffer it wanted. The result may still contain symlinks or `..`. */
    def _NSGetExecutablePath(buf: CString, size: Ptr[CUnsignedInt]): CInt = extern

  /** The path of the running executable as it was reached, and again with symlinks resolved — in that order, and
    * without duplicates. Both matter, and for different installations:
    *
    *   - **As reached.** A package manager links a program into a shared prefix (`/opt/homebrew/bin/texish`) and
    *     links each package's data alongside (`/opt/homebrew/share/texish/`). Searching from the unresolved path
    *     finds that shared prefix — which is the only way a program finds files belonging to a *different*
    *     package it depends on, since those are nowhere inside its own.
    *   - **Resolved.** The link points into a versioned directory holding both the binary and its own data
    *     (`…/Cellar/texish/<version>/{bin,share}`). Searching from there finds a program's own files without
    *     depending on the prefix's links being intact.
    *
    * Empty on a platform with no way to ask, in which case nothing is located and `$TEXISHHOME` is all there is.
    */
  def executablePaths(): Seq[String] = Zone {
    val buf = alloc[Byte](PathMax)

    def resolved(raw: String): Option[String] =
      val out = alloc[Byte](PathMax)

      // A path that cannot be resolved (a deleted file, a directory the user cannot read) still leaves the
      // unresolved one worth searching, so this contributes nothing rather than failing.
      Zone(if realpath(toCString(raw), out) != null then Some(fromCString(out)) else None)

    val reached =
      if LinktimeInfo.isMac then
        val size = alloc[CUnsignedInt]()

        !size = PathMax.toUInt

        if dyld._NSGetExecutablePath(buf, size) == 0 then Some(fromCString(buf)) else None
      else if LinktimeInfo.isLinux then
        // /proc/self/exe is already symlink-resolved by the kernel, so both entries coincide there.
        val n = readlink(c"/proc/self/exe", buf, PathMax.toUSize)

        if n > 0 then
          buf(n) = 0.toByte
          Some(fromCString(buf))
        else None
      else None

    reached.toSeq.flatMap(path => Seq(path) ++ resolved(path)).distinct
  }

  /** The texish home found by walking up from an executable: a directory holding a `fonts/` or a `packages/`
    * folder, which is what [[Typesetter.home]] wants. Either folder is enough — an installation may ship the
    * packages and leave the 151MB font tree out, and the engine copes with whichever half it is given.
    *
    * At each level two layouts are tried: `share/texish/`, where a Unix package installs the data belonging to a
    * program, and the directory itself, which is what an unpacked archive or a build tree looks like. Four levels
    * up covers `<prefix>/bin/texish` and a versioned directory one deeper without wandering off into the user's
    * home directory.
    */
  def homeNear(executable: String): Option[String] =
    val start = new File(executable).getAbsoluteFile.getParentFile

    def hasTree(dir: File): Boolean = new File(dir, "fonts").isDirectory || new File(dir, "packages").isDirectory

    Iterator
      .iterate(start)(_.getParentFile)
      .takeWhile(_ != null)
      .take(4)
      .flatMap(dir => Seq(new File(dir, "share/texish"), dir))
      .find(hasTree)
      .map(_.getPath)

  /** The texish home for this installation, or None when the program is not installed beside one. */
  def locateHome(): Option[String] = executablePaths().iterator.flatMap(homeNear).nextOption()

  /** Point the engine at the tree this installation shipped, if it shipped one. Silent when there is none: the
    * core faces and the `base` and `document` packages are compiled in, so a program with no tree beside it still
    * sets an ordinary document — it simply has the guaranteed families and formats and not the rest.
    *
    * Call before constructing any typesetter, since [[Typesetter.home]] is read when one is built. Leaves an
    * already-set home alone, so a host that has made its own arrangements keeps them.
    */
  def configure(): Unit =
    if Typesetter.home.isEmpty then locateHome().foreach(Typesetter.home = _)
