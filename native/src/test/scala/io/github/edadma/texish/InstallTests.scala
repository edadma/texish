package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.Files

/** Locating the `fonts/` and `packages/` an installation shipped, from the running executable's own path. This is
  * what lets a packaged texish — and a packaged program that depends on it — work with neither a wrapper script
  * nor `$TEXISHHOME`.
  */
class InstallTests extends AnyFreeSpec with Matchers:

  /** Build a directory layout under a temporary root and return the root. Each entry is a relative path; one
    * ending in `/` is a directory, anything else an empty file. */
  private def layout(entries: String*): File =
    val root = Files.createTempDirectory("texish-install").toFile

    for entry <- entries do
      val f = new File(root, entry)

      if entry.endsWith("/") then f.mkdirs()
      else
        f.getParentFile.mkdirs()
        f.createNewFile()

    root

  private def remove(f: File): Unit =
    if f.isDirectory then f.listFiles.foreach(remove)
    f.delete()

  private def homeOf(root: File, exe: String): Option[String] =
    Install.homeNear(new File(root, exe).getPath)

  "a package layout is found from the binary, without an environment variable" in {
    // what `brew install` leaves behind: the program in bin/, its data in share/<name>/
    val root = layout("bin/texish", "share/texish/fonts/LatinModernRoman/lmroman10-regular.otf")

    try homeOf(root, "bin/texish") shouldBe Some(new File(root, "share/texish").getPath)
    finally remove(root)
  }

  "an unpacked archive with the tree beside the binary is found too" in {
    val root = layout("texish", "fonts/LatinModernRoman/lmroman10-regular.otf")

    try homeOf(root, "texish") shouldBe Some(root.getPath)
    finally remove(root)
  }

  // Either half of the tree is enough to make a directory the home. An installation may ship the packages and
  // leave the 151MB of fonts out, and before this the packages would have been invisible — the search looked for
  // a fonts/ folder and nothing else, from the days when every package was compiled into the artifact.
  "a packages-only installation is found, not just a fonts-only one" in {
    val root = layout("bin/texish", "share/texish/packages/diagram.texish")

    try homeOf(root, "bin/texish") shouldBe Some(new File(root, "share/texish").getPath)
    finally remove(root)
  }

  // Within one directory the package layout is the one to prefer: share/ is where the data belonging to a
  // program is supposed to live, and a bare fonts/ at a prefix is more likely to be something else's.
  "the package layout wins over a bare fonts/ at the same level" in {
    val root = layout("bin/texish", "fonts/x.otf", "share/texish/fonts/x.otf")

    try homeOf(root, "bin/texish") shouldBe Some(new File(root, "share/texish").getPath)
    finally remove(root)
  }

  // Across levels the nearer tree wins, so a build tree or an unpacked archive sitting inside some larger
  // installation is not overruled by whatever that installation happens to carry.
  "a tree nearer the binary wins over one further up" in {
    val root = layout("share/texish/fonts/x.otf", "nested/bin/texish", "nested/fonts/x.otf")

    try homeOf(root, "nested/bin/texish") shouldBe Some(new File(root, "nested").getPath)
    finally remove(root)
  }

  "an installation with no tree at all offers nothing" in {
    val root = layout("bin/texish", "share/texish/README.md")

    try homeOf(root, "bin/texish") shouldBe None
    finally remove(root)
  }

  // The search stops before it can wander out of the installation and into whatever the user keeps further up.
  "the walk up is bounded" in {
    val root = layout("a/b/c/d/e/bin/texish", "fonts/x.otf")

    try homeOf(root, "a/b/c/d/e/bin/texish") shouldBe None
    finally remove(root)
  }

  // --- which executable path is searched -------------------------------------

  // The case that makes a dependency work. A package manager links each program into a shared prefix and links
  // each package's data alongside it, so a program finds a *different* package's files only through that prefix
  // — they are nowhere inside its own versioned directory. Resolving the symlink first would land in the wrong
  // one and find nothing, which is exactly what happened before both paths were searched.
  "a dependency's tree is found through the prefix the binary was reached by" in {
    val root = layout(
      "bin/",                                            // the shared prefix's bin, holding links
      "Cellar/app/1.0/bin/app",                          // the dependent program, in its own keg
      "share/texish/packages/diagram.texish",            // texish's data, linked into the prefix
      "Cellar/texish/0.24.0/share/texish/packages/diagram.texish",
    )

    try
      val link = new File(root, "bin/app")

      Files.createSymbolicLink(link.toPath, new File(root, "Cellar/app/1.0/bin/app").toPath)

      // as reached: the shared prefix, where the dependency's data is
      Install.homeNear(link.getPath) shouldBe Some(new File(root, "share/texish").getPath)

      // resolved: the program's own keg, which carries no texish data at all
      Install.homeNear(link.getCanonicalPath) shouldBe None
    finally remove(root)
  }

  "the running executable can locate itself" in {
    // The test binary, not an installed texish — all this asserts is that the platform call works and gives back
    // absolute paths to things that exist. Which layout they sit in is the business of the tests above.
    val paths = Install.executablePaths()

    withClue(s"executablePaths() = $paths: ") {
      paths should not be empty
      paths.distinct shouldBe paths // no duplicate work when reached and resolved coincide
      for p <- paths do
        new File(p).isAbsolute shouldBe true
        new File(p).exists shouldBe true
    }
  }

  "configure leaves a home a host has already chosen" in {
    val original = Typesetter.home

    try
      Typesetter.home = "/somewhere/a/host/picked"
      Install.configure()
      Typesetter.home shouldBe "/somewhere/a/host/picked"
    finally Typesetter.home = original
  }
