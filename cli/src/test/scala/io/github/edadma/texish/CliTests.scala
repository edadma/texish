package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.Files

/** The command-line tool turns a texish source document into PDF or PNG files on disk. These tests drive the
  * same rendering helpers the `texish` executable uses and check that real, non-trivial output files appear.
  */
class CliTests extends AnyFreeSpec with Matchers:

  private val source =
    "A short document with inline math $x^2 + y^2 = z^2$ and a fraction $\\frac{a+b}{c}$.\n\n"

  "renderPdf writes a non-empty PDF with a %PDF header" in {
    val out = File.createTempFile("texish-cli", ".pdf")

    try
      renderPdf(source, out.getAbsolutePath, "letter")

      out.length should be > 0L

      val head = new String(Files.readAllBytes(out.toPath).take(5))
      head shouldBe "%PDF-"
    finally out.delete()
  }

  "renderPng writes one PNG per page with a PNG signature" in {
    val dir  = Files.createTempDirectory("texish-cli").toFile
    val base = new File(dir, "page").getAbsolutePath

    try
      // two pages, so the helper numbers the files page_1.png / page_2.png
      renderPng("first\n\n\\vfill\\eject second\n\n", base, "letter", "sd")

      val one = new File(dir, "page_1.png")
      val two = new File(dir, "page_2.png")

      one.exists shouldBe true
      two.exists shouldBe true

      val sig = Files.readAllBytes(one.toPath).take(4).map(_ & 0xff)
      sig shouldBe Array(0x89, 0x50, 0x4e, 0x47) // \x89 P N G
    finally
      dir.listFiles.foreach(_.delete())
      dir.delete()
  }

  "a numeric resolution sets the DPI, so at 72 one point renders as one pixel" in {
    val dir  = Files.createTempDirectory("texish-cli").toFile
    val base = new File(dir, "px").getAbsolutePath

    try
      // a letter page is 612x792 points; at 72 DPI the PNG must be exactly 612x792 pixels
      renderPng("hello\n\n", base, "letter", "72")

      val png   = new File(dir, "px.png")
      val bytes = Files.readAllBytes(png.toPath)

      // PNG IHDR: width is the 4 big-endian bytes at offset 16, height at offset 20
      def be32(off: Int) = (0 until 4).foldLeft(0)((acc, i) => (acc << 8) | (bytes(off + i) & 0xff))

      be32(16) shouldBe 612
      be32(20) shouldBe 792
    finally
      dir.listFiles.foreach(_.delete())
      dir.delete()
  }

  "a single-page PNG keeps the base name with a .png extension" in {
    val dir  = Files.createTempDirectory("texish-cli").toFile
    val base = new File(dir, "solo").getAbsolutePath

    try
      renderPng(source, base, "letter", "sd")

      new File(dir, "solo.png").exists shouldBe true
    finally
      dir.listFiles.foreach(_.delete())
      dir.delete()
  }

  "ensureExtension only appends when the extension is missing" in {
    ensureExtension("doc", "pdf") shouldBe "doc.pdf"
    ensureExtension("doc.pdf", "pdf") shouldBe "doc.pdf"
    ensureExtension("doc.PDF", "pdf") shouldBe "doc.PDF" // case-insensitive match, original kept
  }

  "stripExtension removes a trailing extension but leaves bare names alone" in {
    stripExtension("doc.pdf") shouldBe "doc"
    stripExtension("doc") shouldBe "doc"
    stripExtension(".hidden") shouldBe ".hidden" // leading dot is not an extension separator
  }

  "the default output base lands beside the input file, not in the current directory" in {
    // no -o: keep the input's directory, drop the extension — so the output sits next to the source
    defaultOutputBase(Some("scripts/picture.script"), None) shouldBe "scripts/picture"
    defaultOutputBase(Some("/tmp/sub/doc.texish"), None) shouldBe "/tmp/sub/doc"
    defaultOutputBase(Some("bare.texish"), None) shouldBe "bare" // no directory component
    defaultOutputBase(None, None) shouldBe "out"                 // reading stdin
    defaultOutputBase(Some("scripts/picture.script"), Some("/x/y")) shouldBe "/x/y" // explicit -o wins
  }

  // --- locating the installation's font tree ---------------------------------

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

  "a package layout is found from the binary, without an environment variable" in {
    // what `brew install` leaves behind: the program in bin/, its data in share/<name>/
    val root = layout("bin/texish", "share/texish/fonts/LatinModernRoman/lmroman10-regular.otf")

    try Install.fontsDirNear(new File(root, "bin/texish").getPath) shouldBe
      Some(new File(root, "share/texish").getPath)
    finally remove(root)
  }

  "an unpacked archive with the fonts beside the binary is found too" in {
    val root = layout("texish", "fonts/LatinModernRoman/lmroman10-regular.otf")

    try Install.fontsDirNear(new File(root, "texish").getPath) shouldBe Some(root.getPath)
    finally remove(root)
  }

  // Within one directory the package layout is the one to prefer: share/ is where the data belonging to a
  // program is supposed to live, and a bare fonts/ at a prefix is more likely to be something else's.
  "the package layout wins over a bare fonts/ at the same level" in {
    val root = layout("bin/texish", "fonts/x.otf", "share/texish/fonts/x.otf")

    try Install.fontsDirNear(new File(root, "bin/texish").getPath) shouldBe
      Some(new File(root, "share/texish").getPath)
    finally remove(root)
  }

  // Across levels the nearer tree wins, so a build tree or an unpacked archive sitting inside some larger
  // installation is not overruled by whatever that installation happens to carry.
  "a tree nearer the binary wins over one further up" in {
    val root = layout("share/texish/fonts/x.otf", "nested/bin/texish", "nested/fonts/x.otf")

    try Install.fontsDirNear(new File(root, "nested/bin/texish").getPath) shouldBe
      Some(new File(root, "nested").getPath)
    finally remove(root)
  }

  "an installation with no font tree offers nothing, rather than a directory with no fonts in it" in {
    val root = layout("bin/texish", "share/texish/packages/document.texish")

    try Install.fontsDirNear(new File(root, "bin/texish").getPath) shouldBe None
    finally remove(root)
  }

  // The search stops before it can wander out of the installation and into whatever the user keeps further up.
  "the walk up is bounded" in {
    val root = layout("a/b/c/d/e/bin/texish", "fonts/x.otf")

    try Install.fontsDirNear(new File(root, "a/b/c/d/e/bin/texish").getPath) shouldBe None
    finally remove(root)
  }

  "the running executable can locate itself" in {
    // The test binary, not an installed texish — all this asserts is that the platform call works and gives back
    // an absolute path to something that exists. Which layout it sits in is the business of the tests above.
    val exe = Install.executablePath()

    withClue(s"executablePath() = $exe: ") {
      exe should not be empty
      new File(exe.get).isAbsolute shouldBe true
      new File(exe.get).exists shouldBe true
    }
  }
