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

  // The 0.27.0 release shipped a binary whose `--version` said 0.26.0: the version was a literal in
  // Main.scala, and nothing in the release flow reads the banner. It now comes from
  // `ThisBuild / version` through the generated `BuildVersion` — put a literal back in either place
  // and this fails at the next bump.
  "the version the CLI reports is the version the build was cut at" in {
    Version shouldBe BuildVersion
    Version should fullyMatch regex """\d+\.\d+\.\d+([-+].*)?"""
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
