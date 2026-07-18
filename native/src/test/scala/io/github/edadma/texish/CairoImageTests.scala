package io.github.edadma.texish

import io.github.edadma.libcairo.Surface
import io.github.edadma.texish.parser.{Processor, TypesetterHandler, registerTypesettingPrimitives}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.scalanative.unsafe.*

/** The native image-surface backend renders each page onto its own ARGB32 surface, collected in the document's
  * printedPages, sized to the paper dimensions times the device scale.
  */
class CairoImageTests extends AnyFreeSpec with Matchers:

  private def render(dpi: Double, script: String, background: Color = Color("white")): List[Surface] =
    val t       = new CairoImageTypesetter(dpi)
    t.backgroundColor = background
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)

    registerTypesettingPrimitives(proc, handler)
    proc.process(script)
    t.end()
    t.getDocument.printedPages.toList.map(_.asInstanceOf[Surface])

  /** Number of pixels on the page that are not opaque white — i.e. where something was drawn. */
  private def inkPixels(s: Surface): Int =
    s.flush()

    val data   = s.getData
    val stride = s.getStride
    val w      = s.getWidth
    val h      = s.getHeight
    var count  = 0
    var y      = 0

    while y < h do
      var x = 0

      while x < w do
        val p = data + y.toLong * stride + x.toLong * 4

        if !((p(0) & 0xff) == 0xff && (p(1) & 0xff) == 0xff && (p(2) & 0xff) == 0xff && (p(3) & 0xff) == 0xff) then
          count += 1
        x += 1
      y += 1
    count

  private def whiteCorner(s: Surface): Boolean =
    s.flush()

    val p = s.getData

    (p(0) & 0xff) == 0xff && (p(1) & 0xff) == 0xff && (p(2) & 0xff) == 0xff && (p(3) & 0xff) == 0xff

  "a page surface is sized to the paper dimensions times the device scale" in {
    // letter paper is 8.5x11in = 612x792pt; at 100dpi the device scale is 100/72
    val pages = render(100, "Hello world\n\n")

    pages.length shouldBe 1
    pages.head.getWidth shouldBe 850   // 612 * 100/72
    pages.head.getHeight shouldBe 1100 // 792 * 100/72
  }

  "the device scale tracks dpi" in {
    val pages = render(200, "Hello world\n\n")

    pages.head.getWidth shouldBe 1700  // 612 * 200/72
    pages.head.getHeight shouldBe 2200 // 792 * 200/72
  }

  "each page has an opaque white background and drawn text" in {
    val pages = render(100, "Hello world\n\n")

    whiteCorner(pages.head) shouldBe true
    inkPixels(pages.head) should be > 0
  }

  "the page background honours backgroundColor" in {
    // A dark page colour (what a dark-scheme screen preview asks for) fills the page instead of
    // white; the default keeps the white paper that print output expects.
    val dark = render(100, "Hello world\n\n", background = Color("black"))

    val p = dark.head
    p.flush()
    val data = p.getData
    // the top-left corner is bare page: opaque, with every colour channel black
    (data(3) & 0xff) shouldBe 0xff
    (data(0) & 0xff) shouldBe 0x00
    (data(1) & 0xff) shouldBe 0x00
    (data(2) & 0xff) shouldBe 0x00

    whiteCorner(render(100, "Hello world\n\n").head) shouldBe true
  }

  "the font base directory is where bundled fonts are read from" in {
    // Each bundled face is named `fonts/…`; CairoTypesetter.fontsDir is the directory *containing* that
    // `fonts/` folder and is prepended before the file is opened, so an embedding application can keep
    // the fonts anywhere. The default "." finds ./fonts from the source tree (every other test here
    // relies on it); this confirms the directory is actually consulted — the fonts' parent given as an
    // absolute path still resolves, and a directory with no fonts fails loudly rather than drawing tofu.
    val original = CairoTypesetter.fontsDir
    try
      CairoTypesetter.fontsDir = new java.io.File("fonts").getAbsoluteFile.getParent
      val t = new CairoImageTypesetter(100)
      t.destroy() // constructed and tore down without a font-load error

      CairoTypesetter.fontsDir = "/no/such/fonts/dir"
      a[RuntimeException] should be thrownBy new CairoImageTypesetter(100)
    finally CairoTypesetter.fontsDir = original
  }

  "a multi-page document ships one surface per page, each with content" in {
    val pages = render(100, "first page\n\n\\vfill\\eject second page\n\n\\vfill\\eject third page\n\n")

    pages.length shouldBe 3
    for page <- pages do
      page.getWidth shouldBe 850
      page.getHeight shouldBe 1100
      inkPixels(page) should be > 0
  }

